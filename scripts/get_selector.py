#!/usr/bin/env python3
"""
Extract 4-byte function selectors from a single Solidity source file.

Usage:
  python extract_selectors.py 0.8.0 MyContract.sol
"""
from __future__ import annotations
import argparse
import json
import os
import pandas as pd
import re
import subprocess
from dataclasses import dataclass
from pathlib import Path
from Crypto.Hash import keccak
from utils import meta, solc_select_path, get_solv_from_str, BENCH_REPO

# ----------------------------- Keccak-256 helpers -----------------------------

def keccak256(data: bytes) -> bytes:
    k = keccak.new(digest_bits=256)
    k.update(data)
    return k.digest()

# ----------------------------- ABI processing -----------------------------

def _split_base_and_arrays(sol_type: str) -> tuple[str, str]:
    m = re.match(r"^([a-zA-Z0-9_]+)(.*)$", sol_type)
    if not m:
        return sol_type, ""
    return m.group(1), m.group(2)

def _canonical_type_from_abi_param(p: dict) -> str:
    typ = p.get("type", "")
    base, suffix = _split_base_and_arrays(typ)
    if base == "tuple":
        comps = p.get("components", []) or []
        inner = ",".join(_canonical_type_from_abi_param(c) for c in comps)
        return f"({inner}){suffix}"
    return f"{base}{suffix}"

@dataclass
class Selector:
    contract: str
    name: str
    signature: str
    selector_hex: str

# ----------------------------- solc path & compile -----------------------------

def compile_to_abi(file: Path, solc_path: str) -> dict[str, list[dict]]:
    cmd = [solc_path, "--combined-json", "abi", str(file)]
    try:
        out = subprocess.run(cmd, capture_output=True, check=True)
    except subprocess.CalledProcessError as e:
        raise RuntimeError(
            f"solc failed (exit {e.returncode}). Output:\n{e.stderr}"
        )
    data = json.loads(out.stdout.decode())
    contracts = data.get("contracts", {})
    result: dict[str, list[dict]] = {}
    for name, payload in contracts.items():
        abi_json = payload.get("abi")
        if not abi_json:
            continue
        try:
            abi = json.loads(abi_json)
        except Exception:
            abi = abi_json
        result[name] = abi
    return result

# ----------------------------- Selector extraction -----------------------------

def selectors_from_abi(abi: list[dict], qualified_contract: str) -> list[Selector]:
    sels: list[Selector] = []
    contract = qualified_contract.split(":")[-1]
    for item in abi:
        if item.get("type") != "function":
            continue
        fname = item["name"]
        inputs = item.get("inputs", [])
        types = [_canonical_type_from_abi_param(p) for p in inputs]
        sig = f"{fname}({','.join(types)})"
        selector = keccak256(sig.encode())[:4].hex()
        sels.append(Selector(contract, fname, sig, selector))
    sels.sort(key=lambda s: (s.contract.lower(), s.name.lower(), s.signature))
    return sels

# ----------------------------- Main -----------------------------

def main_(version: str, file: Path):
    solc_path = solc_select_path(version)
    contract_abis = compile_to_abi(file, solc_path)

    all_selectors: list[Selector] = []
    for qname, abi in contract_abis.items():
        all_selectors.extend(selectors_from_abi(abi, qname))

    dedup = {(s.contract, s.signature): s for s in all_selectors}
    out = list(dedup.values())
    out.sort(key=lambda s: (s.contract, s.name, s.signature))

    return out

def find_func(version: str, file: str, selector: str):
    if selector.startswith('0x'):
        selector = selector[2:]
    if version == '0.5.0':
        version = '0.5.1'
    out = main_(version, Path(file))
    out = [s.name for s in out if s.selector_hex == selector]
    if len(out) == 0:
        raise ValueError(f'No corresponding function: {selector}')
    out = set(out)
    if len(out) != 1:
        raise ValueError(f'Duplicated corresponding function: {selector}')
    return out.pop()

def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Extract function selectors from a single Solidity source file")
    ap.add_argument("version", help="Solidity compiler version, e.g., 0.8.0")
    ap.add_argument("path", type=str, help=".sol file")
    args = ap.parse_args(argv)

    file = Path(args.path)
    if not file.exists():
        raise FileNotFoundError(args.path)

    out = main_(args.version, file)
    print("Contract,Function,Signature,Selector")
    for s in out:
        print(f"{s.contract},{s.name},{s.signature},{s.selector_hex}")
        
    return 0

def main2():
    dataset = 're'
    df_meta = meta(dataset)
    df = pd.DataFrame({
        'id': [],
        'contract': [],
        'signature': [],
        'function': [], 
        'selector': [],
    })
    for i in df_meta.index:
        id = df_meta.loc[i]['id']
        version = df_meta.loc[i]['compiler_version']
        version = get_solv_from_str(version, minimum_required='0.4.11')
        path = os.path.join(BENCH_REPO, 'contracts', dataset, f'{id}.sol')
        ret = main_(version, Path(path))
        for s in ret:
            df.loc[len(df)] = {'id': id, 'contract': s.contract, 'signature': s.signature, 
                               'function': s.name, 'selector': s.selector_hex}
    df.to_csv(os.path.join(BENCH_REPO, 'misc', 'selectors', f'{dataset}.csv'), index=False)
        
if __name__ == "__main__":
    # try:
    #     raise SystemExit(main())
    # except KeyboardInterrupt:
    #     raise SystemExit(130)
    main2()