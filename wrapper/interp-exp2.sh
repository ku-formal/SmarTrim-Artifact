docker run --rm --init \
  --volume "$(pwd)":/workspace \
  --workdir /workspace \
  --entrypoint python3 \
  artifact-setter:fse26 scripts/analyze_ablation.py