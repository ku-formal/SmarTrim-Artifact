docker run --rm --init \
  --user "$(id -u):$(id -g)" \
  --volume "$(pwd)":/workspace \
  --workdir /workspace \
  --entrypoint python3 \
  artifact-setter:fse26 scripts/analyze_tool.py