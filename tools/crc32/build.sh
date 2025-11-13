#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
OUT="${ROOT_DIR}/tools/crc32/crc32"

g++ -I"${ROOT_DIR}/amxmodx" \
	"${ROOT_DIR}/amxmodx/sm_crc32.cpp" \
	"${ROOT_DIR}/tools/crc32/main.cpp" \
	-o "${OUT}"

echo "Built ${OUT}"
