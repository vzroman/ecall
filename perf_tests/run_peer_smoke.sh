#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IMAGE="${PEER_SMOKE_IMAGE:-ecall-peer-smoke:otp27}"
if [[ -n "${PEER_SMOKE_BASE_IMAGE:-}" ]]; then
  BASE_IMAGE="$PEER_SMOKE_BASE_IMAGE"
elif docker image inspect "vzroman/erlang_otp:v27.2.3" >/dev/null 2>&1; then
  BASE_IMAGE="vzroman/erlang_otp:v27.2.3"
else
  BASE_IMAGE="erlang:27.2.2"
fi
DOCKERFILE="$ROOT_DIR/perf_tests/peer_smoke.Dockerfile"

if [[ "${PEER_SMOKE_FORCE_IMAGE_BUILD:-0}" == "1" ]] ||
   ! docker image inspect "$IMAGE" >/dev/null 2>&1; then
  docker build \
    --file "$DOCKERFILE" \
    --build-arg "BASE_IMAGE=$BASE_IMAGE" \
    --tag "$IMAGE" \
    "$ROOT_DIR"
fi

export PEER_SMOKE_IMAGE="$IMAGE"
export PEER_SMOKE_ROOT="$ROOT_DIR"

exec "$ROOT_DIR/rebar3" ct --suite "$ROOT_DIR/perf_tests/peer_smoke_SUITE.erl" "$@"
