#!/bin/sh

set -eu

ARTIFACT_DIR=${ECALL_PERFORMANCE_ARTIFACT_DIR:-/home/romanvozfp/ecall_tests}
IMAGE_ARCHIVE="$ARTIFACT_DIR/images/ecall-performance-images.tar.gz"
CONFIG_FILE="$ARTIFACT_DIR/performance.config"
SPEC_FILE="$ARTIFACT_DIR/test.spec"
LOG_DIR="$ARTIFACT_DIR/logs"
LOG_OWNER=$(id -u):$(id -g)

restore_log_ownership() {
  docker run --rm \
    --volume "$LOG_DIR:/logs" \
    --entrypoint chown \
    ecall-performance-controller:otp27 \
    --recursive "$LOG_OWNER" /logs
}

mkdir -p "$LOG_DIR"
docker load --input "$IMAGE_ARCHIVE"
trap restore_log_ownership EXIT

docker run --rm \
  --network host \
  --volume /var/run/docker.sock:/var/run/docker.sock \
  --volume "$CONFIG_FILE:/opt/ecall/test/performance/performance.config:ro" \
  --volume "$SPEC_FILE:/opt/ecall/test/performance/test.spec:ro" \
  --volume "$LOG_DIR:/opt/ecall/_build/test/logs" \
  ecall-performance-controller:otp27
