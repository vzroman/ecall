ARG DOCKER_CLI_IMAGE=docker:cli
ARG PERFORMANCE_IMAGE=ecall-performance:otp27

FROM ${DOCKER_CLI_IMAGE} AS docker_cli

FROM ${PERFORMANCE_IMAGE}

RUN apt-get update \
  && apt-get install -y --no-install-recommends sshpass \
  && rm -rf /var/lib/apt/lists/*

COPY --from=docker_cli /usr/local/bin/docker /usr/local/bin/docker

ENV ECALL_PERFORMANCE_PREBUILT_IMAGE=true

ENTRYPOINT []
CMD ["make", "performance_tests"]
