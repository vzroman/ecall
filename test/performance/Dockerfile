ARG BASE_IMAGE=ecall-performance-env:otp27

FROM ${BASE_IMAGE}

WORKDIR /opt/ecall
COPY . /opt/ecall

ENTRYPOINT ["/usr/local/bin/erl"]
