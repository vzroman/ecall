ARG BASE_IMAGE=erlang:27.2.2
FROM ${BASE_IMAGE}

ENTRYPOINT ["/usr/local/bin/erl"]
