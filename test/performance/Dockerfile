ARG BASE_IMAGE=erlang:27.2.2
ARG OTP_VERSION=27.2.2

FROM ${BASE_IMAGE} AS lcnt_builder
ARG OTP_VERSION

RUN mkdir /tmp/otp \
  && curl -fsSL \
    "https://github.com/erlang/otp/releases/download/OTP-${OTP_VERSION}/otp_src_${OTP_VERSION}.tar.gz" \
    -o /tmp/otp.tar.gz \
  && tar -xzf /tmp/otp.tar.gz -C /tmp/otp --strip-components=1 \
  && cd /tmp/otp \
  && ./configure \
  && ERL_TOP=/tmp/otp make -C erts/emulator lcnt \
  && cp bin/*/beam.lcnt.smp /tmp/beam.lcnt.smp

FROM ${BASE_IMAGE}

COPY --from=lcnt_builder /tmp/beam.lcnt.smp /tmp/beam.lcnt.smp
RUN cp /tmp/beam.lcnt.smp \
    /usr/local/lib/erlang/erts-*/bin/ \
  && rm /tmp/beam.lcnt.smp

WORKDIR /opt/ecall
COPY . /opt/ecall

ENTRYPOINT ["/usr/local/bin/erl"]
