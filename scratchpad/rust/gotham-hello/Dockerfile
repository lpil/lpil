FROM ekidd/rust-musl-builder:1.34.2 as builder
COPY . .
RUN cargo build --release

FROM alpine:latest as user
RUN addgroup -S happylabs-api-group && adduser -S happylabs-api-user -G happylabs-api-group

# Release image

FROM scratch
LABEL maintainer="Louis Pilfold <louis@lpil.uk>"

ENV PORT 3000
EXPOSE ${PORT}

COPY --from=builder /home/rust/src/target/x86_64-unknown-linux-musl/release/happylabs-api .
COPY --from=user /etc/passwd /etc/passwd
USER happylabs-api-user

ENTRYPOINT ["/happylabs-api"]
