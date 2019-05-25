FROM ekidd/rust-musl-builder:1.34.2 as build

# Build
COPY . .
RUN cargo build --release

# Create user
FROM alpine:latest as alpine
RUN addgroup -S appgroup && adduser -S appuser -G appgroup

# Release
# FROM alpine:latest
FROM scratch
COPY --from=build /home/rust/src/target/x86_64-unknown-linux-musl/release/berry .
COPY --from=alpine /etc/passwd /etc/passwd
USER appuser
ENTRYPOINT ["./berry"]
