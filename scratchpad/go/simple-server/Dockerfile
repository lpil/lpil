# Build
FROM golang:alpine AS builder
RUN apk update && apk add --no-cache git
RUN adduser -D -g '' appuser
WORKDIR /tmp/simple-server
COPY . .
RUN go mod download
RUN GOOS=linux GOARCH=amd64 go build -ldflags="-w -s" -o /simple-server

# Release
FROM scratch
COPY --from=builder /etc/passwd /etc/passwd
COPY --from=builder /simple-server /simple-server
USER appuser
ENTRYPOINT ["/simple-server"]
