use Mix.Config

config :exq,
  # Workers listen to no queues by default, for testing exploring
  # queues: [],

  # Process jobs sequentially.
  concurrency: 1

  # name: Exq,
  # host: "127.0.0.1",
  # port: 6379,
  # password: "optional_redis_auth",
  # namespace: "exq",
  # concurrency: :infinite,
  # queues: ["default"],
  # poll_timeout: 50,
  # scheduler_poll_timeout: 200,
  # scheduler_enable: true,
  # max_retries: 25,
  # shutdown_timeout: 5000
