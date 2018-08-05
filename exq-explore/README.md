# Exq (Sidekiq) Exploration

Redis is used as the data store for the queue.

After enqueuing some jobs in the `"default"` queue we get these keys in Redis.

```redis
127.0.0.1:6379> KEYS *
1) "exq:queue:default"
2) "exq:queues"
```

`exq:queues` is a set of all the queues.

```redis
127.0.0.1:6379> TYPE exq:queues
set

127.0.0.1:6379> SMEMBERS exq:queues
1) "default"
```

`exq:queue:$QUEUE_NAME` is a list of jobs in the queue.

```redis
127.0.0.1:6379> TYPE exq:queue:default
list

127.0.0.1:6379> LRANGE exq:queue:default 0 1000000
1) "{\"retry\":25,\"queue\":\"default\",\"jid\":\"1802df7d-6e7c-4b82-9575-11f0c58d1c23\",\"enqueued_at\":1493201269.385918,\"class\":\"Exqy.Worker\",\"args\":[\"world\"]}"
2) "{\"retry\":25,\"queue\":\"default\",\"jid\":\"eff5033e-2cab-4cf1-a104-710754d5dce0\",\"enqueued_at\":1493200993.530107,\"class\":\"Exqy.Worker\",\"args\":[\"hello\"]}"
```

The structure of a job is this:

```js
{
  "retry": 25,
  "queue": "default",
  "jid": "eff5033e-2cab-4cf1-a104-710754d5dce0",
  "enqueued_at": 1493200993.530107,
  "class": "Exqy.Worker",
  "args": ["hello"]
}
```

- `class`: The class/module/thing that the worker runs to process the job.
- `args`: The arguments given to the Job when enqueued.


## When jobs crash

I started workers with an error in them that meant they pulled the two jobs
above and then crashes immediately. This is the state of Redis:

```redis
127.0.0.1:6379> KEYS *
1) "exq:retry"
2) "exq:stat:failed"
3) "exq:queues"
4) "exq:stat:failed:2017-04-26"

127.0.0.1:6379> GET exq:stat:failed
"2"

127.0.0.1:6379> GET exq:stat:failed:2017-04-26
"2"

127.0.0.1:6379> TYPE exq:retry
zset

127.0.0.1:6379> ZRANGE exq:retry 0 10000
1) "{\"retry_count\":1,\"retry\":25,\"queue\":\"default\",\"processor\":null,\"jid\":\"1802df7d-6e7c-4b82-9575-11f0c58d1c23\",\"finished_at\":null,\"failed_at\":1493202617.584599,\"error_message\":\"{{:badmatch, true},\\n [{Exqy.Worker, :register_self, 0, [file: 'lib/exqy.ex', line: 46]},\\n  {Exqy.Worker, :perform, 1, [file: 'lib/exqy.ex', line: 22]},\\n  {Exq.Worker.Server, :\\\"-dispatch_work/3-fun-0-\\\", 4,\\n   [file: 'lib/exq/worker/server.ex', line: 130]}]}\",\"error_class\":null,\"enqueued_at\":1493201269.385918,\"class\":\"Exqy.Worker\",\"args\":[\"world\"]}"
2) "{\"retry_count\":1,\"retry\":25,\"queue\":\"default\",\"processor\":null,\"jid\":\"eff5033e-2cab-4cf1-a104-710754d5dce0\",\"finished_at\":null,\"failed_at\":1493202617.556911,\"error_message\":\"{{:badmatch, true},\\n [{Exqy.Worker, :register_self, 0, [file: 'lib/exqy.ex', line: 46]},\\n  {Exqy.Worker, :perform, 1, [file: 'lib/exqy.ex', line: 22]},\\n  {Exq.Worker.Server, :\\\"-dispatch_work/3-fun-0-\\\", 4,\\n   [file: 'lib/exq/worker/server.ex', line: 130]}]}\",\"error_class\":null,\"enqueued_at\":1493200993.530107,\"class\":\"Exqy.Worker\",\"args\":[\"hello\"]}"
```

`exq:stat:failed` and `exq:stat:failed:$DATE` hold the number of failed jobs,
and the number of failed jobs for that $DATE.

```js
{
  "retry_count": 1,
  "retry": 25,
  "queue": "default",
  "processor": null,
  "jid": "eff5033e-2cab-4cf1-a104-710754d5dce0",
  "finished_at": null,
  "failed_at": 1493202617.556911,
  "error_message": "$ERROR_STACK_TRACE_STRING_GOES_HERE",
  "error_class": null,
  "enqueued_at": 1493200993.530107,
  "class": "Exqy.Worker",
  "args": ["hello"]
}
```
