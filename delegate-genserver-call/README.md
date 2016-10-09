# Del

An experiment into having a single GenServer recieving calls and then
delegating the reply to a worker that is spun up dynamically.

```elixir
{:hello, pid} = Del.hello

# Del (#PID<0.134.0>) casting :hello to Del.Manager.
# Del.Manager (#PID<0.133.0>) got cast :hello from {#PID<0.134.0>, #Reference<0.0.2.1201>}.
# Del.Manager (#PID<0.133.0>) delegating to worker. Not replying.
# Del.WorkerSupervisor starting child with state {#PID<0.134.0>, #Reference<0.0.2.1201>}.
# Del.Worker (#PID<0.157.0>) worker starting with immediate timeout.
# Del.Worker (#PID<0.157.0>) got timeout. Replying to {#PID<0.134.0>, #Reference<0.0.2.1201>}
# Del (#PID<0.134.0>) got response {:hello, #PID<0.157.0>}

{:hello, PID<0.157.0>}
```
