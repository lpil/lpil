# BEAM

## Turn off busy waiting

https://erlangforums.com/t/vm-tuning-guide/1945/4

> I quite often get asked this question and my answer will remain the same. The default settings are good for most applications. There is nothing that you can tune that would make a general application perform better, it all depends on what that application is actually doing.
>
> There is one exception to this rule, and that is the scheduler busy wait time aka +sbtw. For any OS that is using CFS 3 (Completely Fair Scheduler) and when you set CPU limits on Erlang, you want to set all schedulers busy wait to none. That is `+sbwt none +sbwtdcpu none +sbwtdio none`.
>
> An example of such a system is when you run: `docker run -it --cpus 1 erlang`

https://www.erlang.org/doc/man/erl.html#emulator-flags
