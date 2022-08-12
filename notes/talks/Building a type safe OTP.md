# Building a type safe OTP

- [[#Can OTP be typed]]?
	- The belief that OTP cannot be typed
	- The creation of Gleam
	- The suspicion that OTP can be typed
- [[#Gleam crash course]]
	- It's a statically typed language that runs on the Erlang VM
	- Can define Erlang records (called custom types)
	- Can call Erlang code without cost (like Elixir can)
	- Functions can be imported (external fn)
	- Data types can be imported (external type)
	- No send/receive etc in the core language
- [[#Wrapping gen_servers]]
	- It's the building block of OTP, most commonly used to build other abstractions
	- Use external fn to import a gen_server's functional facade
	- Use it via that imported facade
- [[#Defining gen_servers]]
	- Define each callback function
	- Define functional interface
	- Is this good enough?
	- It doesn't catch mistakes
		- Function names
		- Incorrect return values
		- Missing functions
	- We want full type safety. Mistakes should be impossible.
- Wrapping OTP is not enough
	- If we wrap gen_server we can't do everything
	- Elixir's task, gen_statem, etc
	- We want type safe versions of the primitives
		- send
		- receive
		- spawn
		- link
		- monitor
		- trap_exits
- Type safe primitives
	- define Pid type (no parameter)
	- link (easy, just a fn)
	- monitor (Monitor type + fn)
	- trap_exits (Erlang wrapper + fn)
	- send
		- How do we know what type of message a pid accepts?
		- Parameterise pids with the message type
	- spawn (fn)
	- receive
		- gen_server style eager receive fn
		- need to have a reference to self in order to know own message type
- Show spawning a process and sending a message to it
- Let's implement `call`
	- Sending a message and getting a response.
	- Show the code
	- What if the response isn't the first message in the inbox?
	- How does OTP do it?
		- send `{call, {Pid, Ref}, Msg}`, reply with `{Ref, Reply}`
		- the reference is used to add some semantic information to messages being received
		- We need selective receive!
	- Selecting for one specific tag tuple
	- `receive` can select from multiple expected messages at once
		- Selecting multiple tags at once with Selector
	- implement call with it
	- Wait! There's a problem!
	- We can't reply because the caller pid might not accept that message type
	- Having a pid accept only one type of message is not enough
	- We need to add semantic information to messages being sent
	- Use subjects again!



## Can OTP be typed?

> "Erlang doesn't have types because you can't type OTP"

## Gleam crash course

## Wrapping gen_servers


## Defining gen_servers
