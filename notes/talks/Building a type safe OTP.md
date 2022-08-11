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
	- Use it
	- Is this good enough?
	- It doesn't catch mistakes
		- Function names
		- Incorrect return values
		- Missing functions
	- We want full type safety. Mistakes should be impossible.
- We need to type message sending
- Parameterised pids
	- Oh no, how do we reply to messages?
- Parameterised subjects
	- hey the "from" thing in handle_call isn't just a pid


## Can OTP be typed?

> "Erlang doesn't have types because you can't type OTP"

## Gleam crash course

## Wrapping gen_servers


## Defining gen_servers
