# class Counter extends Actor {
#   def counter(n: Int): Receive = {
#     case "incr" => context.become(counter(n + 1))
#     case "get"  => sender ! n
#   }
#   def receive = counter(0)
# }
#
# class Main extends Actor {
#   val counter = context.actorOf(Props[Counter], "counter")
#   
#   counter ! "incr"
#   counter ! "incr"
#   counter ! "incr"
#   counter ! "get"
#   
#   def receive = {
#     case count: Int =>
#       println(s"count was $count")
#       context.stop(self)
#   }
# }

defmodule Counter do
  def loop(count) do
    receive do
      :incr ->
        loop(count + 1)

      {:get, caller} ->
        send(caller, count)
    end
  end
end

counter = spawn fn -> Counter.loop(0) end

send counter, :incr
send counter, :incr
send counter, :incr
send counter, {:get, self}

receive do
  count -> IO.puts "count was #{count}"
end
