IO.puts "I'M GOING TO SHOUT EVERYTHING BACK AT YOU.
"

stream = IO.stream(:stdio, :line)
for line <- stream, into: stream do
  String.upcase(line) <> "\n"
end
