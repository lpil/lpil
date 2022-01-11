IO.puts "What is the input string?"

stream = IO.stream(:stdio, :line)

for line <- stream, into: stream do
  word = String.trim(line)
  count = String.length(word)
  """
  "#{word}" has #{count} characters.

  What is the input string?
  """
end
