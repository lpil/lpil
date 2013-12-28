#!/usr/bin/ruby

# If a string is passed, reverse it and return it
if ARGV.any?
  puts ARGV.first.reverse
  exit
end

# Else, start the interactive mode

puts "Hello! :)\nEnter some text, and I'll reverse it! You can type '/exit' to exit.\n"

loop do
  usr_str = gets.chomp
  exit if usr_str == '/exit'
  puts "\nHere you go:"
  puts "\t'#{usr_str.reverse}'"
  puts "Give me another, or type '/exit' to exit."
end
