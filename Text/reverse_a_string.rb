#!/usr/bin/ruby

# The new reverse
class String
  def my_reverse
    reved = []
    (self.length - 1).downto(0).to_a.each do |i|
      reved<<self[i]
    end
    reved.join
  end
end

# If a string is passed, reverse it and return it
if ARGV.any?
  puts ARGV.first.my_reverse
  exit
end

# Else, start the interactive mode
puts "Hello! :)\nEnter some text, and I'll reverse it! You can type '/exit' to exit.\n"
loop do
  usr_str = gets.chomp
  exit if usr_str == '/exit'
  puts "\nHere you go:"
  puts "\t'#{usr_str.my_reverse}'"
  puts "Give me another, or type '/exit' to exit."
end
