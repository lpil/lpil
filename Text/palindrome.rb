#!/usr/bin/env ruby
# encoding: utf-8

puts 'Give me words. q to exit.'
loop do
  x = gets.chomp.downcase
  break if x == 'q'
  puts x + (x == x.reverse ? ' is a palindrome' : " isn't a palindrome")
end
puts 'Bye!'
