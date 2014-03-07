#!/usr/bin/env ruby

# I'm bad at comments
class String
  def count_vowels
    counts = {}
    %w(a e i o u).each do |vowel|
      counts[vowel.to_sym] = count vowel
    end
    counts
  end
end

# If a string is passed, count the vowels
if ARGV.any?
  counts = ARGV.join.count_vowels
  counts.each do |i|
    puts "#{i[0]}\t#{i[1]}"
  end
  puts "Total\t#{counts.values.reduce(:+)}"
else
  # Else display help
  puts 'Please pass some text, like this:'
  puts "\truby count_vowels.rb Hello World"
end
