#!/usr/bin/ruby

class String
  def count_vowels
    counts = {}
    vowels = ['a','e','i','o','u']
    self.each_char do |c|
      dc = c.downcase
      is_vowel = vowels.any? { |v| dc == v }
      counts[dc.to_sym] = counts[dc.to_sym].to_i + 1 if is_vowel
    end
    counts
  end
end

# If a string is passed, count the vowels
if ARGV.any?
  counts = ARGV.first.count_vowels
  total = 0
  counts.each do |i|
    puts i[0].to_s + "\t" + i[1].to_s
    total += i[1]
  end
  puts "Total\t" + total.to_s
else
  # Else display help
  puts "Please pass some text, like this:\n\truby count_vowels.rb 'Hello World'"
end
