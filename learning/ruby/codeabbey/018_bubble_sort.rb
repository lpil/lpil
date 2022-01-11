#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n")[1].split(' ').map { |e| e.to_i }
passes, swaps = 0, 0
sorted = false
until sorted
  clean_pass = true
  (0..x.size - 2).each do |e|
    if x[e] > x[e + 1]
      swaps += 1
      clean_pass = false
      x[e], x[e + 1] = x[e + 1], x[e]
    end
  end
  passes += 1
  sorted = true if clean_pass
end
puts "#{passes} #{swaps}"
