#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split(' ')[0..-2].map { |e| e.to_i }
swaps = 0
x.size.times do |i|
  next if i == x.size - 1
  if x[i] > x[i + 1]
    x[i], x[i + 1] = x[i + 1], x[i]
    swaps += 1
  end
end
checksum = x.reduce(0) { |a, e| ((a + e) * 113) % 10_000_007 }
puts "#{swaps} #{checksum}"
