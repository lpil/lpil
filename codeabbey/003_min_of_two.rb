#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n").map do |line|
  nums = line.split(' ')
  if nums.size == 2
    nums.map { |e| e.to_i }.min
  else
    nil
  end
end.compact.join ' '
puts x
