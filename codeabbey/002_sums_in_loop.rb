#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n").map do |line|
  nums = line.split(' ')
  if nums.size == 2
    nums[0].to_i + nums[1].to_i
  else
    nil
  end
end.compact.join ' '
puts x
