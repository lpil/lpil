#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n").map do |e|
  nums = e.split(' ')
  nums.size == 2 ? (nums[0].to_f / nums[1].to_f).round : nil
end.compact.join ' '
puts x
