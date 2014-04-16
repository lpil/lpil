#!/usr/bin/env ruby
# encoding: utf-8

nums = ARGF.file.read.split(' ')
nums.shift
nums.map! do |e|
  ((e.to_f - 32) * (5.0 / 9.0)).round
end
puts nums.join ' '
