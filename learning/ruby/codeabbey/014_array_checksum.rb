#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n")[1].split(' ').map do |e|
  e.to_i
end.reduce(0) do |a, e|
  ((a + e) * 113) % 10000007
end
puts x
