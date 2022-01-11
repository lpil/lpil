#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split "\n"
start = x.shift.to_i
x = x.reduce(start) do |a, e|
  e = e.split ' '
  a.public_send e[0].to_sym, e[1].to_i
end
puts x
