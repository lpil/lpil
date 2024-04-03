#!/usr/bin/env ruby
# encoding: utf-8

x = []
ARGF.file.read.split("\n")[1].split(' ').each do |e|
  e = e.to_i - 1
  x[e] = x[e] ? x[e] + 1 : 1
end
puts x.join ' '
