#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split "\n"
x.shift
x.map! do |r|
  r = r.split(' ')[0..-2]
  (r.reduce(0) { |a, e| a + e.to_f } / r.size).round
end
puts x.join ' '
