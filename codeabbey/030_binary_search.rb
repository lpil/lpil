#!/usr/bin/env ruby
# encoding: utf-8

z = ARGF.file.read.split("\n")[1..-1].map do |e|
  e.split(' ').map { |r|r.to_f }
end
margin = 0.0000001
z.map! do |e|
  ans = 1 # Number bigger than margin
  a, b, c, d, top, bottom = e[0], e[1], e[2], e[3], 100, 0
  until ans.abs < margin
    x = (top + bottom) / 2.0
    ans = (a * x) + (b * Math.sqrt(x**3)) - (c * Math.exp(-x / 50)) - d
    ans > 0 ? top = x : bottom = x
  end
  x
end
puts z.join ' '
