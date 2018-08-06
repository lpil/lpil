#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n")[1].split(' ').map { |e| e.to_f }
y = []
x.each.with_index do |e, i|
  if i == 0 || i == x.size - 1
    y << e
  else
    y << (x[i - 1] + e + x[i + 1]) / 3
  end
end
puts y.join ' '
