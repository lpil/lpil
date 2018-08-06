#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n")[1..-1].map { |e| e.split ' ' }
x.map! do |e|
  e.map! { |r| r.to_i }
  start, step_size, steps = e[0], e[1], e[2]
  final = start + step_size * (steps - 1)
  (steps * (start + final)) / 2
end
puts x.join ' '
