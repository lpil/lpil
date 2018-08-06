#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n")[1..-1].map { |e| e.to_i }
x.map! do |e|
  if e == 0
    0
  elsif e == 1
    1
  else
    step, num, prev_num = 0, 0, 1
    until num == e
      step += 1
      num, prev_num = prev_num + num, num
    end
    step
  end
end
puts x.join ' '
