#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n")[1].split(' ').map do |e|
  a = 0
  e.scan(/./).each.with_index do |r, i|
    a += r.to_i * (i + 1)
  end
  a
end.join ' '
puts x
