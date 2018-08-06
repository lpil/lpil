#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split "\n"
x.shift
x.map! do |e|
  e.split(' ').map { |n| n.to_i }.sort[1]
end
puts x.join ' '
