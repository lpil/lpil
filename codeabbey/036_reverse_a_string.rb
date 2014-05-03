#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read
y = ''
(x.size - 1).downto 0 do |e|
  y << x[e]
end
puts y
