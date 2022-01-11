#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n")[1..-1].map { |e| e.split ' ' }
x.map! do |e|
  e.map! { |r| r.to_i }
  pages0 = ((e[1] * e[2]).to_f / (e[0] + e[1])).ceil
  pages1 = ((e[0] * e[2]).to_f / (e[0] + e[1])).ceil
  [pages0 * e[0], pages1 * e[1]].min
end
puts x.join ' '
