#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n")[1..-1].map { |e| e.split ' ' }
x.map! do |e|
  e.map! { |r| r.to_i }
  e[0], e[1], e[2] = e[0] * 100, e[1] * 100, (1 + e[2] / 100.0)
  years = 0
  until e[0] > e[1]
    years += 1
    e[0] = (e[0] * e[2]).floor
  end
  years
end
puts x.join ' '
