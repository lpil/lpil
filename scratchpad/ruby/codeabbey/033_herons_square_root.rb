#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n")[1..-1].map { |e| e.split }
x.map! do |e|
  e.map! { |r| r.to_i }
  root = 1
  e[1].times do
    division_result = e[0].to_f / root
    root = (root + division_result) / 2
  end
  root
end
puts x.join ' '
