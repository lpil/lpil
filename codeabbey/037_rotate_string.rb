#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n")[1..-1].map { |e| e.split ' ' }
x.map! do |e|
  e[1].split('').rotate(e[0].to_i).join ''
end
puts x.join ' '
