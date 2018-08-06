#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n")[1].split(' ').map { |e| e.to_i }
x.map! do |e|
  chain = [e]
  until chain.include? 1
    e = e.even? ? e / 2 : e * 3 + 1
    chain << e
  end
  chain.size - 1
end
puts x.join ' '
