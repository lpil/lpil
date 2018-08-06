#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n")[1].split(' ').map { |e| e.to_i }
x.map! do |e|
  loops = 0
  chain = []
  until chain.include? e
    chain << e
    loops += 1
    e **= 2
    # to_str, add leading zeros to get 8 chars if needed
    e = format('%08d', e)
    e = e[2..5].to_i
  end
  loops
end
puts x.join ' '
