#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n")[1].split(' ')
x.map!.with_index { |e, i| [i + 1, e.to_i] }
x.sort! { |e, r| e[1] <=> r[1] }
x.map! { |e| e[0] }
puts x.join ' '
