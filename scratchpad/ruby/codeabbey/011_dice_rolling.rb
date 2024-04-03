#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split "\n"
x.shift
puts x.map { |e| (e.to_f * 6).floor + 1 }.join ' '
