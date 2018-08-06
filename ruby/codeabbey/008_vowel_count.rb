#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split("\n")
x.shift
puts x.map { |e| e.scan(/[aeiouy]/).size }.join ' '
