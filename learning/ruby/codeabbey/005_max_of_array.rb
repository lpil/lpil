#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split(' ').map { |e| e.to_i }
puts "#{x.max} #{x.min}"
