#!/usr/bin/env ruby
# encoding: utf-8

puts ARGF.file.read.split("\n")[1].split(' ').reduce(0) { |a, e| a + e.to_f }
