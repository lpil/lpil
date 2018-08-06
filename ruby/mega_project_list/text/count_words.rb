#!/usr/bin/env ruby
# encoding: utf-8

puts File.read('words.txt').gsub("\n", ' ').gsub(/[^A-z ]/, '').split(' ').size
