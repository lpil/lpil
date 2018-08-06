#!/usr/bin/env ruby
# encoding: utf-8

def g_c_d(a, b)
  x = [a, b].min
  x -= 1 until a % x == 0 && b % x == 0
  x
end

def l_c_m(a, b)
  x = [a, b].max
  x += 1 until x % a == 0 && x % b == 0
  x
end

x = ARGF.file.read.split("\n").map { |e| e.split ' ' }
x.shift
x.map! do |e|
  e.map! { |r| r.to_i }
  "(#{g_c_d(e[0], e[1])} #{l_c_m(e[0], e[1])})"
end
puts x.join ' '
