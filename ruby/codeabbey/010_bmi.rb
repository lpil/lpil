#!/usr/bin/env ruby
# encoding: utf-8

x = ARGF.file.read.split "\n"
x.shift
x.map! do |e|
  e = e.split(' ')
  bmi = e[0].to_f / (e[1].to_f**2)
  if bmi < 18.5
    'under'
  elsif bmi < 25
    'normal'
  elsif bmi < 30
    'over'
  else
    'obese'
  end
end
puts x.join ' '
