#!/usr/bin/env ruby
# encoding: utf-8

def sum_digits(num)
  def rec_sum(accumulator, num)
    return accumulator + num if num < 10
    accumulator += num % 10
    rec_sum accumulator, num / 10
  end
  rec_sum 0, num
end

x = ARGF.file.read.split "\n"
x.shift
x.map! do |e|
  e = e.split ' '
  e[0].to_i * e[1].to_i + e[2].to_i
end
puts x.map { |e| sum_digits(e) }.join ' '
