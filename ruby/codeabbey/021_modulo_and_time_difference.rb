#!/usr/bin/env ruby
# encoding: utf-8

def to_seconds(days, hours, minutes, seconds)
  days.to_i    * 60 * 60 * 24 +
  hours.to_i   * 60 * 60 +
  minutes.to_i * 60 +
  seconds.to_i
end

def from_seconds(seconds)
  days    = seconds / 86_400
  seconds = seconds % 86_400
  hours   = seconds / 3600
  seconds = seconds % 3600
  minutes = seconds / 60
  seconds = seconds % 60
  "(#{days} #{hours} #{minutes} #{seconds})"
end

x = ARGF.file.read.split("\n").map { |e| e.split ' ' }
x.shift
x.map! do |e|
  from_seconds(to_seconds(*e[4..7]) - to_seconds(*e[0..3]))
end
puts x.join ' '
