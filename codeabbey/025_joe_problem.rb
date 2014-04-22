#!/usr/bin/env ruby
# encoding: utf-8

args = ARGF.file.read.split(' ').map { |e| e.to_i }
grp = (1..args[0]).to_a
until grp.size == 1
  grp.rotate! args[1] - 1
  grp.delete_at 0
end
puts grp[0]
