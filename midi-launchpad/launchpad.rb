#!/usr/bin/env ruby

require 'unimidi'

output = UniMIDI::Output.first
input  = UniMIDI::Input.first

loop do
  m = input.gets.first
  p m[:data]
  output.puts m[:data]
end
