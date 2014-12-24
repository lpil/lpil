#!/usr/bin/env ruby

require 'unimidi'

class Launchpad
  attr_accessor :input, :output

  def initialize
    @output = UniMIDI::Output.find { |e| e.name.match(/Launchpad/) }.open
    @input  = UniMIDI::Input.find  { |e| e.name.match(/Launchpad/) }.open
  end

  def light(x, y)
    note = x + y * 16
    @output.puts 144, note, 1
  end
end

lp = Launchpad.new

loop do
  m = lp.input.gets.first
  p m[:data]
end
