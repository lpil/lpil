#!/usr/bin/env ruby
# encoding: utf-8

require 'curses'

# Patch them monkeys
module Curses
  def self.mvadd(y, x, i)
    setpos y, x # move to position
    addstr i
  end
end

Curses.init_screen
begin
  Curses.curs_set 0
  i = 0
  loop do
    Curses.mvadd i % Curses.stdscr.maxy, i % Curses.stdscr.maxx, i.to_s
    Curses.refresh
    i = i + 1
    sleep 0.01
  end
rescue Interrupt => _
  nil
ensure
  Curses.close_screen
end
