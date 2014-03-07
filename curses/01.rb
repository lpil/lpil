#!/usr/bin/env ruby
# encoding: utf-8

require 'curses'

Curses.init_screen
begin
  Curses.crmode # Let interupts, etc, pass through
  Curses.noecho # Don't show typed keys
  Curses.stdscr.keypad true # Enable extra keys
  loop do
    input = Curses.getch                    # Get user input
    Curses.addstr "'#{input}' was pressed.\n" # Add it
    Curses.refresh            # Refresh the screen to see it
  end
rescue Interrupt => _
  nil
ensure
  Curses.close_screen
end
