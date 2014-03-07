#!/usr/bin/env ruby

# Gun
class Revolver
  # This allows you to access these instance variables externally.
  attr_accessor :chambers

  def initialize(chambers = 6)
    @chambers = Array.new(chambers)
  end

  def spin!
    # Spin the cylinder to a random position
    @chambers.rotate!(rand(@chambers.length))
  end

  def load!
    # Load a round into the first chamber
    # Returns False if unsuccessful due to the chamber being full
    return false if @chambers[0]
    @chambers[0] = :loaded
  end

  def fire!
    # Returns true if it fires, false if not
    chamber = @chambers[0]
    @chambers[0] = nil
    @chambers.rotate!
    return true if chamber
    false
  end

  def empty?
    @chambers.none?
  end

  def any_bullets_left?
    @chambers.any?
  end
end

# Game
class RouletteGame
  def initialize(bullets = 1, gun = nil)
    gun ? @gun = gun : @gun = Revolver.new
    num_chambers = @gun.chambers.size

    if num_chambers <= bullets
      @gun.chambers = Array.new(num_chambers, :loaded)
      return true
    end
    while bullets > 0
      # If the gun loads successfully decrement bullets
      bullets = bullets - 1 if @gun.load!
      @gun.spin!
    end
  end

  def play_round!(spin_after_shot = false)
    # Returns the round number if the gun went off, else false
    @round_num = @round_num.to_i + 1
    did_fire = @gun.fire!
    @gun.spin! if spin_after_shot
    return @round_num if did_fire
    false
  end

  def play_game!
    # Returns and array of the rounds on which the gun went off
    fatal_rounds = []
    while @gun.any_bullets_left?
      round_result = play_round!
      fatal_rounds.push(round_result) if round_result
    end
    fatal_rounds
  end
end
