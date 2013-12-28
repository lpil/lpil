# Hi Mum!
#   - Louis

class Revolver
    # This allows you to access these instance variables externally.
    attr_accessor :chambers

    def initialize (chambers = 6)
        @chambers = Array.new(chambers)
    end
    
    def spin!
        # Spin the cylinder to a random position
        @chambers.rotate!( rand(@chambers.length) )
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
        return @chambers.none?
    end
    def any_bullets_left?
        return @chambers.any?
    end
end


class RouletteGame
    def initialize(bullets = 1, gun = nil)
        @round_num = -1

        if gun
            @gun = gun
        else
            @gun = Revolver.new
        end
        num_chambers = @gun.chambers.size

        if num_chambers <= bullets
            @gun.chambers = Array.new(num_chambers, :loaded)
            return true
        end
        while bullets > 0 do
            # If the gun loads successfully decrement bullets
            bullets = bullets - 1 if @gun.load!
            @gun.spin!
        end
    end

    def play_round! (spin_after_shot = false)
        # Returns the round number if the gun went off, else false
        @round_num = @round_num + 1
        did_fire = @gun.fire!
        @gun.spin! if spin_after_shot
        return @round_num if did_fire
        false
    end

    def play_game!
        # Returns and array of the rounds on which the gun went off
        fatal_rounds = Array.new
        while @gun.any_bullets_left?
            round_result = play_round!
            fatal_rounds.push(round_result) if round_result
        end
        fatal_rounds
    end
end
