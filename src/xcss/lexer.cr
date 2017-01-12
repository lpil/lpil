# Adapted from this ->
# https://github.com/manastech/crystal-toml/blob/master/src/toml/lexer.cr
#
# More mutate-y than need be. Oops.
#

class Xcss::Lexer
  def initialize(string)
    @reader = Char::Reader.new(string)
    @line = 1
    @column = 1
  end

  def next_token!
    case current_char
    when '0'..'9'
      consume_number!
    when 'a'..'z', '_'
      consume_atom!
    when '-'
      consume_minus_token!
    when ' ', '\t', '\n'
      consume_whitespace!
    when '{'
      consume_punctuation!(:"{")
    when '}'
      consume_punctuation!(:"}")
    when '.'
      consume_punctuation!(:".")
    when ','
      consume_punctuation!(:",")
    when ';'
      consume_punctuation!(:";")
    when '+'
      consume_punctuation!(:"+")
    when '/'
      consume_punctuation!(:"/")
    when '*'
      consume_punctuation!(:"*")
    when '#'
      consume_punctuation!(:"#")
    when '\0'
      Token.new(:EOF, line: @line, column: @column)
    else
      raise Exception.new("Xcss::Lexer: Unexpected #{current_char.inspect}")
    end
  end

  private def current_char
    @reader.current_char
  end

  private def next_char!(advance_column = true)
    @column += 1 if advance_column
    @reader.next_char
  end

  private def consume_minus_token!
    case @reader.peek_next_char
    when '-'
      consume_atom!
    when '0'..'9'
      consume_number!
    else
      consume_punctuation!(:"-")
    end
  end

  private def consume_punctuation!(type)
    t = Token.new(type, line: @line, column: @column)
    next_char!
    t
  end

  private def consume_whitespace!
    io = IO::Memory.new
    line = @line
    column = @column
    while true
      case current_char
      when ' ', '\t'
        io << current_char
        next_char!
      when '\n'
        io << current_char
        next_char!(advance_column: false)
        @line += 1
      when '\n'
      else
        break
      end
    end
    Token.new(:ws, io.to_s, line: line, column: column)
  end


  private def consume_atom!
    io = IO::Memory.new
    line = @line
    column = @column
    while true
      case current_char
      when 'a'..'z', '-', '_'
        io << current_char
        next_char!
      else
        break
      end
    end
    Token.new(:atom, io.to_s, line: line, column: column)
  end

  private def consume_number!
    io = IO::Memory.new
    line = @line
    column = @column
    # Optional preceeding -
    if current_char == '-'
      io << current_char
      next_char!
    end
    # Digits
    chomp_digits!(io)
    # Optional decimal
    if current_char == '.'
      io << current_char
      next_char!
      chomp_digits!(io)
    end
    Token.new(:number, io.to_s, line: line, column: column)
  end

  private def chomp_digits!(io)
    while true
      case current_char
      when '0'..'9'
        io << current_char
        next_char!
      else
        break
      end
    end
  end
end
