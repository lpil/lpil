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
    discard_whitespace!
    discard_newlines!

    case current_char
    when '0'..'9'
      consume_number!
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
    when '-'
      consume_punctuation!(:"-")
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

  private def next_char!
    @column += 1
    @reader.next_char
  end

  private def discard_whitespace!
    while current_char == ' ' || current_char == '\t'
      next_char!
    end
  end

  private def discard_newlines!
    while current_char == '\n'
      @line += 1
      @column = 1
      next_char!
    end
  end

  private def consume_punctuation!(type)
    t = Token.new(type, line: @line, column: @column)
    next_char!
    t
  end

  private def consume_number!
    io = IO::Memory.new
    line = @line
    column = @column
    chomp_digits!(io)
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
