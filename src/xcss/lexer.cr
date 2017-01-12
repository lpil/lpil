require "./token"

#
# Tokenizes CSS code.
# Spec: https://www.w3.org/TR/css-syntax-3/#consume-a-token
#
class Xcss::Lexer
  include Enumerable(Xcss::Token)

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
    when '"', '\''
      consume_string!
    when '-'
      consume_minus_etc!
    when ' ', '\t', '\n'
      consume_whitespace!
    when '+'
      consume_plus_etc!
    when '.'
      consume_dot_etc!
    when '/'
      consume_slash_etc!
    when '#'
      consume_hash_etc!
    when '<'
      consume_less_than_etc!
    when '@'
      consume_at_etc!
    when '|'
      consume_pipe_etc!
    when '^'
      consume_match!(:prefix_match)
    when '$'
      consume_match!(:suffix_match)
    when '*'
      consume_match!(:substring_match)
    when '~'
      consume_match!(:include_match)
    when ','
      consume_char!(:comma)
    when ':'
      consume_char!(:colon)
    when ';'
      consume_char!(:semicolon)
    when '{'
      consume_char!(:"{")
    when '}'
      consume_char!(:"}")
    when '['
      consume_char!(:"[")
    when ']'
      consume_char!(:"]")
    when '@'
      consume_char!(:"@")
    when '('
      consume_char!(:"(")
    when ')'
      consume_char!(:")")
    when '\0'
      Token.new(:EOF, line: @line, column: @column)
    else
      consume_char!(:delim)
    end
  end

  def each
    while true
      token = next_token!
      yield token
      break if token.type == :EOF
    end
  end

  private def current_char
    @reader.current_char
  end

  private def next_char!(advance_column = true)
    @column += 1 if advance_column
    @reader.next_char
  end

  private def consume_less_than_etc!
    # TODO
    consume_char!(:delim)
  end

  private def consume_pipe_etc!
    if @reader.peek_next_char == '|'
      t = Token.new(:column, line: @line, column: @column)
      next_char!
      t
    else
      consume_match!(:dash_match)
    end
  end

  private def consume_slash_etc!
    case @reader.peek_next_char
    when '*'
      next_char!
      while true
        if current_char == '\0'
          break
        elsif current_char == '*' && @reader.peek_next_char == '/'
          next_char!
          next_char!
          break
        end
        next_char!
      end
      next_token!
    else
      consume_char!(:delim)
    end
  end

  private def consume_minus_etc!
    case @reader.peek_next_char
    when '-'
      consume_atom!
    when '0'..'9'
      consume_number!
    else
      consume_char!(:delim)
    end
  end

  private def consume_at_etc!
    consume_char!(:delim)
  end

  private def consume_dot_etc!
    case @reader.peek_next_char
    when '0'..'9'
      consume_number!
    else
      consume_char!(:delim)
    end
  end

  private def consume_plus_etc!
    case @reader.peek_next_char
    when '0'..'9'
      consume_number!
    else
      consume_char!(:delim)
    end
  end

  private def consume_match!(type)
    case @reader.peek_next_char
    when '='
      line = @line
      column = @column
      next_char!
      next_char!
      Token.new(type, line: line, column: column)
    else
      consume_char!(:delim)
    end
  end

  private def consume_hash_etc!
    case @reader.peek_next_char
    when 'a'..'z', '0'..'9', '-'
      raise "TODO!"
    else
      consume_char!(:delim)
    end
  end

  private def consume_char!(type)
    t = Token.new(type, current_char.to_s, line: @line, column: @column)
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
      else
        break
      end
    end
    Token.new(:whitespace, io.to_s, line: line, column: column)
  end

  private def consume_string!
    io = IO::Memory.new
    line = @line
    column = @column
    delimeter = current_char
    next_char!
    while current_char != delimeter
      io << current_char
      next_char!
    end
    next_char!
    Token.new(:string, io.to_s, line: line, column: column)
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
    # Optional preceeding - or +
    if current_char == '-'
      io << current_char
      next_char!
    elsif current_char == '+'
      next_char!
    end
    # Digits
    if current_char == '.'
      io << '0'
    else
      chomp_digits!(io)
    end
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
