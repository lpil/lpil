require "../spec_helper"

private def it_lexes(src, type, value = "", file = __FILE__, line = __LINE__)
  it "lexes #{src.inspect}", file, line do
    lexer = Xcss::Lexer.new(src)
    token = lexer.next_token!
    token.type.should eq(type)
    token.value.should eq(value) if value != ""
  end
end

#
# https://www.w3.org/TR/css-syntax-3/#consume-a-token
#

describe Xcss::Lexer do
  # Whitespace
  it_lexes "    12", :whitespace, "    "
  it_lexes "\t\t\t", :whitespace, "\t\t\t"
  it_lexes "\n\n00", :whitespace, "\n\n"

  # " Strings
  it_lexes %(""), :string, ""
  it_lexes %("hiya"), :string, "hiya"
  it_lexes %("sup?"), :string, "sup?"
  it_lexes %("''''"), :string, "''''"

  # Hash
  # TODO
  # it_lexes "#hello", :hash, "hello"
  # TODO: Tests for - and \ names

  # # Delim
  it_lexes "#", :delim, "#"

  # suffix-match
  it_lexes "$=", :suffix_match

  # $ delim
  it_lexes "$x", :delim, "$"

  # ' Strings
  it_lexes %(''), :string, ""
  it_lexes %('hiya'), :string, "hiya"
  it_lexes %('sup?'), :string, "sup?"
  it_lexes %('""""'), :string, %("""")

  # ( paren
  it_lexes "(", :"("

  # ) paren
  it_lexes ")", :")"

  # Substring match
  it_lexes "*=", :substring_match

  # * Delim
  it_lexes "*", :delim, "*"

  # + Number
  it_lexes "+100", :number, "100"

  # + Delim
  it_lexes "+a", :delim, "+"

  # Comma
  it_lexes ",", :comma

  # - Number
  it_lexes "-1000", :number, "-1000"

  # - Ident
  # TODO

  # --> CDC
  # TODO

  # - Delim
  it_lexes "-", :delim, "-"

  # . Number
  it_lexes ".20", :number, "0.20"

  # . Delim
  it_lexes ".hi", :delim, "."

  # /* Comments */
  it_lexes "/*     ", :EOF
  it_lexes "/* * */", :EOF
  it_lexes "/* */23", :number, "23"

  # / Delim
  it_lexes "/", :delim, "/"

  # Colon
  it_lexes ":", :colon

  # Semicolon
  it_lexes ";", :semicolon

  # <!-- CDC
  # TODO

  # < Delim
  it_lexes "<", :delim, "<"

  # @ at-keyword
  # TODO

  # @ Delim
  it_lexes "@", :delim, "@"

  # [
  it_lexes "[", :"["

  # \ Escaped Ident
  # TODO

  # \ Delim
  # TODO

  # ^= prefix-match
  it_lexes "^=", :prefix_match

  # ^ Delim
  it_lexes "^", :delim, "^"

  # {
  it_lexes "{", :"{"
  # }
  it_lexes "}", :"}"

  # Numerical
  # TODO
  it_lexes "1", :number, "1"
  it_lexes "2", :number, "2"
  it_lexes "3", :number, "3"
  it_lexes "10", :number, "10"
  it_lexes "20.20", :number, "20.20"

  # U
  # TODO

  # u
  # TODO

  # Name start
  # TODO

  # | dash-match
  it_lexes "|=", :dash_match

  # || column
  it_lexes "||", :column

  # | delim
  it_lexes "|", :delim, "|"

  # ~ include-match
  it_lexes "~=", :include_match

  # ~ delim
  it_lexes "~", :delim, "~"

  # EOF
  it_lexes "", :EOF

  # Anything else
  it_lexes "`", :delim, "`"
  it_lexes "£", :delim, "£"

  it "keeps track of column numbers" do
    lexer = Xcss::Lexer.new("  1")
    lexer.next_token!
    lexer.next_token!.column.should eq(3)
  end

  it "keeps track of line numbers" do
    lexer = Xcss::Lexer.new("\n\n\n1")
    lexer.next_token!
    lexer.next_token!.line.should eq(4)
  end

  describe "#each" do
    it "iterates over all tokens" do
      lexer = Xcss::Lexer.new("1 2")
      tokens = [] of Xcss::Token
      lexer.each { |t| tokens << t }
      tokens.should eq([
        Xcss::Token.new(:number, "1"),
        Xcss::Token.new(:whitespace, " "),
        Xcss::Token.new(:number, "2"),
        Xcss::Token.new(:EOF),
      ])
    end
  end
end
