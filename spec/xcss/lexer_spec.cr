require "../spec_helper"

private def it_lexes(src, type, value = "", file = __FILE__, line = __LINE__)
  it "lexes #{src.inspect}", file, line do
    lexer = Xcss::Lexer.new(src)
    token = lexer.next_token!
    token.type.should eq(type)
    token.value.should eq(value)
  end
end

describe Xcss::Lexer do
  # EOF
  it_lexes "", :EOF

  # Numbers
  it_lexes "1", :number, "1"
  it_lexes "2", :number, "2"
  it_lexes "3", :number, "3"
  it_lexes "10", :number, "10"
  it_lexes "20.20", :number, "20.20"
  it_lexes "-1000", :number, "-1000"

  # Delimeters
  it_lexes "{", :"{"
  it_lexes "}", :"}"

  # Classes
  it_lexes ".", :"."

  # IDs
  it_lexes "#", :"#"

  # Punctuation
  it_lexes ",", :","
  it_lexes ";", :";"

  # Mathematical operators
  it_lexes "+", :"+"
  it_lexes "-", :"-"
  it_lexes "/", :"/"
  it_lexes "*", :"*"

  # Atoms
  it_lexes "main", :atom, "main"
  it_lexes "s-pp", :atom, "s-pp"
  it_lexes "--go", :atom, "--go"
  it_lexes "d__b", :atom, "d__b"
  it_lexes "_db_", :atom, "_db_"

  # Whitespace
  it_lexes "    12", :ws, "    "
  it_lexes "\t\t\t", :ws, "\t\t\t"
  it_lexes "\n\n00", :ws, "\n\n"

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
end
