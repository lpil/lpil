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
  it "keeps track of column numbers" do
    Xcss::Lexer.new("  1").next_token!.column.should eq(3)
  end

  it "keeps track of line numbers" do
    Xcss::Lexer.new("\n\n\n1").next_token!.line.should eq(4)
  end

  # EOF
  it_lexes "", :EOF

  # Numbers
  it_lexes "1", :number, "1"
  it_lexes "2", :number, "2"
  it_lexes "3", :number, "3"
  it_lexes "10", :number, "10"
  it_lexes "20.20", :number, "20.20"

  # Whitespace
  it_lexes "     ", :EOF
  it_lexes "\t\t3", :number, "3"

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
end
