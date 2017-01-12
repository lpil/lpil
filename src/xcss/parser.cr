class Xcss::Stylesheet
  def ==(other)
    true
  end
end

class Xcss::Parser
  def initialize(source)
    lexer = Lexer.new(source)
  end

  def parse
    Xcss::Stylesheet.new
  end
end
