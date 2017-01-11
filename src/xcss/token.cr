class Xcss::Token
  getter! type : Symbol
  getter! value : String
  getter! line : Int32
  getter! column : Int32

  def initialize(type, value = "", line = 1, column = 1)
    @type = type
    @value = value
    @line = line
    @column = column
  end

  def ==(other)
    self.type == other.type &&
      self.value == other.value
  end
end
