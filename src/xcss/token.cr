class Xcss::Token
  getter! type : Symbol
  getter! value : String

  def initialize(type, value = "")
    @type = type
    @value = value
  end

  def ==(other)
    self.type == other.type &&
      self.value == other.value
  end
end
