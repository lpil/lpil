#!/usr/bin/env ruby

class Book
  attr_reader :title, :author, :category

  def initialize(title, author, category)
    @title    = title
    @author   = author
    @category = category
  end
end
