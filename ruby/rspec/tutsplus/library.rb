#!/usr/bin/env ruby

class Library
  def initialize(books = [])
    @books = books
  end

  def books
    @books.length
  end

  def get_books_in_category(category)
    @books.select { |book| book.category == category }
  end

  def get_book(title)
    @books.find { |book| book.title == title }
  end

  def add_book(book)
    @books << book
    self
  end
end
