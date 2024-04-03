#!/usr/bin/env ruby

require_relative 'spec_helper'

describe Library do
  before :each do
    @books = [
      Book.new('JavaScript Good Parts', 'Douglas Crockford', :development),
      Book.new('Designing  Web Standards', 'Jeffrey Zeldman', :design),
      Book.new("Don't Think", 'Steve Krug', :usability),
      Book.new('JavaScript Patterns', 'Stoyan Stefanov', :development),
      Book.new('Responsive Design', 'Ethan Marcotte', :design)
    ]

    @lib = Library.new @books
  end

  describe '#new' do
    context 'when passed no books' do
      it 'has no books' do
        expect(Library.new.books).to eq(0)
      end
    end

    context 'when passed some books' do
      it 'has the right number of books' do
        expect(@lib.books).to eq(@books.length)
      end
    end
  end

  describe '#get_books_in_category' do
    it 'returns the books for the category' do
      expect(@lib.get_books_in_category(:development)).to eq(
        @books.select { |book| book.category == :development })

      expect(@lib.get_books_in_category(:design)).to eq(
        @books.select { |book| book.category == :design })
    end
  end

  describe '#get_book' do
    it 'returns a book with the title passed' do
      title = 'JavaScript Good Parts'
      expect(@lib.get_book title).to eq(@books.find { |b| b.title == title })
    end

    it "returns nil when the book doesn't exist" do
      expect(@lib.get_book 'Some missing title').to be_nil
    end

    it 'it does not modify the books array' do
      expect { @lib.get_book 'JavaScript Good Parts' }
        .not_to change { @lib.books }
    end
  end

  describe '#add_book' do
    before :all do
      @new_book = Book.new('Rails Tutorial', 'A smart dude', :development)
    end

    it 'adds a book to the library' do
      expect(@lib.get_book @new_book.title).to be_nil
      @lib.add_book @new_book
      expect(@lib.get_book @new_book.title).not_to be_nil
    end

    it 'it modifies the books array' do
      expect { @lib.add_book @new_book }
        .to change { @lib.books }
    end
  end
end
