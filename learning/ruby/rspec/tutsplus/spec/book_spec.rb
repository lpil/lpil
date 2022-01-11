#!/usr/bin/env ruby

require_relative 'spec_helper'

# Lets describe the book

describe Book do

  # A hook for prep code.
  #   :each runs every time before EACH each
  #   :all runs once before we start running tests
  before :each do
    @title    = 'The Flintstones'
    @author   = 'Fred Flintstone'
    @category = :PeriodDrama
    @book = Book.new @title, @author, @category
  end

  describe '#new' do
    it 'returns a Book obj' do
      expect(@book).to be_a(Book)
    end
  end

  describe '#title' do
    it 'returns the correct author' do
      expect(@book.title).to eq(@title)
    end
  end

  describe '#author' do
    it 'returns the correct author' do
      expect(@book.author).to eq(@author)
    end
  end

  describe '#category' do
    it 'returns the correct category' do
      expect(@book.category).to eq(@category)
    end
  end
end
