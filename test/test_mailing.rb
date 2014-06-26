require_relative 'test_helper.rb'

# Test general application
class TestMailing < Minitest::Test
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_can_query_db_for_mailing
    Mailing.first
  end

  def test_can_query_specific_mailing
    m = create :mailing
    assert_equal Mailing.find_by_order_ref(m[:order_ref]), m
  end

  def test_mailing_lookup_returns_hash
    m = create :mailing
    assert_equal Mailing.lookup(m[:order_ref]).class, Hash
  end

  def test_mailing_lookup_keys_are_symbols
    m = create :mailing
    all_syms = Mailing.lookup(m[:order_ref]).keys.reduce(true) do |acc, key|
      acc && (key.class == Symbol)
    end
    assert all_syms, 'Mailing.lookup hash keys are not all symbols'
  end

  def test_mailing_lookup_includes_dpd_url
    m = create :mailing
    assert Mailing.lookup(m[:order_ref]).include? :url
  end
end
