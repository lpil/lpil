#!/usr/bin/env ruby
# encoding: utf-8

# Cost and payment should be formatted with pence as a decimal
# i.e.  '3.50'
#
# Demoninations are in pence
class ChangeFinder
  def self.calculate(cost, payment, denominations)
    cal(to_ele(payment) - to_ele(cost), denominations, {}).each do |x|
      puts x[0].to_s << "\t" << x[1].to_s
    end
  end

  private

  def self.to_ele(num, elem_per_unit)
    (num * elem_per_unit).to_i
  end

  def self.cal(money, denoms, change)
    return change if money == 0
    change[denoms[0][0].to_sym] = money / denoms[0][1] if money > denoms[0][1]
    cal money % denoms[0][1], denoms[1..-1], change
  end
end

# Rule Britania
class BritishChangeFinder < ChangeFinder
  def self.calculate(cost, payment)
    denominations = [
      ['£50', 5000], ['£20', 2000], ['£10', 1000], ['£5' ,  500],
      ['£2' ,  200], ['£1' ,  100], ['50p',   50], ['50p',   50],
      ['20p',   20], ['10p',   10], ['5p' ,    5], ['2p' ,    2],
      ['1p' ,    1]
    ]
    super cost, payment, denominations
  end

  private

  def self.to_ele(num, *_)
    super num, 100
  end
end

# Input cost, then payment
# i.e. ruby change.rb 1.50 5.00
BritishChangeFinder.calculate ARGV[0].to_f, ARGV[1].to_f
