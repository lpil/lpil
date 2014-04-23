#!/usr/bin/env ruby
# encoding: utf-8

def calc_num_primes(num)
  e, primes = 3, [2]
  until primes.size == num
    e_prime = true
    primes.each do |p|
      if e % p == 0
        e_prime = false
        break
      end
      break if p > Math.sqrt(e)
    end
    primes << e if e_prime
    e += 2
  end
  primes
end

nums = ARGF.file.read.split("\n")[1].split(' ').map { |e| e.to_i }
primes = calc_num_primes nums.max
nums.map! { |e| primes[e - 1] }
puts nums.join ' '
