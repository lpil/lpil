# encoding: utf-8

foo = {}
foo.default = {}
foo[:bar][:one] = 1
# foo seems to be empty, yet I can access the value within
p foo.empty?      # => true
p foo             # => {}
p foo[:bar]       # => {:one=>1}
p foo[:bar][:one] # => 1

foo = {}
foo.default = {}
a = foo[:bar]
a[:one] = 1
foo[:bar] = a
# foo behaves as I would expect with this method
p foo.empty?      # => false
p foo             # => {:bar=>{:one=>1}}
p foo[:bar]       # => {:one=>1}
p foo[:bar][:one] # => 1

p RUBY_VERSION    # => "2.1.0"
