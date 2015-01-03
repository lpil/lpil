guard :shell do
  watch(/src\/.*\.rs/) do |m|
    mod = m.first.sub(/src\//, '')
                 .sub(/\.rs/, '')
                 .gsub(/\//, '::') << '::tests'

    puts "\n\n\n\n"
    puts "cargo test #{mod}"
    `cargo test #{mod}`
  end
end
