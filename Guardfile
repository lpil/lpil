guard :shell do
  watch(/src\/.*\.rs/) do |m|
    mod = m.first.sub(/src\//, '')
                 .sub(/\/tests/, '')
                 .sub(/\/mod/, '')
                 .gsub(/\//, '::')
                 .sub(/\.rs/, '::tests')

    puts "\n\n\n\n"
    puts "\e[33mcargo test #{mod}\e[0m"
    `cargo test #{mod}`
  end
end
