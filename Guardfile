guard :shell do
  watch(/src\/.*\.rs/) do |m|
    path = m.first

    mod = unless path['main'] || path['lib']
            path.sub(/src\//, '')
                .sub(/\/tests\.rs/, '.rs')
                .sub(/\/mod\.rs/, '.rs')
                .gsub(/\//, '::')
                .sub(/\.rs/, '::tests')
          end

    puts "\n\n\n\n"
    puts "\e[33mcargo test #{mod}\e[0m"
    `cargo test #{mod}`
  end
end
