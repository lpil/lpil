guard :shell do
  watch(/src\/.*\.rs/) do |m|
    path = m.first

    mod = unless %w(src/main.rs src/lib.rs).include? path
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
