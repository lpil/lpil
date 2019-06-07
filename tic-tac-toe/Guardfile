guard :shell do
  watch(/src\/.+\.rs/) do
    puts "

\e[33m$ cargo test\e[0m

"
    `cargo test`
  end
end
