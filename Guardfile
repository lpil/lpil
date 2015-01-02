guard :shell, all_on_start: true do
  watch(/.*\.rs/) do
    puts "\n\n\n\n\n\n"
    `cargo test`
  end
end
