notes = (
  scale(:c2, :minor_pentatonic, num_octaves: 5) +
  scale(:c2, :minor_pentatonic, num_octaves: 5).reverse
)

2.times do
  with_fx :echo do
    use_synth :sine
    play_pattern_timed(
      notes,
      0.125,
      release: 0.1
    )
  end
end
