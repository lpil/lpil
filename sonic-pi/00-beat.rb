bpm = 172
beat = 60.0 / bpm

step = 0

live_loop :clock do
  puts "bar: #{step}"
  sleep beat * 3.9
  step = (step + 1) % 32
  sleep beat * 0.1
end

live_loop :straight do
  sample :bd_haus, rate: 1
  sleep beat
end

live_loop :offbeat do
  sleep (beat * 3) + (beat / 2)
  sample :bd_haus, rate: 1
  sleep beat / 2
end

live_loop :snap do
  sleep beat / 2
  sample :perc_snap, rate: 0.7
  sleep beat / 2
  sleep beat
end

live_loop :snare do
  if step >= 16
    sleep beat
    sample :drum_snare_hard, rate: 1.3
    sleep beat * 2
    sample :drum_snare_hard, rate: 1.3
    sample :drum_snare_hard, rate: 0.8
    sleep beat
  else
    sleep beat * 4
  end
end

rates = [1, 0.9]
live_loop :bass do
  sample :bass_voxy_c, rate: rates.first
  rates.rotate! 1
  sleep beat * 16
end
