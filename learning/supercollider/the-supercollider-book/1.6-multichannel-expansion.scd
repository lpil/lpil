(
{
    // Multichannel expansion means that the UGen is expanded into multiple
    // channels, one per item in the array.
    var freqs = [5, 10, 2, 25, 1, 9, 2];
    var noise = LFNoise0.kr(freq: freqs, mul: 12, add: 14);
    Blip.ar(freq: 25, numharm: noise, mul: 0.3)
}.play
)