(
{
    Blip.ar(
        freq: TRand.kr(lo: 100, hi: 1000, trig: Impulse.kr(Line.kr(1, 20, 60))),
        numharm: TRand.kr(lo: 1, hi: 10, trig: Impulse.kr(Line.kr(1, 20, 60))),
        mul: Linen.kr(
            gate: Impulse.kr(Line.kr(1, 20, 60)),
            attackTime: 0,
            susLevel: 0.5,
            releaseTime: 1 / Line.kr(1, 20, 60),
        )
    ).dup(2)
}.play
)
