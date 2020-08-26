(
{
    r = MouseX.kr(1/3, 10);
    SinOsc.ar(
        freq: MouseY.kr(100, 700),
        mul: Linen.kr(
            gate: Impulse.kr(r),
            attackTime: 0,
            susLevel: 1,
            releaseTime: 1/r,
        )
    )
}.play
)

// Or when modulating the freq too

(
{
    r = MouseX.kr(1/3, 10);
    i = Impulse.kr(r);
    f = Linen.kr(
        gate: i,
        attackTime: 0,
        susLevel: 200,
        releaseTime: 1/r,
    );
    SinOsc.ar(
        freq: MouseY.kr(1, 200) + f,
        mul: Linen.kr(
            gate: i,
            attackTime: 0,
            susLevel: 1,
            releaseTime: 1/r,
        )
    )
}.play
)
