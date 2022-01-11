'use strict';

function impulseResponse(ctx, duration, decay) {
  const sampleRate = ctx.sampleRate;
  const length = sampleRate * duration;
  const impulse = ctx.createBuffer(2, length, sampleRate);
  const impulseL = impulse.getChannelData(0);
  const impulseR = impulse.getChannelData(1);

  if (!decay) { decay = 2.0; }
  for (var i = 0; i < length; i++){
    impulseL[i] = (Math.random() * 2 - 1) * Math.pow(1 - i / length, decay);
    impulseR[i] = (Math.random() * 2 - 1) * Math.pow(1 - i / length, decay);
  }
  return impulse;
}

function createReverb(ctx) {
  const convolver  = ctx.createConvolver();
  convolver.buffer = impulseResponse(ctx, 0.5, 8, false);
  return convolver;
}

export default createReverb;
