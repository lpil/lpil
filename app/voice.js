'use strict';

const attackTime  = 0.05;
const releaseTime = 0.2;

export default class Voice {
  constructor(ctx, out) {
    const oscNode  = ctx.createOscillator();
    const gainNode = ctx.createGain();

    oscNode.type = 'triangle';
    oscNode.connect(gainNode);
    gainNode.connect(out);

    this.oscNode = oscNode;
    this.freqObj = oscNode.frequency;
    this.gainObj = gainNode.gain;
    this.ctx = ctx;
  }

  play(hz) {
    this.freqObj.value = hz;
    const now = this.ctx.currentTime;
    this.oscNode.start();
    this.gainObj.cancelScheduledValues(now);
    this.gainObj.setValueAtTime(0, now);
    this.gainObj.linearRampToValueAtTime(0.5, now + attackTime);
  }
  stop() {
    const now = this.ctx.currentTime;
    this.gainObj.cancelScheduledValues(now);
    this.gainObj.setValueAtTime(this.gainObj.value, now);
    this.gainObj.linearRampToValueAtTime(0, now + releaseTime);
    setTimeout(
      () => { console.log('hi');this.oscNode.stop(); },
      1000 * releaseTime * 2
    );
  }

}

