const display = document.querySelector("time");

let time = 0;

function playPing() {
  const context = new window.AudioContext();
  const oscillator = context.createOscillator();
  const gainNode = context.createGain();
  oscillator.connect(gainNode);
  gainNode.connect(context.destination);
  oscillator.frequency.setValueAtTime(440, context.currentTime);
  gainNode.gain.setValueAtTime(0, context.currentTime);
  gainNode.gain.linearRampToValueAtTime(1, context.currentTime + 0.001);
  gainNode.gain.linearRampToValueAtTime(0, context.currentTime + 0.1);
  oscillator.start(context.currentTime);
  oscillator.stop(context.currentTime + 0.1);
}

function updateDisplay() {
  const minutes = Math.floor(time / 60);
  const seconds = time % 60;
  const paddedMinutes = minutes.toString().padStart(2, "0");
  const paddedSeconds = seconds.toString().padStart(2, "0");
  display.textContent = `${paddedMinutes}:${paddedSeconds}`;
}

for (const element of document.querySelectorAll("[data-add]")) {
  const amount = parseInt(element.dataset.add);
  element.addEventListener("click", () => {
    time += amount;
    updateDisplay();
    ensureTimerRunning();
  });
}

let tickInterval = null;

function tick() {
  time -= 1;
  updateDisplay();
  if (time < 1) {
    playPing();
    stopTimer();
  }
}

function ensureTimerRunning() {
  if (tickInterval === null) {
    tickInterval = setInterval(tick, 1000);
    navigator.wakeLock.request("screen");
  }
}

function stopTimer() {
  clearInterval(tickInterval);
  tickInterval = null;
}

document.addEventListener("visibilitychange", () => {
  if (tickInterval && document.visibilityState === "visible") {
    navigator.wakeLock.request("screen");
  }
});

requestWakeLock();
