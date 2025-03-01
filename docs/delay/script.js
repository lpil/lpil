const delayedVideo = document.getElementById("delayedVideo");
const startButton = document.getElementById("startButton");
const delayInput = document.getElementById("delayInput");
const controls = document.getElementById("controls");

let delayedStream = null;
let animationFrameId = null;
let isRunning = false;

startButton.addEventListener("click", async () => {
  try {
    // Get delay value in milliseconds
    const delayMs = parseFloat(delayInput.value) * 1000;

    if (isNaN(delayMs) || delayMs < 100) {
      alert("Please enter a valid delay (minimum 0.1 seconds)");
      return;
    }

    startButton.disabled = true;
    startButton.textContent = "Starting...";

    // Get user media with high resolution
    const constraints = {
      video: {
        width: { ideal: 1920 },
        height: { ideal: 1080 },
      },
    };

    const stream = await navigator.mediaDevices.getUserMedia(constraints);

    // Set up delay
    await setupDelayedFeed(stream, delayMs);

    // Hide controls after successful start
    controls.classList.add("hidden");
  } catch (err) {
    alert(`Error accessing camera: ${err.message}`);
    startButton.disabled = false;
    startButton.textContent = "Start Camera";
    console.error("Error accessing camera:", err);
  }
});

async function setupDelayedFeed(stream, delayMs) {
  // Fixed array buffer with circular index to avoid memory leak
  const BUFFER_SIZE = Math.ceil((delayMs / 1000) * 30) + 10; // 30fps + buffer
  const frameBuffer = new Array(BUFFER_SIZE);
  let writeIndex = 0;
  let frameCount = 0;

  // Create two canvases - one for capturing and one for displaying
  const captureCanvas = document.createElement("canvas");
  const captureCtx = captureCanvas.getContext("2d", {
    willReadFrequently: true,
  });

  const displayCanvas = document.createElement("canvas");
  const displayCtx = displayCanvas.getContext("2d");

  // Get video track settings
  const videoTrack = stream.getVideoTracks()[0];
  const settings = videoTrack.getSettings();

  // Set initial canvas dimensions to match video source
  captureCanvas.width = settings.width;
  captureCanvas.height = settings.height;
  displayCanvas.width = settings.width;
  displayCanvas.height = settings.height;

  // Create a delayed video stream from the display canvas
  delayedStream = displayCanvas.captureStream();
  delayedVideo.srcObject = delayedStream;

  // Set up video element for capturing
  const videoElement = document.createElement("video");
  videoElement.srcObject = stream;
  videoElement.autoplay = true;
  videoElement.muted = true;

  // Wait for video to be ready
  await new Promise((resolve) => {
    videoElement.onloadedmetadata = resolve;
  });
  await videoElement.play();

  // Start the capture process
  let lastCaptureTime = 0;
  const frameInterval = 1000 / 30; // Target 30fps
  isRunning = true;

  function captureFrame(now) {
    if (!isRunning) return;

    // Capture at target framerate
    if (now - lastCaptureTime >= frameInterval) {
      // Draw current frame to capture canvas
      captureCtx.drawImage(
        videoElement,
        0,
        0,
        captureCanvas.width,
        captureCanvas.height,
      );

      // Create a copy of the frame data
      const imageData = captureCtx.getImageData(
        0,
        0,
        captureCanvas.width,
        captureCanvas.height,
      );

      // Store frame with timestamp using circular buffer
      frameBuffer[writeIndex] = {
        imageData,
        timestamp: now,
      };

      // Update write index for circular buffer
      writeIndex = (writeIndex + 1) % BUFFER_SIZE;
      frameCount = Math.min(frameCount + 1, BUFFER_SIZE);

      lastCaptureTime = now;
    }

    // Find the frame that's closest to our desired delay
    let bestFrame = null;
    let bestDelta = Infinity;

    // Only search through the valid frames we have
    for (let i = 0; i < frameCount; i++) {
      const idx = (writeIndex - 1 - i + BUFFER_SIZE) % BUFFER_SIZE;
      const frame = frameBuffer[idx];

      if (frame) {
        const delta = Math.abs(now - frame.timestamp - delayMs);
        if (delta < bestDelta) {
          bestDelta = delta;
          bestFrame = frame;
        }
      }
    }

    // Draw the delayed frame to the display canvas
    if (bestFrame) {
      displayCtx.putImageData(bestFrame.imageData, 0, 0);
    }

    // Continue the loop
    animationFrameId = requestAnimationFrame(captureFrame);
  }

  // Start the render loop
  animationFrameId = requestAnimationFrame(captureFrame);
}

// Clean up resources when page is unloaded
window.addEventListener("beforeunload", () => {
  isRunning = false;

  if (animationFrameId) {
    cancelAnimationFrame(animationFrameId);
    animationFrameId = null;
  }

  if (delayedVideo.srcObject) {
    delayedVideo.srcObject = null;
  }

  if (delayedStream) {
    delayedStream.getTracks().forEach((track) => track.stop());
  }
});
