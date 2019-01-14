import springy from "springy";

function newRenderer(canvas, layout) {
  const ctx = canvas.getContext("2d");

  let currentBB = layout.getBoundingBox();
  let targetBB  = {
    bottomleft: new springy.Vector(-2, -2),
    topright:   new springy.Vector(2, 2),
  };

  // Auto adjusting bounding box.
  // Current gets 20% closer to target every iteration
  springy.requestAnimationFrame(function adjust() {
    targetBB = layout.getBoundingBox();

    const bottomleft = currentBB.bottomleft.add(
      targetBB.bottomleft.subtract(currentBB.bottomleft).divide(10)
    );
    const topright = currentBB.topright.add(
      targetBB.topright.subtract(currentBB.topright).divide(10)
    );
    currentBB = { bottomleft, topright };

    springy.requestAnimationFrame(adjust);
  });

  function toScreen(p) {
    const size = currentBB.topright.subtract(currentBB.bottomleft);
    const n  = p.subtract(currentBB.bottomleft);
    const sx = n.divide(size.x).x * canvas.width;
    const sy = n.divide(size.y).y * canvas.height;
    return new springy.Vector(sx, sy);
  }

  function clear() {
    canvas.width = canvas.width;
  }

  function drawEdge(edge, p1, p2) {
    ctx.save();

    const one = toScreen(p1);
    const two = toScreen(p2);

    ctx.lineWidth   = 1;
    ctx.strokeStyle = "#000000";

    ctx.beginPath();
    ctx.moveTo(one.x, one.y);
    ctx.lineTo(two.x, two.y);
    ctx.stroke();

    ctx.restore();
  }

  function drawNode(node, p) {
    ctx.save();

    const s = toScreen(p);
    ctx.fillStyle = "hotpink";
    ctx.fillRect(s.x, s.y, 5, 5);

    ctx.restore();
  }

  const spRenderer = new springy.Renderer(
    layout, clear, drawEdge, drawNode
  );

  const start = () => spRenderer.start();
  const stop  = () => spRenderer.stop();

  return { start, stop };
}

export { newRenderer };
