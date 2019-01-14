const svgContainer = document.querySelector('.js-svg');
const ns = 'http://www.w3.org/2000/svg';

function drawRing({ parent, radius }) {
  const svg = document.createElementNS(ns, 'circle');
  svg.setAttribute('cx', 0);
  svg.setAttribute('cy', 0);
  svg.setAttribute('r', radius);
  svg.setAttribute('fill', 'none');
  svg.setAttribute('stroke', 'black');
  svg.setAttribute('stroke-width', 0.25);
  parent.appendChild(svg);
}

function drawStaggeredRings({ radius, radiusStep, numRings, delay }) {
  const parent = document.createElementNS(ns, 'g');
  svgContainer.appendChild(parent);
  doDrawStaggeredRings({ parent, radius, radiusStep, numRings, delay });
  return parent;
}

function doDrawStaggeredRings({ parent, radius, radiusStep, numRings, delay }) {
  if (numRings < 1) { return; }
  drawRing({ parent, radius });
  const drawNext = () => {
    doDrawStaggeredRings({
      parent,
      radius: radius + radiusStep,
      radiusStep,
      numRings: numRings - 1,
      delay,
    });
  }
  setTimeout(drawNext, delay);
}

function drawMapPoint({ x, y }) {
  const rings =  drawStaggeredRings({
    radius: 5,
    radiusStep: 5,
    numRings: 4,
    delay: 200,
  });
  rings.setAttribute('transform', `translate(${x} ${y})`);
}

drawMapPoint({ x: 10, y: 10 });
setTimeout(() => drawMapPoint({ x: 20, y: 50 }), 350);
