const colors = [
  "blueviolet",
  "darkgoldenrod",
  "darkgreen",
  "darkred",
  "deeppink",
  "goldenrod",
  "greenyellow",
  "hotpink",
  "indigo",
  "lightseagreen",
  "limegreen",
  "mediumpurple",
  "mediumvioletred",
  "navy",
  "orange",
  "powderblue",
  "teal",
];

function randomColor() {
  return colors[Math.floor(Math.random() * colors.length)];
}

export default randomColor();
