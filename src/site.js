import React from "react";
import ReactDOM from "react-dom";
import Gallery from "./gallery";

const photos = [
  {
    src: "https://source.unsplash.com/2ShvY8Lf6l0/800x599",
    width: 1,
    height: 1
  },
  {
    src: "https://source.unsplash.com/Dm-qxdynoEc/800x799",
    width: 1,
    height: 1
  },
  {
    src: "https://source.unsplash.com/qDkso9nvCg0/600x799",
    width: 1,
    height: 1
  },
  {
    src: "https://source.unsplash.com/iecJiKe_RNg/600x799",
    width: 1,
    height: 1
  },
  {
    src: "https://source.unsplash.com/epcsn8Ed8kY/600x799",
    width: 1,
    height: 1
  },
  {
    src: "https://source.unsplash.com/NQSWvyVRIJk/800x599",
    width: 1,
    height: 1
  },
  {
    src: "https://source.unsplash.com/zh7GEuORbUw/600x799",
    width: 1,
    height: 1
  },
  {
    src: "https://source.unsplash.com/PpOHJezOalU/800x599",
    width: 1,
    height: 1
  },
  {
    src: "https://source.unsplash.com/I1ASdgphUH4/800x599",
    width: 1,
    height: 1
  }
];

class App extends React.Component {
  constructor() {
    super();
  }

  render() {
    return <Gallery photos={photos} />;
  }
}

ReactDOM.render(<App photos={photos} />, document.getElementById("js-app"));
