require("file?name=index.html!./index.html");

import springy         from "springy";
import { newRenderer } from "./renderer";

const graph = new springy.Graph();

const node1 = graph.newNode({label: "1"});
const node2 = graph.newNode({label: "2"});
const node3 = graph.newNode({label: "3"});
const node4 = graph.newNode({label: "4"});

graph.newEdge(node1, node2);
graph.newEdge(node2, node3);
graph.newEdge(node2, node4);

const layout = new springy.Layout.ForceDirected(
  graph,
  400.0, // Spring stiffness
  400.0, // Node repulsion
  0.5    // Damping
);

const canvas   = document.querySelector("canvas");
const renderer = newRenderer(canvas, layout);
renderer.start();
