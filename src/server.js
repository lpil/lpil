import express from "express";
import Storage from "@google-cloud/storage";

const server = express();

server.get("/", (req, res) => res.send("Hello World!"));

export default server;
