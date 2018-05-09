import express from "express";
import * as storage from "./storage";

const server = express();

const handleError = res => error => {
  switch (error.type) {
    case "gallery-not-found":
      console.log("gallery not found");
      return res.status(404).json({ error: "gallery not found" });

    case "invalid-password":
      console.log("invalid password");
      return res.status(400).json({ error: "invalid password" });

    default:
      console.error(error);
      res.status(500).json({ error: "internal server error" });
  }
};

const checkPassword = password => passwords => {
  if (!passwords.includes(password)) {
    throw { type: "invalid-password" };
  }
};

server.get("/", (req, res) => res.send("Hello, mum!"));

server.get("/gallery/:id", (req, res) => {
  const password = req.query.password;
  const galleryId = req.params.id;

  storage
    .getGalleryPasswords(galleryId)
    .then(checkPassword(password))
    .then(_ => storage.getGalleryImages(galleryId))
    .then(storage.getSignedUrls)
    .then(images => {
      res.json({ images });
    })
    .catch(handleError(res));
});

export default server;
