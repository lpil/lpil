import Storage from "@google-cloud/storage";

// TODO: take from config
const bucketName = "lpil-public";
const projectId = "lpil-cloud";
const keyFilename = "gcloud-service-account-key.json";

const storage = new Storage({ projectId, keyFilename });
const bucket = storage.bucket(bucketName);

export const getGalleryPasswords = galleryId => {
  const parsePasswordsFromBuffer = buffer =>
    buffer
      .toString()
      .split("\n")
      .map(s => s.trim())
      .filter(s => s.length > 3);

  return bucket
    .file(galleryId + "/passwords.txt")
    .download()
    .then(parsePasswordsFromBuffer)
    .catch(error => {
      if (error.code === 404) {
        throw { type: "gallery-not-found" };
      } else {
        throw error;
      }
    });
};

export const getGalleryImages = galleryId => {
  return bucket
    .getFiles({ prefix: galleryId + "/images/" })
    .then(results =>
      results[0].map(file => file.name).filter(s => !s.endsWith("/"))
    );
};

export const getSignedUrls = paths => {
  const datetime = new Date();
  datetime.setHours(datetime.getHours() + 1);
  const expires = datetime.toISOString();

  const promises = paths.map(p =>
    bucket
      .file(p)
      .getSignedUrl({ action: "read", expires })
      .then(data => data[0])
  );
  return Promise.all(promises);
};
