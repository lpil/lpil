#!/bin/sh
set -eu

FUNCTIONS_BUCKET=gs://treasure-app-functions
STORAGE_BUCKET=gs://treasure-app-storage
PUBLIC_BUCKET=gs://treasure-app-public

createBucket() {
  echo
  echo "Ensuring bucket exists ($1)"
  gsutil ls "$1" > /dev/null || gsutil mb "$1" && echo "ok :)"
}

createBucket $FUNCTIONS_BUCKET
createBucket $STORAGE_BUCKET
createBucket $PUBLIC_BUCKET

echo
echo "Making bucket public ($PUBLIC_BUCKET)"
gsutil iam ch allUsers:objectViewer "$PUBLIC_BUCKET"

echo
echo "Deploying function"
gcloud beta functions deploy treasure-api \
  --stage-bucket "$FUNCTIONS_BUCKET" \
  --entry-point default \
  --source dist/server \
  --memory 128MB \
  --trigger-http

echo
echo "Uploading site assets"
gsutil cp dist/site/* "$PUBLIC_BUCKET"
