#!/bin/sh
set -eu

BUCKET=gs://reasonable-serverless-graphql-functions

echo "Ensuring storage bucket exists ($BUCKET)"
gsutil ls "$BUCKET" > /dev/null || gsutil mb "$BUCKET" && echo "Cool. :)"

echo "Deploying function"
gcloud beta functions deploy api \
  --stage-bucket "$BUCKET" \
  --entry-point handler \
  --source dist \
  --trigger-http
