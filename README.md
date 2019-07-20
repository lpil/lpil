# Treasure

A password protected photo gallery app.

## Dev setup

```sh
gcloud auth login
gcloud config set project $GCP_PROJECT_ID
gcloud iam service-accounts create $SERVICE_ACCOUNT_NAME
gcloud iam service-accounts keys create gcloud-service-account-key.json \
  --iam-account $SERVICE_ACCOUNT_NAME@$GCP_PROJECT_ID.iam.gserviceaccount.co

yarn install

make start
```
