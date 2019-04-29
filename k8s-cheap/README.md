# k8s cheap

One app deployed to k8s.

Multiple application envs in one cluster, isolated using namespaces. i.e. dev,
staging, prod


## Usage

```sh
gcloud init
bin/infra ops init
bin/infra ops plan
bin/infra ops apply

# This command can also be found in the GCP GKE clusters list under "connect"
gcloud beta container clusters get-credentials learning-k8s-cheap--ops --region europe-west1 --project gcp-simple-app-dev

bin/infra ops destroy
```
