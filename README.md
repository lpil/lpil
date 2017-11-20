# Cube

### Setup

```sh
# Create Kubenetes cluster on Google Cloud Platfrom Container Engine
gcloud container clusters create MY_CLUSTER_NAME

# Configure CLI tools to connect to the cluster
gcloud auth application-default login
```

### Checking out the cluster

```sh
# Check CLI client and K8s server versions
kubectl version

# View cluster components
kubectl get componentstatus

# View cluster nodes
kubectl get nodes

# Inspect a node
kubectl describe nodes NAME_OF_A_NODE

# Run a HTTP proxy (UI on localhost:8001/ui)
kubectl proxy
```

### Manipulating objects

```sh
# Use a config file to create/update objects
kubectl apply -f my-file.yml

# Delete objects described in config file
kubectl delete -f my-file.yml
```

### Manipulating object labels

```sh
# Add a label to an object
kubectl label pods MY_POD_NAME colour=red

# Remove a lable
kubectl label pods MY_POD_NAME -colour
```

### Debugging

```sh
# View pod logs
kubectl logs MY_POD_NAME

# Exec a shell in a running container
kubectl exec -it MY_POD_NAME -- bash

# Copy a file from a container to the local filesystem
kubectl cp MY_POD_NAME:/path/to/remote/file /path/to/local/target
```
