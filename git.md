# git

## Add repo to another repo

```shell
git subtree add \
  -P $REPO1/directory/in/which/to/insert/repo2
  $REPO2 \
  $REPO2_COMMIT_REF
```

## Delete tag from remote

```shell
git push --delete origin $TAG_NAME
```

## Extract directory from repo keeping history

This delete all the other directories, so do it in a fresh copy of the repo.

```shell
git filter-branch --prune-empty --subdirectory-filter $FOLDER_NAME $BRANCH_NAME
```
