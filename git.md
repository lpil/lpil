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
