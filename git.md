# git

## Add repo to another repo

```shell
git subtree \
  -P $REPO1/directory/in/which/to/insert/repo2
  $REPO2 \
  $REPO2_COMMIT_REF
```
