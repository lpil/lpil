# SSH

## Port forward

```sh
ssh -L 8000:localhost:8384 cubone
```

Expose remote server `cubone`'s port 8384 on the current computer as `localhost:8000`.
