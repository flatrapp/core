# Flatr-App-Core

## Configure:

```
cat > flatrapp.cfg << EOF
db        = "flatrapp.db"
port      = 8080
jwtSecret = "secret"
EOF
```

## Prerequisites
`$ nix-env -i cabal-install`

## Build
Launch the nix shell

`$ cabal build`

## Run
Launch the nix shell
`$ cabal run`


## Launching the nix shell
Either launch it yourself with `$ nix-shell`

or install [direnv](https://direnv.net)
and allow it do be active in the directory `direnv allow`.
