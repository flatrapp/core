# Flatr-App-Core

## Configure:

```
cat > flatrapp.cfg << EOF
db               = "flatrapp.db"
port             = 8080
jwtSecret        = "secret"
whitelistedMails = [ "foobar@example.org" ]
EOF
```

## NixOS
### Prerequisites
`$ nix-env -i cabal-install`

### Build
Launch the nix shell

`$ cabal build`

### Run
Launch the nix shell
`$ cabal run`

### Launching the nix shell
Either launch it yourself with `$ nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-17.09.tar.gz`

or install [direnv](https://direnv.net)
and allow it do be active in the directory `direnv allow`.

---

## Non NixOS
### Prerequisites
Install stack with your package manager

### Build
`$ stack build --fast --pedantic`

### Run
`$ stack exec Flatr-App-Core-exe`
