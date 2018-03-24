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
`$ nix-build`

### Run
Launch the nix shell
`$ ./result/bin/Flatr-App-Core-exe`

### Launching the nix shell
Either launch it yourself with `$ nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-17.09.tar.gz`

or install [direnv](https://direnv.net)
and allow it do be active in the directory `direnv allow`.

---

## Non NixOS
### Prerequisites
Install stack with your package manager

### Configure
`$ stack setup --install-ghc`

### Build
`$ # install system dependencies like e.g zlib`
`$ stack build --fast --pedantic`

### Run
`$ stack exec Flatr-App-Core-exe`

### Compile statically linked
`$ stack build --ghc-options='-optl-static -optl-pthread' --force-dirty`
