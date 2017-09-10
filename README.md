# Spock-example

Configure:

```
cat > flatrapp.cfg << EOF
db        = "flatrapp.db"
port      = 8080
jwtSecret = "secret"
EOF
```

Build:

`$ stack build --fast --pedantic`

Run:

`$ stack exec Spock-example-exe`
