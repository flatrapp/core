{ db               = "flatrapp.db"
, port             = 8080
, jwtSecret        = "secret"
, whitelistedMails = [ "foobar@example.org"
                     , "foobar@example.com"
                     ]
, smtpConfig       = [{ host     = "smtp.example.org"
                      , smtpPort = 587
                      , username = "admin@flatr.example.org"
                      , password = "pa$sw0rd"
                      , sender   = "admin@flatr.example.org"
                      }] : Optional ./smtpConfig.dhall
}
