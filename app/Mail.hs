{-# LANGUAGE OverloadedStrings     #-}

module Mail
    ( sendBuiltMail
    , buildVerificationMail
    , buildInvitationMail
    )
where

import           Data.Text                   (Text, unpack)
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as TL
import           Formatting                  ((%), stext, format)
import qualified Network.Mail.Mime           as Mime
import           Network.HaskellNet.SMTP     ( authenticate, sendMimeMail2
                                             , AuthType(PLAIN) )
import           Network.HaskellNet.SMTP.SSL (doSMTPSTARTTLSWithSettings)
import           Network.HaskellNet.SSL      (defaultSettingsWithPort)
import           Network.Socket              (PortNumber)

import qualified Config                      as Cfg
import           Model.CoreTypes             (Email)


sendMail :: Cfg.SmtpConfig -> Mime.Mail -> IO ()
sendMail cfg mail = do
        putStrLn "Trying to connect to SMTP server"
        doSMTPSTARTTLSWithSettings host settings $ \conn -> do
          putStrLn "Trying to authenticate"
          authSucceed <- authenticate PLAIN user password conn
          if authSucceed
              then do putStrLn "Sending mail"
                      sendMimeMail2 mail conn
                      putStrLn "Sent mail"
              else putStrLn "Authentication with SMTP server failed"
  where host     = unpack $ Cfg.host cfg
        port     = fromInteger $ Cfg.smtpPort cfg :: PortNumber
        user     = unpack $ Cfg.username cfg
        password = unpack $ Cfg.password cfg
        settings = defaultSettingsWithPort port

sendBuiltMail :: Cfg.FlatrCfg -> Email
              -> (Cfg.SmtpConfig -> Email -> Mime.Mail)
              -> IO ()
sendBuiltMail cfg emailAddress builder
  | (Just smtpConfig) <- Cfg.smtpConfig cfg =
      sendMail smtpConfig $ builder smtpConfig emailAddress
  | otherwise =
    putStrLn "No smtp config was set so no email was sent"

buildMail :: Text -> Maybe Text -> TL.Text -> Cfg.SmtpConfig -> Text -> Mime.Mail
buildMail subject username body smtpConfig toAddress =
  Mime.Mail { Mime.mailFrom    = Mime.Address (Just "FlatrAdmin") from
            , Mime.mailTo      = [Mime.Address username toAddress]
            , Mime.mailCc      = []
            , Mime.mailBcc     = []
            , Mime.mailHeaders = [("Subject", subject)]
            , Mime.mailParts   = [[bodyPart]]
            }
  where from = Cfg.sender smtpConfig
        bodyPart = Mime.Part "text/plain; charset=utf-8"
                             Mime.QuotedPrintableText Nothing [] $ TL.encodeUtf8 body

buildVerificationMail :: Text -> Text -> Cfg.SmtpConfig -> Email -> Mime.Mail
buildVerificationMail code username =
  buildMail subject (Just username) body
  where subject = "Flatr Verification"
        body = format
                   ( "Please confirm your email adress by visiting this URL"
                     % stext % "/#signup?code=" % stext % "&serverUrl=" % stext
                   ) frontendUrl code serverUrl
        serverUrl = "https://localhost:8080" :: Text
        frontendUrl = "https://localhost:8000" :: Text

buildInvitationMail :: Text -> Cfg.SmtpConfig -> Email -> Mime.Mail
buildInvitationMail code =
  buildMail subject Nothing body
  where subject = "Flatr Invitation"
        body = format
                   ( "You are invited to join Flatr, to accept visit this URL"
                     % stext % "/#signup?code=" % stext % "&serverUrl=" % stext
                   ) frontendUrl code serverUrl
        serverUrl = "https://localhost:8080" :: Text
        frontendUrl = "https://localhost:8000" :: Text
