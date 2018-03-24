{-# LANGUAGE OverloadedStrings     #-}

module Mail
    ( sendBuiltMail
    , buildVerificationMail
    , buildInvitationMail
    )
where

import           Data.Text         (Text, unpack)
import           Formatting        ((%), stext, format)
import qualified Network.Mail.Mime as Mime
import           Network.Socket    (PortNumber)

import qualified Config            as Cfg
import           Model.CoreTypes   (Email)

import Network.HaskellNet.SMTP     hiding (sendMail)
--import Network.HaskellNet.Auth
import Network.HaskellNet.SMTP.SSL hiding (sendMail)
import Network.HaskellNet.SSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

sendMail :: Cfg.SmtpConfig -> Mime.Mail -> IO ()
sendMail cfg mail = doSMTPSTARTTLSWithSettings host settings $ \conn -> do
                   authSucceed <- authenticate PLAIN user password conn
                   if authSucceed
                       then sendMimeMail2 mail conn
                       else putStrLn "Authentication failed lol"
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
  | otherwise = putStrLn "No smtp config was set so no verification email was sent"

buildMail :: Text
          -> Maybe Text
          -> TL.Text
          -> Cfg.SmtpConfig
          -> Text
          -> Mime.Mail
buildMail subject username body smtpConfig toAddress =
  Mime.Mail { Mime.mailFrom    = Mime.Address (Just "FlatrAdmin") from
            , Mime.mailTo      = [Mime.Address username toAddress]
            , Mime.mailCc      = []
            , Mime.mailBcc     = []
            , Mime.mailHeaders = [("Subject", "This is the subject")]
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
