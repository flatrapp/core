{-# LANGUAGE OverloadedStrings     #-}

module Mail
    ( sendBuiltMail
    , buildVerificationMail
    , buildInvitationMail
    )
where

import           Data.Text         (Text, unpack)
import           Formatting        ((%), stext, format)
import qualified Network.Mail.SMTP as Smtp
import qualified Network.Mail.Mime as Mime
import           Network.Socket    (PortNumber)

import qualified Config            as Cfg
import           Model.CoreTypes   (Email)


sendMail :: Cfg.SmtpConfig -> Mime.Mail -> IO ()
sendMail cfg = Smtp.sendMailWithLogin' host port user password
  where host     = unpack $ Cfg.host cfg
        port     = fromInteger $ Cfg.smtpPort cfg :: PortNumber
        user     = unpack $ Cfg.username cfg
        password = unpack $ Cfg.password cfg

sendBuiltMail :: Cfg.FlatrCfg -> Email
              -> (Cfg.SmtpConfig -> Email -> Mime.Mail)
              -> IO ()
sendBuiltMail cfg emailAddress builder
  | (Just smtpConfig) <- Cfg.smtpConfig cfg =
      sendMail smtpConfig $ builder smtpConfig emailAddress
  | otherwise = putStrLn "No smtp config was set so no verification email was sent"

buildMail :: Text
          -> Maybe Text
          -> Mime.Part
          -> Cfg.SmtpConfig
          -> Text
          -> Mime.Mail
buildMail subject username body smtpConfig toAddress =
  Smtp.simpleMail from to [] [] subject [body]
  where from = Smtp.Address (Just "Flatr Admin") (Cfg.sender smtpConfig)
        to = [Smtp.Address username toAddress]

buildVerificationMail :: Text -> Text -> Cfg.SmtpConfig -> Email -> Mime.Mail
buildVerificationMail code username =
  buildMail subject (Just username) body
  where subject = "Flatr Verification"
        body = Smtp.plainTextPart $ format
                   ( "Please confirm your email adress by visiting this URL"
                     % stext % "/#signup?code=" % stext % "&serverUrl=" % stext
                   ) frontendUrl code serverUrl
        serverUrl = "https://localhost:8080" :: Text
        frontendUrl = "https://localhost:8000" :: Text

buildInvitationMail :: Text -> Cfg.SmtpConfig -> Email -> Mime.Mail
buildInvitationMail code =
  buildMail subject Nothing body
  where subject = "Flatr Invitation"
        body = Smtp.plainTextPart $ format
                   ( "You are invited to join Flatr, to accept visit this URL"
                     % stext % "/#signup?code=" % stext % "&serverUrl=" % stext
                   ) frontendUrl code serverUrl
        serverUrl = "https://localhost:8080" :: Text
        frontendUrl = "https://localhost:8000" :: Text
