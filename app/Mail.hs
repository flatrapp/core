{-# LANGUAGE OverloadedStrings     #-}

module Mail
    ( sendVerificationMail
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

sendVerificationMail :: Cfg.FlatrCfg -> Email -> Text -> IO ()
sendVerificationMail cfg emailAddress code
  | (Just smtpConfig) <- Cfg.smtpConfig cfg =
      sendMail smtpConfig $ buildVerificationMail smtpConfig emailAddress code
  | otherwise = putStrLn "No smtp config was set so no verification email was sent"

buildVerificationMail :: Cfg.SmtpConfig -> Email -> Text -> Mime.Mail
buildVerificationMail smtpConfig toAddress code =
  Smtp.simpleMail from to [] [] subject [body]
  where from = Smtp.Address (Just "Flatr Admin") (Cfg.sender smtpConfig)
        to = [Smtp.Address (Just username) toAddress]
        username = "TODO FIXME"
        subject = "Flatr Verification"
        serverUrl = "https://localhost:8080" :: Text
        frontendUrl = "https://localhost:8000" :: Text
        body = Smtp.plainTextPart $ format
                 ( "Please confirm your email adress by visiting that URL"
                   % stext % "/#signup?code=" % stext % "&serverUrl=" % stext
                 ) frontendUrl code serverUrl
