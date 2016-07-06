module Email where

import Network.HaskellNet.SMTP.SSL

import Network.HaskellNet.Auth
import qualified Data.Text.Lazy as T

serverHostName = "smtp.gmail.com"
user = "myserverwatchdog@gmail.com"
pass = "qV78wbj6wRYL3UXQ"



settings = defaultSettingsSMTPSSL { sslPort = 587 }


main = doSMTPSSLWithSettings serverHostName settings $ \conn -> do
   authSucceed <- authenticate PLAIN user pass conn
   if authSucceed
      then sendPlainTextMail "runesvend@gmail.com" user "test subject 47"
         (T.pack "Hello! This is the mail body!\n\n-Rune") conn
      else print "Authentication failed."
