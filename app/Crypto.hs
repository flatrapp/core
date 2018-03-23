module Crypto
    ( hashPassword
    , verificationCode
    , invitationCode
    , tokenId
    , passwordSalt
    )
where

import           Crypto.Error              (throwCryptoError)
import qualified Crypto.KDF.Argon2         as Ar2
import           Crypto.Random             (getRandomBytes)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base16    as B16
import           Data.Text                 (Text)
import qualified Data.Text.Encoding        as E
import           Data.Word8                (Word8)
import           System.Random             (StdGen, next)

invitationCode :: IO Text
invitationCode = makeHex <$> (getRandomBytes 10)

verificationCode :: IO Text
verificationCode = makeHex <$> (getRandomBytes 10)

tokenId :: IO Text
tokenId = makeHex <$> (getRandomBytes 10)

passwordSalt :: StdGen -> Text
passwordSalt = randomText 512

randomText :: Int -> StdGen -> Text
randomText i s = makeHex $ randomBS i s  -- TODO do pointfree

-- TODO do we want to use this function???
randomBytes :: Int -> StdGen -> [Word8]
randomBytes 0 _ = []
randomBytes ct g =
    let (value, nextG) = next g
    in fromIntegral value:randomBytes (ct - 1) nextG

randomBS :: Int -> StdGen -> BS.ByteString
randomBS len g =
    BS.pack $ randomBytes len g

makeHex :: BS.ByteString -> Text
makeHex = E.decodeUtf8 . B16.encode

decodeHex :: Text -> BS.ByteString
decodeHex = fst . B16.decode . E.encodeUtf8

-- TODO save the salt as bytestring in database
hashPassword :: Text -> Text -> Text
hashPassword password salt = makeHex . throwCryptoError
    $ Ar2.hash Ar2.defaultOptions (E.encodeUtf8 password) (decodeHex salt) 1024
    -- throwCryptoError can in theory throw, crashing the program.
    -- But this will happen only if salt length or output size are invalid.
    -- As this will never be the case (as long as we provide acceptable salts),
    -- this will never happen.
