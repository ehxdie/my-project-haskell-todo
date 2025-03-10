module Config.PasswordHashing where

import Crypto.BCrypt (hashPasswordUsingPolicy, validatePassword, slowerBcryptHashingPolicy)
import Data.ByteString.Char8 (pack, unpack)

hashPassword :: String -> IO String
hashPassword password = do
    hashed <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (pack password)
    return $ maybe "" unpack hashed

verifyPassword :: String -> String -> Bool
verifyPassword plain hashed = validatePassword (pack hashed) (pack plain) == True
