{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day4 (day4) where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Bifunctor as BiFunc
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as DText
import qualified Data.Text.Encoding as DTextEncoding

type Text = DText.Text

newtype Hash = Hash {getText :: Text} deriving (Eq, Show)

input4 :: Text
input4 = "ckczppom"

encodeHex :: Char8.ByteString -> Text
encodeHex input =
    DTextEncoding.decodeUtf8 (Base16.encode input)

hashText :: Text -> Hash
hashText input =
    Hash $ encodeHex $ MD5.hash $ DTextEncoding.encodeUtf8 input

buildSource :: Text -> Int -> (Text, Int)
buildSource input number = (input <> DText.pack (show number), number)

firstMatch :: Text -> Text -> (Hash, Int)
firstMatch prefix input =
    head $
        take 1 $
            filter ((prefix `DText.isPrefixOf`) . getText . fst) $
                map (BiFunc.first hashText . buildSource input) [1 ..]

day4 :: IO ()
day4 = do
    print "day 4:"
    print (firstMatch "00000" input4)
    print (firstMatch "000000" input4)
