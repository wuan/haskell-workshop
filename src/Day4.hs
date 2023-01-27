{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day4 (day4, input4, buildSource, hashText) where

----- Day 4: The Ideal Stocking Stuffer ---
--Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts for all the economically forward-thinking little girls and boys.
--
--To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes. The input to the MD5 hash is some secret key (your puzzle input, given below) followed by a number in decimal. To mine AdventCoins, you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...) that produces such a hash.
--
--For example:
--
--If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest such number to do so.
--If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....
--Your puzzle input is ckczppom.

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Bifunctor as BiFunc
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as DText
import qualified Data.Text.Encoding as DTextEncoding

type Text = DText.Text

input4 :: Text
input4 = "ckczppom"

encodeHex :: Char8.ByteString -> Text
encodeHex input =
    DTextEncoding.decodeUtf8 (Base16.encode input)

hashText :: Text -> Text
hashText input =
    encodeHex $ MD5.hash $ DTextEncoding.encodeUtf8 input

buildSource :: Text -> Int -> (Text, Int)
buildSource input number = (input <> DText.pack (show number), number)

firstMatch :: Text -> Text -> (Text, Int)
firstMatch prefix input =
    head $
        take 1 $
            filter ((prefix `DText.isPrefixOf`) . fst) $
                map (BiFunc.first hashText . buildSource input) [1 ..]

day4 :: IO ()
day4 = do
    print (firstMatch "00000" input4)
    print (firstMatch "000000" input4)
