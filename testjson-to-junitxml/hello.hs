{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hello where 

import Data.Aeson
import qualified Data.List as Llist
import Data.Text
import Data.Text.Encoding
import Data.ByteString.Lazy

import Json_generator

main :: IO ()
main =
  let json = Data.ByteString.Lazy.fromStrict (encodeUtf8 (Data.Text.pack (generator 0 20)))
      decoded_json = decode json :: Maybe [Int] in 
    case decoded_json of
      Just ints -> do
        putStrLn ("Decoded: " ++  Llist.intercalate ", " (Llist.map Prelude.show ints))
      Nothing -> do
        putStrLn "Couldn't parse json"
    
