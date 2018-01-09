{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad              (when)
import           Data.Char                  (toUpper)
import           System.Environment         (getArgs)

import           Control.Lens               ((&))
import           Data.Aeson                 (Value, eitherDecode)
import           Data.Aeson.Text            (encodeToLazyText)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text.Lazy.IO          as L
import           Network.HTTP.Simple

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 2) $ error "Exactly 2 arguments must be specified"
    let [url, method] = args
    res <- http url method
    let status = getResponseStatusCode res
    when (status /= 200) . putStr $ show status ++ "\t"
    L.putStrLn . encodeToLazyText $ getResponseBody res

http :: String -> String -> IO (Response Value)
http url method = do
    req <- parseRequest url
    case map toUpper method of
        "GET" -> httpJSON $ req & setRequestMethod "GET"
                                & setRequestHeader "Accept" ["application/json"]
        "POST" -> do
            json <- readJSONfromStdin
            httpJSON $ req & setRequestMethod "POST"
                           & setRequestBodyJSON json
                           & setRequestHeader "Accept" ["application/json"]
        _ -> error $ "Unsupported HTTP method: " ++ method

readJSONfromStdin :: IO Value
readJSONfromStdin = do
    input <- BLC.getContents
    return . either error id $ eitherDecode input
