{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main  where

import System.Console.Readline
import Text.Megaparsec.Error

import Parsers (doParse)

main :: IO ()
main = readline "lox> " >>= useLine

useLine :: Maybe String -> IO ()
useLine Nothing = return ()
useLine (Just "exit") = return ()
useLine (Just readLine) =  do
      addHistory readLine
      case doParse readLine of
        Left err -> putStrLn $ errorBundlePretty err
        Right parsed -> print parsed
      main
