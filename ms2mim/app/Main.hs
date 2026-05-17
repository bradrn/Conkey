{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Text.Encoding (decodeUtf16LE, decodeUtf16BE, decodeUtf8)
import System.Environment (getArgs)

import qualified Data.ByteString as B
import qualified Data.Text.IO.Utf8 as TIO8

import qualified Convert.Intermediate as Int
import Convert.MS2MIM (convertWithMetadata)
import Convert.XCompose (XComposeConvertMode(..), convertXCompose)
import Intermediate.Print (Mode(..), renderIntermediate)
import MSKLC.Keyboard (Keyboard(Keyboard))
import MSKLC.Parser (parse)
import M17N.Print (render)
import XCompose.Print (XComposePrintMode(..), renderXCompose)

main ::  IO ()
main = do
    (kbdPath : kbdOutPath : format : rest) <- getArgs
    kbdB <- B.readFile kbdPath
    let kbdText = case B.splitAt 2 kbdB of
            -- attempt BOM detection
            ("\xFF\xFE", kbdB') -> decodeUtf16LE kbdB'
            ("\xFE\xFF", kbdB') -> decodeUtf16BE kbdB'
            _ -> decodeUtf8 kbdB
    case parse kbdText of
        Left err -> putStrLn $ "Error: Keyboard cannot be parsed. Parse error was:\n" ++ err
        Right kbdMS@(Keyboard metadata _) ->
            let kbdInt = Int.convert kbdMS
                kbdOut = case format of
                    "--mim" -> Just $ render $ convertWithMetadata metadata kbdInt
                    "--xc" ->
                        let (convertMode, printMode) = case rest of
                                ("--filter":"--multikey":_) -> (FilterSingleChars, WithMultiKey)
                                ("--filter":_) -> (FilterSingleChars, WithModifiers)
                                ("--multikey":_) -> (NoFilter, WithMultiKey)
                                _ -> (NoFilter, WithModifiers)
                        in Just $ renderXCompose printMode $ convertXCompose convertMode kbdInt
                    "--int" ->
                        let mode = case rest of
                                ("--osx":_) -> OSX
                                _ -> Unmodified
                        in Just $ renderIntermediate mode kbdInt
                    _ -> Nothing
            in case kbdOut of
                Just kbdOut' -> do
                    TIO8.writeFile kbdOutPath kbdOut'
                    putStrLn "Wrote keyboard successfully."
                Nothing -> putStrLn "Parse error in command-line options; could not write keyboard."
