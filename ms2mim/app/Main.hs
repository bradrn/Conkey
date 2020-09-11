module Main where

import System.Environment (getArgs)
import System.IO (openFile, hSetEncoding, utf16, IOMode(ReadMode))

import qualified Data.Text.IO as TIO

import qualified Convert.Intermediate as Int
import Convert.MS2MIM (convertWithMetadata)
import Intermediate.Print (Mode(..), renderIntermediate)
import MSKLC.Keyboard (Keyboard(Keyboard))
import MSKLC.Parser (parse)
import M17N.Print (render)

main :: IO ()
main = do
    (kbdPath : kbdOutPath : format : rest) <- getArgs
    let mode = case rest of
            ("--osx":_) -> OSX
            _ -> Unmodified
    kbdH <- openFile kbdPath ReadMode
    hSetEncoding kbdH utf16
    kbdText <- TIO.hGetContents kbdH
    case parse kbdText of
        Left err -> putStrLn $ "Error: Keyboard cannot be parsed. Parse error was:\n" ++ err
        Right kbdMS@(Keyboard metadata _) ->
            let kbdInt = Int.convert kbdMS
                kbdOut = case format of
                    "--mim" -> Just $ render $ convertWithMetadata metadata kbdInt
                    "--int" -> Just $ renderIntermediate mode kbdInt
                    _ -> Nothing
            in case kbdOut of
                Just kbdOut' -> do
                    TIO.writeFile kbdOutPath kbdOut'
                    putStrLn "Wrote keyboard successfully."
                Nothing -> putStrLn "Parse error in command-line options; could not write keyboard."
