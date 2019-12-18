module Main where

import System.Environment (getArgs)
import System.IO (openFile, hSetEncoding, utf16, IOMode(ReadMode))

import qualified Data.Text.IO as TIO

import Convert.MS2MIM (convert)
import MSKLC.Parser (parse)
import M17N.Print (render)

main :: IO ()
main = do
    [kbdPath, kbdOutPath] <- getArgs
    kbdH <- openFile kbdPath ReadMode
    hSetEncoding kbdH utf16
    kbdText <- TIO.hGetContents kbdH
    case parse kbdText of
        Left err -> putStrLn $ "Error: Keyboard cannot be parsed. Parse error was:\n" ++ err
        Right kbdMS ->
            let kbdMim = convert kbdMS
                kbdOut = render kbdMim
            in TIO.writeFile kbdOutPath kbdOut >> putStrLn "Wrote keyboard successfully."
