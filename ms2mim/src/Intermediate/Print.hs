{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Intermediate.Print where

import Data.Char (toLower, toUpper)
import Data.Maybe (mapMaybe)

import Data.Text (Text)
import qualified Data.Text as T

import Convert.Intermediate (Intermediate)
import qualified MSKLC.Keyboard as MS

data Mode = Unmodified | OSX
    deriving (Show, Eq)

renderIntermediate :: Mode -> Intermediate -> Text
renderIntermediate mode = T.unlines . mapMaybe mkLine
  where
    mkLine (ks, char2text -> c) = case mapMaybe (uncurry (composeFromVk mode)) ks of
        [] -> Nothing
        ks'
            -- eliminate duplicate lines (e.g. a mapping from "a" to "a")
            | (k':_) <- ks', c == k' -> Nothing
            -- eliminate lines with S-SPC on OSX
            | mode == OSX, (MS.Shft, MS.VkSpace) `elem` ks -> Nothing
            | otherwise -> Just $ T.intercalate " " (c : ks')

    char2text ' '  = "[SPC]"
    char2text '\t' = "[TAB]"
    char2text c    = T.singleton c

composeFromVk :: Mode -> MS.ShiftState -> MS.VkCode -> Maybe Text
composeFromVk mode ss vk =
    let explicitShift = (vk == MS.VkSpace)
    in fmap (getPrefix mode explicitShift ss <>) $ toKeyChar (hasShift ss) vk
  where
    getPrefix _ _     MS.NoMod = ""
    getPrefix _ True  MS.Shft = "S-"
    getPrefix _ False MS.Shft = ""
    getPrefix _ _     MS.Ctrl = "C-"
    getPrefix _ True  MS.ShftCtrl = "S-C-"
    getPrefix _ False MS.ShftCtrl = "C-"
    getPrefix Unmodified _     MS.CtrlAlt = "G-"
    getPrefix Unmodified True  MS.ShftCtrlAlt = "S-G-"
    getPrefix Unmodified False MS.ShftCtrlAlt = "G-"
    getPrefix OSX        _     MS.CtrlAlt = "O-"
    getPrefix OSX        True  MS.ShftCtrlAlt = "S-O-"
    getPrefix OSX        False MS.ShftCtrlAlt = "O-"

    hasShift = (`elem` [MS.Shft, MS.ShftCtrl, MS.ShftCtrlAlt])

toKeyChar :: Bool -> MS.VkCode -> Maybe Text
toKeyChar False MS.Oem1 = Just ";"
toKeyChar False MS.Oem2 = Just "/"
toKeyChar False MS.Oem3 = Just "`"
toKeyChar False MS.Oem4 = Just "["
toKeyChar False MS.Oem5 = Just "\\"
toKeyChar False MS.Oem6 = Just "]"
toKeyChar False MS.Oem7 = Just "'"
toKeyChar False MS.OemPlus = Just "="
toKeyChar False MS.OemComma = Just ","
toKeyChar False MS.OemMinus = Just "-"
toKeyChar False MS.OemPeriod = Just "."
toKeyChar False MS.Vk0 = Just "0"
toKeyChar False MS.Vk1 = Just "1"
toKeyChar False MS.Vk2 = Just "2"
toKeyChar False MS.Vk3 = Just "3"
toKeyChar False MS.Vk4 = Just "4"
toKeyChar False MS.Vk5 = Just "5"
toKeyChar False MS.Vk6 = Just "6"
toKeyChar False MS.Vk7 = Just "7"
toKeyChar False MS.Vk8 = Just "8"
toKeyChar False MS.Vk9 = Just "9"
toKeyChar False MS.VkSpace = Just "[SPC]"
toKeyChar False (MS.Vk c) = Just $ T.singleton $ toLower c
toKeyChar True MS.Oem1 = Just ":"
toKeyChar True MS.Oem2 = Just "?"
toKeyChar True MS.Oem3 = Just "~"
toKeyChar True MS.Oem4 = Just "{"
toKeyChar True MS.Oem5 = Just "|"
toKeyChar True MS.Oem6 = Just "}"
toKeyChar True MS.Oem7 = Just "\""
toKeyChar True MS.Oem102 = Just "|"
toKeyChar True MS.OemPlus = Just "+"
toKeyChar True MS.OemComma = Just "<"
toKeyChar True MS.OemMinus = Just "_"
toKeyChar True MS.OemPeriod = Just ">"
toKeyChar True MS.Vk0 = Just ")"
toKeyChar True MS.Vk1 = Just "!"
toKeyChar True MS.Vk2 = Just "@"
toKeyChar True MS.Vk3 = Just "#"
toKeyChar True MS.Vk4 = Just "$"
toKeyChar True MS.Vk5 = Just "%"
toKeyChar True MS.Vk6 = Just "^"
toKeyChar True MS.Vk7 = Just "&"
toKeyChar True MS.Vk8 = Just "*"
toKeyChar True MS.Vk9 = Just "("
toKeyChar True MS.VkSpace = Just "[SPC]"
toKeyChar True (MS.Vk c) = Just $ T.singleton $ toUpper c
-- Don't know how to convert these ones
toKeyChar _ MS.Oem102 = Nothing
toKeyChar _ MS.VkDecimal = Nothing
