{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Convert.MS2MIM where

import Data.Char (toLower, toUpper, ord)
import Data.Maybe (mapMaybe)

import Data.Text (Text)
import qualified Data.Text as T

import qualified MSKLC.Keyboard as MS
import qualified M17N.Keyboard as Mim
import Convert.Intermediate (Intermediate)

convertWithMetadata :: MS.Metadata -> Intermediate -> Mim.InputMethod
convertWithMetadata MS.Metadata{..} im = Mim.InputMethod
    { imDeclaration = Just Mim.ImDeclaration
        { dclLanguage = "t"  -- don't bother converting language
        , dclName = kbdName
        , dclExtraId = Nothing
        , dclVersion = Nothing
        }
    , imDescription = Just $ Mim.ConstMText $ Mim.MText kbdDescription
    , imTitle = Just $ Mim.MText kbdName
    , imVariableList = Nothing
    , imCommandList = Nothing
    , imModuleList = Nothing
    , imMacroList = Nothing
    , imMapList = Just $ Mim.MapList Nothing [("simple", getRules im)]
    , imStateList = Just $ Mim.StateList Nothing
        [Mim.State "init" Nothing [Mim.MapBranch "simple" []]] Nothing
    }

getRules :: Intermediate -> [Mim.Rule]
getRules = fmap getRule . filter filterSeqs
  where
    getRule (ks, c) = Mim.Rule
        (Mim.KeySeq $ mapMaybe (fmap Left . uncurry composeFromVk) ks) 
        [Mim.ActInsert $ Mim.InsInt $ ord c]

    -- Remove unwanted key sequences
    filterSeqs ([(MS.Ctrl, MS.Oem4)], _) = False
    filterSeqs _ = True

composeFromVk :: MS.ShiftState -> MS.VkCode -> Maybe Mim.Symbol
composeFromVk ss vk =
    let explicitShift = (vk == MS.VkSpace)
    in (getPrefix explicitShift ss <>) <$> toMimKey (hasShift ss) vk
  where
    getPrefix _     MS.NoMod = ""
    getPrefix True  MS.Shft = "S-"
    getPrefix False MS.Shft = ""
    getPrefix _     MS.Ctrl = "C-"
    getPrefix True  MS.ShftCtrl = "S-C-"
    getPrefix False MS.ShftCtrl = "C-"
    getPrefix _     MS.CtrlAlt = "G-"
    getPrefix True  MS.ShftCtrlAlt = "S-G-"
    getPrefix False MS.ShftCtrlAlt = "G-"

    hasShift = (`elem` [MS.Shft, MS.ShftCtrl, MS.ShftCtrlAlt])

toMimKey :: Bool -> MS.VkCode -> Maybe Text
toMimKey False MS.Oem1 = Just "\\;"
toMimKey False MS.Oem2 = Just "/"
toMimKey False MS.Oem3 = Just "`"
toMimKey False MS.Oem4 = Just "["
toMimKey False MS.Oem5 = Just "\\\\"
toMimKey False MS.Oem6 = Just "]"
toMimKey False MS.Oem7 = Just "'"
toMimKey False MS.OemPlus = Just "="
toMimKey False MS.OemComma = Just ","
toMimKey False MS.OemMinus = Just "-"
toMimKey False MS.OemPeriod = Just "."
-- Need to escape numbers so they don't get interpreted
-- as ASCII characters 0,1,2,etc.
toMimKey False MS.Vk0 = Just "\\0"
toMimKey False MS.Vk1 = Just "\\1"
toMimKey False MS.Vk2 = Just "\\2"
toMimKey False MS.Vk3 = Just "\\3"
toMimKey False MS.Vk4 = Just "\\4"
toMimKey False MS.Vk5 = Just "\\5"
toMimKey False MS.Vk6 = Just "\\6"
toMimKey False MS.Vk7 = Just "\\7"
toMimKey False MS.Vk8 = Just "\\8"
toMimKey False MS.Vk9 = Just "\\9"
toMimKey False MS.VkSpace = Just "\\ "
toMimKey False (MS.Vk c) = Just $ T.singleton $ toLower c
toMimKey True MS.Oem1 = Just ":"
toMimKey True MS.Oem2 = Just "\\?"
toMimKey True MS.Oem3 = Just "~"
toMimKey True MS.Oem4 = Just "{"
toMimKey True MS.Oem5 = Just "|"
toMimKey True MS.Oem6 = Just "}"
toMimKey True MS.Oem7 = Just "\\\""
toMimKey True MS.Oem102 = Just "|"
toMimKey True MS.OemPlus = Just "+"
toMimKey True MS.OemComma = Just "<"
toMimKey True MS.OemMinus = Just "_"
toMimKey True MS.OemPeriod = Just ">"
toMimKey True MS.Vk0 = Just "\\)"
toMimKey True MS.Vk1 = Just "!"
toMimKey True MS.Vk2 = Just "@"
toMimKey True MS.Vk3 = Just "#"
toMimKey True MS.Vk4 = Just "$"
toMimKey True MS.Vk5 = Just "%"
toMimKey True MS.Vk6 = Just "^"
toMimKey True MS.Vk7 = Just "&"
toMimKey True MS.Vk8 = Just "*"
toMimKey True MS.Vk9 = Just "\\("
toMimKey True MS.VkSpace = Just "\\ "
toMimKey True (MS.Vk c) = Just $ T.singleton $ toUpper c
-- Don't know how to convert these ones
toMimKey _ MS.Oem102 = Nothing
toMimKey _ MS.VkDecimal = Nothing
