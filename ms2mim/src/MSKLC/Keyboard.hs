{-# LANGUAGE BangPatterns #-}

module MSKLC.Keyboard where

import Data.Text (Text)
import Data.Map.Strict (Map)

data Keyboard = Keyboard Metadata Layout
    deriving (Show)

data Metadata = Metadata
    { kbdName          :: !Text
    , kbdDescription   :: !Text
    , kbdCopyright     :: !Text
    , kbdCompany       :: !Text
    , kbdLocaleName    :: !Text
    , kbdLocaleId      :: !Text
    , kbdVersion       :: !Text
    , kbdLanguageNames :: !Text
    } deriving (Show)

type Layout = Map VkCode CharAssign

data VkCode = Oem1 | Oem2 | Oem3 | Oem4 | Oem5 | Oem6 | Oem7 |  Oem102
            | OemPlus | OemComma | OemMinus | OemPeriod
            | Vk0 | Vk1 | Vk2 | Vk3 | Vk4 | Vk5 | Vk6 | Vk7 | Vk8 | Vk9
            | VkSpace | VkDecimal | Vk Char
    deriving (Eq, Ord, Show)

type CharAssign = Map ShiftState Entry

data ShiftState = NoMod | Shft | Ctrl | ShftCtrl | CtrlAlt | ShftCtrlAlt
    deriving (Eq, Ord, Show)

data Entry = Unassigned | Entry Char | DeadKey DeadKeyDesc
    deriving (Eq, Show)

data DeadKeyDesc = DeadKeyDesc
    { dkBaseChar :: Char
    , dkMapping  :: Map Char Char
    } deriving (Eq, Show)
