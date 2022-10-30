module Convert.XCompose (XComposeConvertMode(..), convertXCompose) where

import Data.Text (pack)

import Convert.Intermediate (Intermediate)
import qualified MSKLC.Keyboard as MS
import XCompose.Keyboard

data XComposeConvertMode = NoFilter | FilterSingleChars
    deriving (Show)
    
convertXCompose :: XComposeConvertMode -> Intermediate -> XCompose
convertXCompose mode = fmap $ convertSequence mode

convertSequence
    :: XComposeConvertMode
    -> ([(MS.ShiftState, MS.VkCode)], Char)
    -> Sequence
convertSequence mode i@(es, r) =
    case result of
        Just s -> s
        Nothing ->   -- include comments in output for every filtered line
            Comment $ pack $ show i
  where
    result =
        let s = flip Sequence (StringResult [r]) <$> traverse convertEvent es
        in case mode of
            FilterSingleChars
                | length es == 1 
                , fst (head es) `elem` [MS.NoMod, MS.Shft]
                -> Nothing
            _ -> s

convertEvent :: (MS.ShiftState, MS.VkCode) -> Maybe Event
convertEvent (ss, vk) = Event (convertShiftState explicitShift ss) <$> convertVkCode hasShift vk
  where
    explicitShift = vk == MS.VkSpace

    hasShift = case ss of
        MS.NoMod -> False
        MS.Shft -> True
        MS.Ctrl -> False
        MS.ShftCtrl -> True
        MS.CtrlAlt -> False
        MS.ShftCtrlAlt -> True

convertShiftState :: Bool -> MS.ShiftState -> Maybe Modifiers
convertShiftState _ MS.NoMod = Nothing
convertShiftState _ MS.Ctrl = Just $ Modifiers True [(Normal, Ctrl)]
convertShiftState _ MS.CtrlAlt = Just $ Modifiers True [(Normal, Ctrl), (Normal, Alt)]
convertShiftState False MS.Shft = Nothing
convertShiftState False MS.ShftCtrl = Just $ Modifiers True [(Normal, Ctrl)]
convertShiftState False MS.ShftCtrlAlt = Just $ Modifiers True [(Normal, Ctrl), (Normal, Alt)]
convertShiftState True MS.Shft = Just $ Modifiers True [(Normal, Shift)]
convertShiftState True MS.ShftCtrl = Just $ Modifiers True [(Normal, Shift), (Normal, Ctrl)]
convertShiftState True MS.ShftCtrlAlt = Just $ Modifiers True [(Normal, Shift), (Normal, Ctrl), (Normal, Alt)]

convertVkCode :: Bool -> MS.VkCode -> Maybe Keysym
convertVkCode False MS.Oem1 = Just XK_semicolon
convertVkCode False MS.Oem2 = Just XK_slash
convertVkCode False MS.Oem3 = Just XK_grave
convertVkCode False MS.Oem4 = Just XK_bracketleft
convertVkCode False MS.Oem5 = Just XK_backslash
convertVkCode False MS.Oem6 = Just XK_bracketright
convertVkCode False MS.Oem7 = Just XK_apostrophe
convertVkCode False MS.OemPlus = Just XK_equal
convertVkCode False MS.OemComma = Just XK_comma
convertVkCode False MS.OemMinus = Just XK_minus
convertVkCode False MS.OemPeriod = Just XK_period
convertVkCode False MS.Vk0 = Just XK_0
convertVkCode False MS.Vk1 = Just XK_1
convertVkCode False MS.Vk2 = Just XK_2
convertVkCode False MS.Vk3 = Just XK_3
convertVkCode False MS.Vk4 = Just XK_4
convertVkCode False MS.Vk5 = Just XK_5
convertVkCode False MS.Vk6 = Just XK_6
convertVkCode False MS.Vk7 = Just XK_7
convertVkCode False MS.Vk8 = Just XK_8
convertVkCode False MS.Vk9 = Just XK_9
convertVkCode False MS.VkSpace = Just XK_space
convertVkCode False (MS.Vk 'A') = Just XK_a
convertVkCode False (MS.Vk 'B') = Just XK_b
convertVkCode False (MS.Vk 'C') = Just XK_c
convertVkCode False (MS.Vk 'D') = Just XK_d
convertVkCode False (MS.Vk 'E') = Just XK_e
convertVkCode False (MS.Vk 'F') = Just XK_f
convertVkCode False (MS.Vk 'G') = Just XK_g
convertVkCode False (MS.Vk 'H') = Just XK_h
convertVkCode False (MS.Vk 'I') = Just XK_i
convertVkCode False (MS.Vk 'J') = Just XK_j
convertVkCode False (MS.Vk 'K') = Just XK_k
convertVkCode False (MS.Vk 'L') = Just XK_l
convertVkCode False (MS.Vk 'M') = Just XK_m
convertVkCode False (MS.Vk 'N') = Just XK_n
convertVkCode False (MS.Vk 'O') = Just XK_o
convertVkCode False (MS.Vk 'P') = Just XK_p
convertVkCode False (MS.Vk 'Q') = Just XK_q
convertVkCode False (MS.Vk 'R') = Just XK_r
convertVkCode False (MS.Vk 'S') = Just XK_s
convertVkCode False (MS.Vk 'T') = Just XK_t
convertVkCode False (MS.Vk 'U') = Just XK_u
convertVkCode False (MS.Vk 'V') = Just XK_v
convertVkCode False (MS.Vk 'W') = Just XK_w
convertVkCode False (MS.Vk 'X') = Just XK_x
convertVkCode False (MS.Vk 'Y') = Just XK_y
convertVkCode False (MS.Vk 'Z') = Just XK_z
convertVkCode True MS.Oem1 = Just XK_colon
convertVkCode True MS.Oem2 = Just XK_question
convertVkCode True MS.Oem3 = Just XK_asciitilde
convertVkCode True MS.Oem4 = Just XK_braceleft
convertVkCode True MS.Oem5 = Just XK_bar
convertVkCode True MS.Oem6 = Just XK_braceright
convertVkCode True MS.Oem7 = Just XK_quotedbl
convertVkCode True MS.Oem102 = Just XK_bar
convertVkCode True MS.OemPlus = Just XK_plus
convertVkCode True MS.OemComma = Just XK_less
convertVkCode True MS.OemMinus = Just XK_underscore
convertVkCode True MS.OemPeriod = Just XK_greater
convertVkCode True MS.Vk0 = Just XK_parenright
convertVkCode True MS.Vk1 = Just XK_exclam
convertVkCode True MS.Vk2 = Just XK_at
convertVkCode True MS.Vk3 = Just XK_numbersign
convertVkCode True MS.Vk4 = Just XK_dollar
convertVkCode True MS.Vk5 = Just XK_percent
convertVkCode True MS.Vk6 = Just XK_asciicircum
convertVkCode True MS.Vk7 = Just XK_ampersand
convertVkCode True MS.Vk8 = Just XK_asterisk
convertVkCode True MS.Vk9 = Just XK_parenleft
convertVkCode True MS.VkSpace = Just XK_space
convertVkCode True (MS.Vk 'A') = Just XK_A
convertVkCode True (MS.Vk 'B') = Just XK_B
convertVkCode True (MS.Vk 'C') = Just XK_C
convertVkCode True (MS.Vk 'D') = Just XK_D
convertVkCode True (MS.Vk 'E') = Just XK_E
convertVkCode True (MS.Vk 'F') = Just XK_F
convertVkCode True (MS.Vk 'G') = Just XK_G
convertVkCode True (MS.Vk 'H') = Just XK_H
convertVkCode True (MS.Vk 'I') = Just XK_I
convertVkCode True (MS.Vk 'J') = Just XK_J
convertVkCode True (MS.Vk 'K') = Just XK_K
convertVkCode True (MS.Vk 'L') = Just XK_L
convertVkCode True (MS.Vk 'M') = Just XK_M
convertVkCode True (MS.Vk 'N') = Just XK_N
convertVkCode True (MS.Vk 'O') = Just XK_O
convertVkCode True (MS.Vk 'P') = Just XK_P
convertVkCode True (MS.Vk 'Q') = Just XK_Q
convertVkCode True (MS.Vk 'R') = Just XK_R
convertVkCode True (MS.Vk 'S') = Just XK_S
convertVkCode True (MS.Vk 'T') = Just XK_T
convertVkCode True (MS.Vk 'U') = Just XK_U
convertVkCode True (MS.Vk 'V') = Just XK_V
convertVkCode True (MS.Vk 'W') = Just XK_W
convertVkCode True (MS.Vk 'X') = Just XK_X
convertVkCode True (MS.Vk 'Y') = Just XK_Y
convertVkCode True (MS.Vk 'Z') = Just XK_Z
-- Don't know how to convert these ones
convertVkCode _ MS.Oem102 = Nothing
convertVkCode _ MS.VkDecimal = Nothing
convertVkCode _ (MS.Vk _) = Nothing
