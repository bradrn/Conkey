{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module XCompose.Print (XComposePrintMode(..), renderXCompose) where

import Data.Functor ((<&>))

import Data.Text (Text)
import qualified Data.Text as T

import XCompose.Keyboard

data XComposePrintMode = WithModifiers | WithMultiKey
    deriving (Show)

data Commented t = NoComment t | Commented t
    deriving (Show, Functor)

instance Applicative Commented where
    pure = NoComment

    (NoComment f) <*> (NoComment x) = NoComment (f x)
    (NoComment f) <*> (Commented x) = Commented (f x)
    (Commented f) <*> (NoComment x) = Commented (f x)
    (Commented f) <*> (Commented x) = Commented (f x)

applyComment :: Commented Text -> Text
applyComment (NoComment t) = t
applyComment (Commented t) = "# " <> t

renderXCompose :: XComposePrintMode -> XCompose -> Text
renderXCompose mode = T.unlines . fmap (applyComment . renderSequence mode)

renderSequence :: XComposePrintMode -> Sequence -> Commented Text
renderSequence mode (Sequence es r) =
    let es' = traverse (renderEvent mode) es
    in es' <&> \es'' -> T.unwords es'' <> " : " <> renderResult r
renderSequence _ (Comment t) = Commented t

renderEvent :: XComposePrintMode -> Event -> Commented Text
renderEvent mode (Event ms k) =
    let mods = maybe (NoComment "") (renderModifiers mode) ms
    in mods <&> (<> (" " <> renderKeysym k))

renderModifiers :: XComposePrintMode -> Modifiers -> Commented Text
renderModifiers WithMultiKey ms@(Modifiers _ ms')
    | Alt `elem` (snd <$> ms') = NoComment "<Multi_key>"
    | otherwise = Commented $ renderModifiers' ms
renderModifiers WithModifiers ms =
    NoComment $ renderModifiers' ms

renderModifiers' :: Modifiers -> Text
renderModifiers' (Modifiers exact ms) =
    (if exact then "! " else "") <> T.concat (renderModifier' <$> ms)
  where
    renderModifier' (Normal, m) = renderModifier m
    renderModifier' (Negated, m) = "~ " <> renderModifier m

renderModifier :: Modifier -> Text
renderModifier Ctrl  = "Ctrl"
renderModifier Lock  = "Lock"
renderModifier Caps  = "Caps"
renderModifier Shift = "Shift"
renderModifier Alt   = "Alt"
renderModifier Meta  = "Meta"

renderResult :: Result -> Text
renderResult (StringResult s) = renderString s
renderResult (KeysymResult k) = renderKeysym k
renderResult (Both s k) = renderString s <> " " <> renderKeysym k

-- This makes the assumption that the input contains no characters
-- needing to be escaped, which will usually be the case. It assumes
-- this mostly because I can't get the escape characters to work on my
-- system...
renderString :: String -> Text
renderString s = "\"" <> T.pack s <> "\""

renderKeysym :: Keysym -> Text
renderKeysym = \case
    XK_VoidSymbol -> "<VoidSymbol>"
    XK_Multi_key -> "<Multi_key>"
    XK_space -> "<space>"
    XK_exclam -> "<exclam>"
    XK_quotedbl -> "<quotedbl>"
    XK_numbersign -> "<numbersign>"
    XK_dollar -> "<dollar>"
    XK_percent -> "<percent>"
    XK_ampersand -> "<ampersand>"
    XK_apostrophe -> "<apostrophe>"
    XK_quoteright -> "<quoteright>"
    XK_parenleft -> "<parenleft>"
    XK_parenright -> "<parenright>"
    XK_asterisk -> "<asterisk>"
    XK_plus -> "<plus>"
    XK_comma -> "<comma>"
    XK_minus -> "<minus>"
    XK_period -> "<period>"
    XK_slash -> "<slash>"
    XK_0 -> "<0>"
    XK_1 -> "<1>"
    XK_2 -> "<2>"
    XK_3 -> "<3>"
    XK_4 -> "<4>"
    XK_5 -> "<5>"
    XK_6 -> "<6>"
    XK_7 -> "<7>"
    XK_8 -> "<8>"
    XK_9 -> "<9>"
    XK_colon -> "<colon>"
    XK_semicolon -> "<semicolon>"
    XK_less -> "<less>"
    XK_equal -> "<equal>"
    XK_greater -> "<greater>"
    XK_question -> "<question>"
    XK_at -> "<at>"
    XK_A -> "<A>"
    XK_B -> "<B>"
    XK_C -> "<C>"
    XK_D -> "<D>"
    XK_E -> "<E>"
    XK_F -> "<F>"
    XK_G -> "<G>"
    XK_H -> "<H>"
    XK_I -> "<I>"
    XK_J -> "<J>"
    XK_K -> "<K>"
    XK_L -> "<L>"
    XK_M -> "<M>"
    XK_N -> "<N>"
    XK_O -> "<O>"
    XK_P -> "<P>"
    XK_Q -> "<Q>"
    XK_R -> "<R>"
    XK_S -> "<S>"
    XK_T -> "<T>"
    XK_U -> "<U>"
    XK_V -> "<V>"
    XK_W -> "<W>"
    XK_X -> "<X>"
    XK_Y -> "<Y>"
    XK_Z -> "<Z>"
    XK_bracketleft -> "<bracketleft>"
    XK_backslash -> "<backslash>"
    XK_bracketright -> "<bracketright>"
    XK_asciicircum -> "<asciicircum>"
    XK_underscore -> "<underscore>"
    XK_grave -> "<grave>"
    XK_quoteleft -> "<quoteleft>"
    XK_a -> "<a>"
    XK_b -> "<b>"
    XK_c -> "<c>"
    XK_d -> "<d>"
    XK_e -> "<e>"
    XK_f -> "<f>"
    XK_g -> "<g>"
    XK_h -> "<h>"
    XK_i -> "<i>"
    XK_j -> "<j>"
    XK_k -> "<k>"
    XK_l -> "<l>"
    XK_m -> "<m>"
    XK_n -> "<n>"
    XK_o -> "<o>"
    XK_p -> "<p>"
    XK_q -> "<q>"
    XK_r -> "<r>"
    XK_s -> "<s>"
    XK_t -> "<t>"
    XK_u -> "<u>"
    XK_v -> "<v>"
    XK_w -> "<w>"
    XK_x -> "<x>"
    XK_y -> "<y>"
    XK_z -> "<z>"
    XK_braceleft -> "<braceleft>"
    XK_bar -> "<bar>"
    XK_braceright -> "<braceright>"
    XK_asciitilde -> "<asciitilde>"
