module XCompose.Keyboard where

import Data.Text (Text)

type XCompose = [Sequence]

data Sequence = Sequence [Event] Result | Comment Text
    deriving (Show)
    
data Event = Event (Maybe Modifiers) Keysym
    deriving (Show)
    
data Modifiers = Modifiers Bool [(Negation, Modifier)]
    deriving (Show)
    
data Negation = Normal | Negated
    deriving (Show)
    
data Modifier = Ctrl | Lock | Caps | Shift | Alt | Meta
    deriving (Eq, Show)
    
data Result = StringResult String | KeysymResult Keysym | Both String Keysym
    deriving (Show)
    
-- For simplicity, this only attempts to represent a very limited
-- subset of keysyms - there are ~2000 other ones!
data Keysym =
    XK_VoidSymbol | XK_Multi_key
    | XK_space | XK_exclam | XK_quotedbl | XK_numbersign | XK_dollar | XK_percent | XK_ampersand
    | XK_apostrophe | XK_quoteright | XK_parenleft | XK_parenright | XK_asterisk | XK_plus | XK_comma | XK_minus
    | XK_period | XK_slash | XK_0 | XK_1 | XK_2 | XK_3 | XK_4 | XK_5 | XK_6 | XK_7 | XK_8 | XK_9
    | XK_colon | XK_semicolon | XK_less | XK_equal | XK_greater | XK_question | XK_at
    | XK_A | XK_B | XK_C | XK_D | XK_E | XK_F | XK_G | XK_H | XK_I | XK_J | XK_K | XK_L | XK_M
    | XK_N | XK_O | XK_P | XK_Q | XK_R | XK_S | XK_T | XK_U | XK_V | XK_W | XK_X | XK_Y | XK_Z
    | XK_bracketleft | XK_backslash | XK_bracketright | XK_asciicircum | XK_underscore | XK_grave | XK_quoteleft
    | XK_a | XK_b | XK_c | XK_d | XK_e | XK_f | XK_g | XK_h | XK_i | XK_j | XK_k | XK_l | XK_m
    | XK_n | XK_o | XK_p | XK_q | XK_r | XK_s | XK_t | XK_u | XK_v | XK_w | XK_x | XK_y | XK_z
    | XK_braceleft | XK_bar | XK_braceright | XK_asciitilde
    deriving (Show)
    
