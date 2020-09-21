{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module M17N.Keyboard where

import Data.Text (Text, pack)
import Data.String (IsString)

-- Note: for details on the data types in this module, refer to the
-- M17N documentation at https://www.nongnu.org/m17n/manual-en/m17nDBFormat.html

data InputMethod = InputMethod
    { imDeclaration :: Maybe ImDeclaration
    , imDescription :: Maybe Description
    , imTitle :: Maybe MText
    , imVariableList :: Maybe VariableList
    , imCommandList :: Maybe CommandList
    , imModuleList :: Maybe ModuleList
    , imMacroList :: Maybe MacroList
    , imMapList :: Maybe MapList
    , imStateList :: Maybe StateList
    }

data Description = ConstMText MText | GetText MText | NilDesc

data ImDeclaration = ImDeclaration
    { dclLanguage :: Symbol
    , dclName :: Symbol
    , dclExtraId :: Maybe Symbol
    , dclVersion :: Maybe Text
    }

type VariableList = [VariableDeclaration]

data VariableDeclaration = VariableDeclaration Symbol (Maybe (Description, VarValue, [VarValueCandidate]))

data VarValue = MTextVal MText | SymbolVal Symbol | IntVal Int

data VarValueCandidate = ValueCandidate VarValue | Range Int Int

type CommandList = [CommandDeclaration]

data CommandDeclaration = CommandDeclaration Symbol (Maybe (Description, [KeySeq]))

type ModuleList = [Module]

data Module = Module Symbol [Symbol]

data MacroList = MacroList (Maybe MacroInclusion) [Macro] (Maybe MacroInclusion)

data MacroInclusion = MacroInclusion Tags (Maybe Symbol)

data Macro = Macro Symbol [Action]

data MapList = MapList (Maybe MapInclusion) [Map]

type Map = (Symbol, [Rule])

data Rule = Rule KeySeq [Action]

data KeySeq = MTextSeq MText | KeySeq [Either Symbol Int]

data MapInclusion = MapInclusion Tags (Maybe Symbol)

data Action = ActInsert InsertParam
            | ActDelete (Either Symbol Int)
            | ActSelect SelectParam
            | ActMove (Either Symbol Int)
            | ActMark Symbol
            | ActShow
            | ActHide
            | ActPushback (Either Int KeySeq)
            | ActPop
            | ActUndo (Either Int Symbol)
            | ActCommit
            | ActUnhandle
            | ActShift Symbol
            | ActCall Symbol Symbol [FnArg]
            | ActSet Cmd Symbol Expr
            | ActIf Cond [Action] [Action]
            | ActCond [(Expr, [Action])]
            | ActMacro Symbol

data InsertParam = InsMText MText | InsInt Int | InsSym Symbol | InsCands [Candidates]

data Candidates = CandsText MText | CandsMult [MText]

data SelectParam = SelPredef PredefinedSymbol | SelInt Int | SelSym Symbol

data FnArg = ArgInt Int | ArgSym Symbol | ArgText MText | ArgPlist Plist

data Cmd = Set | Add | Sub | Mul | Div

data Expr = ExprInt Int | ExprSym Symbol | ExprOp Operator [Expr]

data Operator = Plus | Minus | MulOp | DivOp | Or | And | Not | Eq | Gt | Lt | Geq | Leq

data Cond = EqCond Expr Expr | LtCond Expr Expr | GtCond Expr Expr | LeqCond Expr Expr | GeqCond Expr Expr

type PredefinedSymbol = Text  -- ^ The predefined symbol, with the starting \@ character.

data StateList = StateList (Maybe StateInclusion) [State] (Maybe StateInclusion)

data State = State Symbol (Maybe MText) [Branch]

data Branch = MapBranch Symbol [Action] | NilBranch [Action] | TBranch [Action]

data StateInclusion = StateInclusion Tags (Maybe Symbol)
           
data Tags = Tags
    { tagLanguage :: Symbol
    , tagName :: Symbol
    , tagExtraId :: Maybe Symbol
    }

type Symbol = Text
newtype MText = MText Text
    deriving (IsString)
type Plist = [(Text, Text)]

renderMText :: MText -> Text
renderMText (MText t) = pack $ show t
