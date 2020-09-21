{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module M17N.Print (render) where

import M17N.Keyboard

import Data.Maybe (catMaybes, fromMaybe)

import Data.Text (Text)
import qualified Data.Text as T

-- | Given an M17N 'InputMethod', render it to its 'Text'
-- representation.
render :: InputMethod -> Text
render InputMethod{..} = T.intercalate "\n" $ catMaybes
    [ renderDeclaration  <$> imDeclaration
    , renderDescription  <$> imDescription
    , renderTitle        <$> imTitle
    , renderVariableList <$> imVariableList
    , renderCommandList  <$> imCommandList
    , renderModuleList   <$> imModuleList
    , renderMacroList    <$> imMacroList
    , renderMapList      <$> imMapList
    , renderStateList    <$> imStateList
    ]

renderDeclaration :: ImDeclaration -> Text
renderDeclaration ImDeclaration{..} = T.intercalate " " $ catMaybes
    [ Just "(input-method"
    , Just dclLanguage
    , Just dclName
    , dclExtraId
    , fmap (\v -> "(version" <> v <> ")") $ dclVersion
    , Just ")"
    ]

renderDescription :: Description -> Text
renderDescription (ConstMText t) = "(description " <> renderMText t <> ")"
renderDescription (GetText t) = "(description (_ " <> renderMText t <> "))"
renderDescription NilDesc = "(description nil)"

renderTitle :: MText -> Text
renderTitle t = "(title " <> renderMText t <> ")"

renderVariableList :: VariableList -> Text
renderVariableList = renderListKwd renderVariableDeclaration "variable"

renderVariableDeclaration :: VariableDeclaration -> Text
renderVariableDeclaration (VariableDeclaration name Nothing) = "(" <> name <> ")"
renderVariableDeclaration (VariableDeclaration name (Just (desc, val, cands))) =
    "(" <> name
    <> " " <> renderDescription desc
    <> " " <> renderVarValue val
    <> " " <> renderList renderCandidate cands <> ")"

renderVarValue :: VarValue -> Text
renderVarValue (MTextVal t) = renderMText t
renderVarValue (SymbolVal s) = s
renderVarValue (IntVal i) = renderInt i

renderCandidate :: VarValueCandidate -> Text
renderCandidate (ValueCandidate val) = renderVarValue val
renderCandidate (Range from to) = "(" <> renderInt from <> " " <> renderInt to <> ")"

renderCommandList :: CommandList -> Text
renderCommandList = renderListKwd renderCommandDeclaration "command"

renderCommandDeclaration :: CommandDeclaration -> Text
renderCommandDeclaration (CommandDeclaration name Nothing) = "(" <> name <> ")"
renderCommandDeclaration (CommandDeclaration name (Just (desc, keyseqs))) =
    "(" <> name <>
    " " <> renderDescription desc <>
    " " <> renderList renderKeySeq keyseqs <> ")"

renderKeySeq :: KeySeq -> Text
renderKeySeq (MTextSeq t) = renderMText t
renderKeySeq (KeySeq ks) = "(" <> renderList renderKey ks <> ")"
  where
    renderKey (Left s) = s
    renderKey (Right i) = renderInt i

renderModuleList :: ModuleList -> Text
renderModuleList = renderListKwd renderModule "module"

renderModule :: Module -> Text
renderModule (Module name fns) = "(" <> name <> " " <> T.intercalate " " fns <> ")"

renderMacroList :: MacroList -> Text
renderMacroList (MacroList incl1 ms incl2) = T.intercalate "\n" $ catMaybes
    [ renderMacroInclusion <$> incl1
    , Just $ renderListKwd renderMacro "macro" ms
    , renderMacroInclusion <$> incl2
    ]

renderMacroInclusion :: MacroInclusion -> Text
renderMacroInclusion (MacroInclusion tags Nothing) = "(include " <> renderTags tags <> " macro)"
renderMacroInclusion (MacroInclusion tags (Just name)) =
    "(include " <> renderTags tags <> " macro " <> name <> ")"

renderMacro :: Macro -> Text
renderMacro (Macro name acts) =
    "(" <> name <> " " <> renderList renderAction acts <> ")"

renderTags :: Tags -> Text
renderTags Tags{..} = "(" <> tagLanguage <> " " <> tagName <> " " <> fromMaybe "" tagExtraId <> ")"

renderMapList :: MapList -> Text
renderMapList (MapList incl ms) = maybe "" ((<>"\n").renderMapInclusion) incl <> renderListKwd renderMap "map" ms

renderMap :: Map -> Text
renderMap (name, rules) = "(" <> name <> "\n" <> T.intercalate "\n" (renderRule <$> rules) <> ")"

renderRule :: Rule -> Text
renderRule (Rule keyseq acts) = "(" <> renderKeySeq keyseq <> " " <> renderList renderAction acts <> ")"

renderMapInclusion :: MapInclusion -> Text
renderMapInclusion (MapInclusion tags name) = "(include " <> renderTags tags <> " map " <> fromMaybe "" name <> ")"

renderStateList :: StateList -> Text
renderStateList (StateList incl1 ss incl2) = T.intercalate "\n" $ catMaybes
    [ renderStateInclusion <$> incl1
    , Just $ renderListKwd renderState "state" ss
    , renderStateInclusion <$> incl2
    ]

renderStateInclusion :: StateInclusion -> Text
renderStateInclusion (StateInclusion tags name) = "(include " <> renderTags tags <> " map " <> fromMaybe "" name <> ")"

renderState :: State -> Text
renderState (State name title branches) =
    "(" <> name <> " " <> fromMaybe "" (renderMText <$> title) <> " " <> renderList renderBranch branches <> ")"

renderBranch :: Branch -> Text
renderBranch (MapBranch name acts) = "(" <> name <> " " <> renderList renderAction acts <> ")"
renderBranch (NilBranch acts) = "(nil " <> renderList renderAction acts <> ")"
renderBranch (TBranch acts) = "(t " <> renderList renderAction acts <> ")"

renderAction :: Action -> Text
renderAction (ActInsert p) = renderInsert p
renderAction (ActDelete p) = renderDelete p
renderAction (ActSelect p) = renderSelect p
renderAction (ActMove p) = renderMove p
renderAction (ActMark p) = "(mark " <> p <> ")"
renderAction ActShow = "(show)"
renderAction ActHide = "(hide)"
renderAction (ActPushback p) = renderPushback p
renderAction ActPop = "(pop)"
renderAction (ActUndo p) = renderUndo p
renderAction ActCommit = "(commit)"
renderAction ActUnhandle = "(unhandle)"
renderAction (ActShift p) = "(shift " <> p <> ")"
renderAction (ActCall mname fname args) =
    "(call " <> mname <> " " <> fname <> " " <> renderList renderArg args <> ")"
renderAction (ActSet cmd sym expr) = "(" <> renderCmd cmd <> " " <> sym <> " " <> renderExpr expr <> ")"
renderAction (ActIf cond thens elses) =
    "(" <> renderCondition cond
    <> " (" <> renderList renderAction thens
    <> ") (" <> renderList renderAction elses <> "))"
renderAction (ActCond conds) = renderListKwd renderCond "cond" conds
  where
    renderCond :: (Expr, [Action]) -> Text
    renderCond (expr, acts) = "(" <> renderExpr expr <> " " <> renderList renderAction acts <> ")"
renderAction (ActMacro name) = "(" <> name <> ")"

renderInsert :: InsertParam -> Text
renderInsert (InsMText t) = renderMText t
renderInsert (InsInt i) = renderInt i
renderInsert (InsSym s) = s
renderInsert (InsCands cs) = "(" <> renderList renderCandidates cs <> ")"

renderCandidates :: Candidates -> Text
renderCandidates (CandsText t) = renderMText t
renderCandidates (CandsMult ts) = "(" <> renderList renderMText ts <> ")"

renderDelete :: Either Symbol Int -> Text
renderDelete (Left s) = "(delete " <> s <> ")"
renderDelete (Right i) = "(delete " <> renderInt i <> ")"

renderSelect :: SelectParam -> Text
renderSelect (SelPredef p) = "(select " <> p <> ")"
renderSelect (SelInt i) = "(select " <> renderInt i <> ")"
renderSelect (SelSym s) = "(select " <> s <> ")"

renderMove :: Either Symbol Int -> Text
renderMove (Left s) = "(move " <> s <> ")"
renderMove (Right i) = "(move " <> renderInt i <> ")"

renderPushback :: Either Int KeySeq -> Text
renderPushback (Left i) = "(pushback " <> renderInt i <> ")"
renderPushback (Right k) = "(pushback " <> renderKeySeq k <> ")"

renderUndo :: Either Int Symbol -> Text
renderUndo (Left i) = "(undo " <> renderInt i <> ")"
renderUndo (Right s) = "(undo " <> s <> ")"

renderArg :: FnArg -> Text
renderArg (ArgInt i) = renderInt i
renderArg (ArgSym s) = s
renderArg (ArgText t) = renderMText t
renderArg (ArgPlist p) = "(" <> T.intercalate " " (flatPlist p) <> ")"
  where
    flatPlist = (>>= \(x,y) -> [x,y])

renderCmd :: Cmd -> Text
renderCmd Set = "set"
renderCmd Add = "add"
renderCmd Sub = "sub"
renderCmd Mul = "mul"
renderCmd Div = "div"

renderExpr :: Expr -> Text
renderExpr (ExprInt i) = renderInt i
renderExpr (ExprSym s) = s
renderExpr (ExprOp o xs) = "(" <> renderOperator o <> " " <> renderList renderExpr xs <> ")"

renderOperator :: Operator -> Text
renderOperator Plus  = "+"
renderOperator Minus = "-"
renderOperator MulOp = "*"
renderOperator DivOp = "/"
renderOperator Or    = "|"
renderOperator And   = "&"
renderOperator Not   = "!"
renderOperator Eq    = "="
renderOperator Gt    = ">"
renderOperator Lt    = "<"
renderOperator Geq   = ">="
renderOperator Leq   = "<="

renderCondition :: Cond -> Text
renderCondition (EqCond a b)  = "= "  <> renderExpr a <> " " <> renderExpr b
renderCondition (LtCond a b)  = "< "  <> renderExpr a <> " " <> renderExpr b
renderCondition (GtCond a b)  = "> "  <> renderExpr a <> " " <> renderExpr b
renderCondition (LeqCond a b) = "<= " <> renderExpr a <> " " <> renderExpr b
renderCondition (GeqCond a b) = ">= " <> renderExpr a <> " " <> renderExpr b

renderListKwd :: (a -> Text) -> Text -> [a] -> Text
renderListKwd r kwd = \rl -> "(" <> kwd <> " " <> renderList r rl <> ")"

renderList :: (a -> Text) -> [a] -> Text
renderList r = \rl -> T.intercalate " " (r <$> rl)

renderInt :: Int -> Text
renderInt = T.pack . show
