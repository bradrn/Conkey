{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module MSKLC.Parser (parse) where

import Data.Bifunctor (first)
import Data.Char (chr)
import Data.Foldable (asum)
import Data.List (find)
import Data.Void (Void)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import MSKLC.Keyboard

type Parser = Parsec Void Text
    
parse :: Text -> Either String Keyboard
parse t = first errorBundlePretty $ runParser parser "" t

parser :: Parser Keyboard
parser = do
    sc
    (kbdName, kbdDescription) <- parseIntro
    kbdCopyright <- parseCopyright
    kbdCompany <- parseCompany
    kbdLocaleName <- parseLocaleName
    kbdLocaleId <- parseLocaleId
    kbdVersion <- parseVersion
    shiftStates <- parseShiftStates
    layout <- parseLayout shiftStates
    deadKeyMappings <- many parseDeadKey
    let layout' = addDeadKeyMappings deadKeyMappings layout
    _ <- parseKeyNames
    _ <- parseKeyNameExts
    _ <- parseKeyNameDeads
    _ <- parseDescriptions
    (_, kbdLanguageNames) <- parseLanguageNames
    _ <- symbol "ENDKBD"
    eof
    return $ Keyboard Metadata{..} layout'

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//" <|> L.skipLineComment ";") empty

-- -- | Variant of sc, but does not accept newlines
-- sc' :: Parser ()
-- sc' = L.space space1' (L.skipLineComment "//" <|> L.skipLineComment ";") empty
--   where
--     space1' = void $ takeWhile1P (Just "white space") (\x -> isSpace x && (x /= "\n") && (x /= "\r"))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

hexadecimal :: Parser Int
hexadecimal = lexeme L.hexadecimal
    
quoted :: Parser Text
quoted = label "quoted string" $ lexeme $ fmap T.pack $
    char '"' >> manyTill anySingle (char '"')

spaced :: Parser Text
spaced = fmap T.pack $ lexeme $ manyTill anySingle spaceChar

parseIntro :: Parser (Text, Text)
parseIntro = (,) <$> (symbol "KBD" *> spaced) <*> quoted

parseCopyright :: Parser Text
parseCopyright = symbol "COPYRIGHT" *> quoted

parseCompany :: Parser Text
parseCompany = symbol "COMPANY" *> quoted

parseLocaleName :: Parser Text
parseLocaleName = symbol "LOCALENAME" *> quoted

parseLocaleId :: Parser Text
parseLocaleId = symbol "LOCALEID" *> quoted

parseVersion :: Parser Text
parseVersion = symbol "VERSION" *> spaced

parseShiftStates :: Parser [ShiftState]
parseShiftStates = symbol "SHIFTSTATE" *> many parseShiftState
  where
    parseShiftState = fmap getShiftStateFromNum $ lexeme $ oneOf @[] "012367"

    getShiftStateFromNum '0' = NoMod
    getShiftStateFromNum '1' = Shft
    getShiftStateFromNum '2' = Ctrl
    getShiftStateFromNum '3' = ShftCtrl
    getShiftStateFromNum '6' = CtrlAlt
    getShiftStateFromNum '7' = ShftCtrlAlt
    getShiftStateFromNum _ = error "Impossible parse state"

parseLayout :: [ShiftState] -> Parser Layout
parseLayout sss = symbol "LAYOUT" *> (fmap M.fromList $ many keyLine)
  where
    keyLine :: Parser (VkCode, CharAssign)
    keyLine = do
        notFollowedBy $ symbol "DEADKEY"  -- ensure we don't try to parse any lines from the following section
        _scanCode <- hexadecimal
        vkCode <- parseVkCode
        _cap <- hexadecimal
        keys <- count (length sss) parseEntry
        return (vkCode, M.fromList $ zip sss keys)

    parseVkCode :: Parser VkCode
    parseVkCode = asum $ consts ++ [Vk <$> lexeme letterChar]
        where
        consts :: [Parser VkCode]
        consts = fmap (\(v,s) -> v <$ lexeme (symbol s))
            [ (Oem102   , "OEM_102")
            , (Oem1     , "OEM_1")
            , (Oem2     , "OEM_2")
            , (Oem3     , "OEM_3")
            , (Oem4     , "OEM_4")
            , (Oem5     , "OEM_5")
            , (Oem6     , "OEM_6")
            , (Oem7     , "OEM_7")
            , (OemPlus  , "OEM_PLUS")
            , (OemComma , "OEM_COMMA")
            , (OemMinus , "OEM_MINUS")
            , (OemPeriod, "OEM_PERIOD")
            , (Vk0      , "0")
            , (Vk1      , "1")
            , (Vk2      , "2")
            , (Vk3      , "3")
            , (Vk4      , "4")
            , (Vk5      , "5")
            , (Vk6      , "6")
            , (Vk7      , "7")
            , (Vk8      , "8")
            , (Vk9      , "9")
            , (VkSpace  , "SPACE")
            , (VkDecimal, "DECIMAL")
            ]

    parseEntry :: Parser Entry
    parseEntry = (Unassigned <$ symbol "-1") <|> do
        val <- lexeme $
            (try $ anySingle <* lookAhead (eitherP space1 $ char '@')) <|> (chr <$> L.hexadecimal)
        dk <- optional $ symbol "@"
        return $ case dk of
            Nothing -> Entry val
            Just _  -> DeadKey $ DeadKeyDesc val M.empty

parseDeadKey :: Parser DeadKeyDesc
parseDeadKey = do
    _ <- symbol "DEADKEY"
    base <- chr <$> hexadecimal
    mapping <- fmap M.fromList $ many $ do
        notFollowedBy $ symbol "DEADKEY"  -- ensure we don't try to parse any lines from the following section
        (\x y -> (chr x, chr y)) <$> hexadecimal <*> hexadecimal
    return $ DeadKeyDesc base mapping

addDeadKeyMappings :: [DeadKeyDesc] -> Layout -> Layout
addDeadKeyMappings dks = M.map $ M.map $ \case
    DeadKey dk@DeadKeyDesc{dkBaseChar=b} -> DeadKey $
        case find ((==b) . dkBaseChar) dks of
            Just dk' -> dk'
            Nothing -> dk
    a -> a

parseKeyNames :: Parser (Map Int Text)
parseKeyNames = do
    _ <- symbol "KEYNAME"
    fmap M.fromList $ many $
        (,) <$> hexadecimal <*> (quoted <|> spaced)

parseKeyNameExts :: Parser (Map Int Text)
parseKeyNameExts = do
    _ <- symbol "KEYNAME_EXT"
    fmap M.fromList $ many $
        (,) <$> hexadecimal <*> (quoted <|> spaced)

parseKeyNameDeads :: Parser (Map Int Text)
parseKeyNameDeads = do
    _ <- symbol "KEYNAME_DEAD"
    fmap M.fromList $ many $ try $
        (,) <$> hexadecimal <*> quoted  -- note that none of the entries in KEYNAME_DEAD are unquoted

parseDescriptions :: Parser (Int, Text)
parseDescriptions = (,) <$> (symbol "DESCRIPTIONS" *> hexadecimal) <*> lexeme (T.pack <$> manyTill anySingle eol)

parseLanguageNames :: Parser (Int, Text)
parseLanguageNames = (,) <$> (symbol "LANGUAGENAMES" *> hexadecimal) <*> lexeme (T.pack <$> manyTill anySingle eol)
