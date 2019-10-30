{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : ALexerGen
Description : Lexer spec generator
Copyright   : (c) Josh Findon and Edward Jones, 2019
License     : MIT
Maintainer  : Josh Findon and Edward Jones
Stability   : experimental
Portability : POSIX + Windows
Language    : Haskell2010

This module generated Alex lexer specs from JSON and Haskell inputs.
-}
module Main (main) where

import           Data.Aeson           (FromJSON, eitherDecode')
import qualified Data.ByteString.Lazy as B (readFile)
import           Data.List            (intercalate, sortOn)
import           Data.Map             (Map, toList)
import           Data.Maybe           (fromMaybe)
import           Data.Semigroup       (Semigroup, (<>))
import           GHC.Generics         (Generic)
import           Prelude              hiding (shows)
import           System.Environment   (getArgs)
import           System.Exit          (die)

type LexerData = Map String TokenData

data TokenData =
    TokenData {
        lexeme          :: !String,
        literal         :: Maybe String,
        tokenValue      :: Maybe String,
        comment         :: Maybe String,
        extraFields     :: Maybe String,
        showExtraFields :: Maybe String,
        showBody        :: Maybe String,
        state           :: Maybe String,
        beginState      :: Maybe String,
        precedence      :: Maybe Int
    } deriving (Show, Generic)

instance FromJSON TokenData

data GeneratedLexer = GeneratedLexer {
    captureLines  :: [String],
    classes       :: [String],
    classToTokens :: [String],
    tokens        :: [String],
    shows         :: [String]
}

instance Semigroup GeneratedLexer where
    (GeneratedLexer a b c d e) <> (GeneratedLexer a' b' c' d' e') = GeneratedLexer (a <> a') (b <> b') (c <> c') (d <> d') (e <> e')

instance Monoid GeneratedLexer where
    mempty = GeneratedLexer [] [] [] [] []
    mappend = (<>)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [jsonFile, startFile, endFile] -> do
            d <- (eitherDecode' <$> B.readFile jsonFile) :: IO (Either String LexerData)
            case d of
                Left err -> putStrLn err
                Right ps -> do
                    start <- readFile startFile
                    end <- readFile endFile
                    putStrLn $ lexerToText start end (makeLexer ps)
        _ -> die "Must have at least 3 arguments"

lexerToText :: String -> String -> GeneratedLexer -> String
lexerToText start end lexer =
    start ++ (strRepl '\t' "    " $ lexerToText' lexer) ++ end ++ "\n}"
        where
            lexerToText' :: GeneratedLexer -> String
            lexerToText' lexer' = '\n' : unlines [ unlines (captureLines lexer')
                    , "{"
                    , "data LexemeClass = " ++ intercalate "\n\t\t| " (classes lexer')
                    , "\t deriving (Eq, Show)"
                    , ""
                    , "mkL :: LexemeClass -> AlexInput -> Int -> Alex Token"
                    , "mkL c (p, _, _, str) len = let t = take len str in case c of"
                    , '\t' : intercalate "\n\t" (classToTokens lexer')
                    , ""
                    , "alexEOF :: Alex Token"
                    , "alexEOF = return TEoF"
                    , ""
                    , "-- | Wrapper function for the lexer---allows the monadic lexer to be used with"
                    , "-- a monadic parser"
                    , "lexWrap :: (Token -> Alex a) -> Alex a"
                    , "lexWrap = (alexMonadScan >>=)"
                    , ""
                    , "-- | Type to represent tokens in the output stream"
                    , "data Token = " ++ intercalate "\n\t\t| " (tokens lexer')
                    , "\t\t| TEoF -- ^ @\\0@"
                    , "\tderiving (Eq, Ord)"
                    , ""
                    , "instance Show Token where"
                    , '\t' : intercalate "\n\t" (shows lexer')
                    , "\tshow TEoF = \"EoF\""
                    , ""
                ]

makeLexer :: LexerData -> GeneratedLexer
makeLexer d = foldl (<>) mempty $ makeLexer' <$> (sortOn (\(_,x) -> -(fromMaybe 0 $ precedence x)) $ toList d)
    where
        makeLexer' :: (String, TokenData) -> GeneratedLexer
        makeLexer' (name, td) = GeneratedLexer {
            captureLines = [ (formatState $ state td) ++ quotify td ++ " { " ++ action ++ " }" ]
            -- captureLines = [ (formatState $ state td) ++ quotify td ++ " { mkL L" ++ name ++ " }" ]
            , classes = [ 'L' : name ]
            , classToTokens = [ 'L' : name ++ " -> return (T" ++ name ++ ' ' : fromMaybeEmpty (tokenValue td) ++ " p)" ]
            , tokens = [ 'T' : name ++ " { " ++ fromMaybeEmpty ((++ ",") <$> extraFields td) ++ "position :: AlexPosn } -- ^ " ++ (sanitize $ fromMaybe ('@' : toLiteral td ++ "@") (comment td)) ]
            , shows = [ "show (T" ++ name ++ ' ' : fromMaybeEmpty (showExtraFields td) ++ " _) = " ++ fromMaybe ('"' : toLiteral td ++ "\"") (showBody td) ]
        }
            where
                toLiteral td' = fromMaybe (lexeme td') (literal td')
                quotify td' = if head (lexeme td') == '@' then lexeme td' else '"' : lexeme td' ++ "\""
                fromMaybeEmpty = fromMaybe ""
                sanitize = (strRepl '{' "Left brace") . (strRepl '}' "Right brace")
                formatState Nothing  = ""
                formatState (Just s) = '<' : s ++ ">"
                action = case beginState td of
                    Nothing -> "mkL L" ++ name
                    Just s  -> "begin " ++ s

strRepl :: Char -> String -> String -> String
strRepl c s s' = foldl (++) "" $ repl <$> s'
    where
        repl :: Char -> String
        repl x
            | x == c    = s
            | otherwise = [x]
