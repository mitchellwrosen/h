{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

import Control.DeepSeq     (NFData, force)
import Control.Exception   (displayException, evaluate)
import Data.Dynamic        (fromDynamic)
import Data.Maybe          (catMaybes, fromJust)
import DynFlags            (ExtensionFlag(..), WarningFlag(..), wopt_unset, xopt_set)
import Exception           (SomeException, gtry, try)
import GHC
import GHC.Paths           (libdir)
import RdrName             (mkRdrUnqual)
import OccName             (mkTcOcc, mkVarOcc)
import Options.Applicative (execParser, headerDoc, helper, headerDoc, info)
import Turtle              hiding (d, e, f, s, x)

import qualified Data.Text                    as Text
import qualified Control.Foldl                as Foldl
import qualified Text.PrettyPrint.ANSI.Leijen as Doc


-- | A 'SupportedType' is the result of parsing the given string.
data SupportedType
  = TextToText         (Text   -> Text)
  | TextToBool         (Text   -> Bool)
  | TextToChar         (Text   -> Char)
  | TextToTextList     (Text   -> [Text])
  | TextListToText     ([Text] -> Text)
  | TextListToTextList ([Text] -> [Text])

-- | A 'Transformation' sort of an alternate view of 'SupportedType', which
-- categorizes each as a "linewise" or "blockwise" function.
--
-- To the user, we are flexible in the output type but ultimately cast it to a
-- [Text], where each Text is assumed to contain no newlines.
data Transformation
  = Linewise  (Text   -> [Text])
  | Blockwise ([Text] -> [Text])

-- | Bin each 'SupportedType' as the appropriate 'Transformation' (dictated
-- by whether its input accepts a Text or a [Text]), and munge the output type
-- accordingly.
supportedTypeTransformation :: SupportedType -> Transformation
supportedTypeTransformation = \case
  TextToText          f -> Linewise  (Text.lines . f)
  TextToBool          f -> Linewise  (\s -> if f s then [s] else [])
  TextToChar          f -> Linewise  (pure . Text.singleton . f)
  TextToTextList      f -> Linewise  f
  TextListToText      f -> Blockwise (Text.lines . f)
  TextListToTextList  f -> Blockwise f

-------------------------------------------------------------------------------
-- main

data Args = Args
  { keep :: Bool
  , func :: Text
  }

parseArgs :: Parser Args
parseArgs = Args
  <$> switch "keep" 'k' "Keep lines that throw exceptions"
  <*> fmap Text.unwords (some (argText "function" "A Haskell function"))

main :: IO ()
main = do
  Args{..} <-
    execParser (info (helper <*> parseArgs)
               (headerDoc (Just helpText)))

  let act :: IO (Maybe Transformation)
      act = fmap supportedTypeTransformation <$>
              compileString (Text.unpack func)

  try act >>= \case
    -- This is (probably) a GHC exception, just print it in all its ugly glory.
    Left (e :: SomeException) -> do
      err (Text.pack (displayException e))
      exit (ExitFailure 1)

    Right Nothing -> do
      err (Text.unlines
        [ "Could not compile `" <> func <> "` as any of the following types:"
        , ""
        , "  Text   -> Text"
        , "  Text   -> Bool"
        , "  Text   -> Char"
        , "  Text   -> [Text]"
        , "  [Text] -> Text"
        , "  [Text] -> [Text]"
        , ""
        , "  Show a => Text   -> a"
        , "  Show a => Text   -> [a]"
        , "  Show a => [Text] -> a"
        , "  Show a => [Text] -> [a]"
        , ""
        , "  (Read a, Show b) => a -> b"
        , "  (Read a, Show b) => a -> [b]"
        , ""
        , "If you think this is a bug, please submit it to https://github.com/mitchellwrosen/h/issues"
        ])
      exit (ExitFailure 1)

    -- Linewise transformation: apply it to each line in stdin
    --
    -- Note: the simpler "stdout (fmap f stdin)" applies f strictly to each
    -- line, but we want to translate exceptions to dropped (or kept) lines
    -- rather than die.
    Right (Just (Linewise f)) -> sh (do
      line <- stdin
      deepTry (f line) >>= \case
        Left (_ :: SomeException) -> when keep (echo line)
        Right ss -> mapM_ echo ss)

    -- Blockwise transformation: read all of stdin into memory, apply
    -- transformation, and stream it all out.
    Right (Just (Blockwise f)) -> do
      ss <- fold stdin Foldl.list
      deepTry (f ss) >>= \case
        Left (_ :: SomeException) -> pure ()
        Right ss' -> stdout (select ss')
 where
  helpText :: Doc.Doc
  helpText = Doc.vcat
    [ "Execute an arbitrary Haskell function on stdin and write the results to"
    , "stdout."
    , ""
    , "The function must have one of the following types:"
    , ""
    , "  Text   -> Text"
    , "  Text   -> Bool"
    , "  Text   -> Char"
    , "  Text   -> [Text]"
    , "  [Text] -> Text"
    , "  [Text] -> [Text]"
    , ""
    , "  Show a => Text   -> a"
    , "  Show a => Text   -> [a]"
    , "  Show a => [Text] -> a"
    , "  Show a => [Text] -> [a]"
    , ""
    , "  (Read a, Show b) => a -> b"
    , "  (Read a, Show b) => a -> [b]"
    , ""
    , "Returning a list as output will print each element on a separate line;"
    , "returning a boolean will filter out that Text if false;"
    , "and accepting a list as input will apply the function to all of stdin."
    , ""
    , "The function is compiled in the following context:"
    , ""
    , "  {-# LANGUAGE ExtendedDefaultRules #-}"
    , "  {-# LANGUAGE MultiWayIf           #-}"
    , "  {-# LANGUAGE OverloadedStrings    #-}"
    , "  {-# LANGUAGE ScopedTypeVariables  #-}"
    , "  {-# LANGUAGE ViewPatterns         #-}"
    , ""
    , "  import Data.Char"
    , "  import Data.Either"
    , "  import Data.Maybe"
    , "  import Data.List   hiding (lines, unlines, unwords, words)"
    , "  import Data.Monoid"
    , "  import Data.Text   (Text, lines, pack, unlines, unpack, unwords, words)"
    , "  import Prelude     hiding (lines, unlines, unwords, words)"
    , "  import Text.Printf"
    , "  import qualified Data.Text as T"
    , ""
    , "Example usage:"
    , ""
    , "  $ ls | h drop 2 | h T.map toUpper"
    , "  PACKAGE.YAML"
    , "  STACK.YAML"
    , ""
    , "  $ ls | h \"T.intersperse ' '\""
    , "  h - c l i - t o o l . c a b a l"
    , "  H . h s"
    , "  p a c k a g e . y a m l"
    , "  s t a c k . y a m l"
    , ""
    , "  $ ls | h \"\\x -> T.length x > 10\""
    , "  h-cli-tool.cabal"
    , "  package.yaml"
    ]

  deepTry :: (NFData a, MonadIO m) => a -> m (Either SomeException a)
  deepTry = liftIO . try . evaluate . force

-- | Compile a string to a 'SupportedType'. Throws a Ghc exception on parse
-- error, or returns Nothing if we couldn't figure out the type.
compileString :: String -> IO (Maybe SupportedType)
compileString s =
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags
           (dflags { ghcLink   = LinkInMemory
                   , hscTarget = HscInterpreted
                   }
             `xopt_set`   Opt_ExtendedDefaultRules
             `xopt_set`   Opt_MultiWayIf
             `xopt_set`   Opt_OverloadedStrings
             `xopt_set`   Opt_PartialTypeSignatures
             `xopt_set`   Opt_ScopedTypeVariables
             `xopt_set`   Opt_ViewPatterns
             `wopt_unset` Opt_WarnPartialTypeSignatures)

    setContext
      [ importModule "Data.Char"
      , importModule "Data.Either"
      , importModule "Data.Maybe"
      , importModuleHiding "Data.List" ["lines", "unlines", "unwords", "words"]
      , importModule "Data.Monoid"
      , importModule' "Data.Text" [] ["lines", "pack", "unlines", "unpack", "unwords", "words"]
      , importModule' "Data.Text.Internal" ["Text"] []
      , importModuleQualified "Data.Text" "T"
      , importModuleHiding "Prelude" ["lines", "unlines", "unwords", "words"]
      , importModule "Text.Printf"
      ]

    -- Parse the expression to throw a parse exception
    _ <- exprType s

    -- We try compiling the string as each of the following things:
    --
    -- 1. A function from Text to Text, Bool, Char, or [Text].

    let attempt1 :: Ghc SupportedType
        attempt1 = do
          d <- dynCompileExpr (ascribe s "Text -> _")
          pure $! head (catMaybes
            [ TextToText     <$> fromDynamic d
            , TextToBool     <$> fromDynamic d
            , TextToChar     <$> fromDynamic d
            , TextToTextList <$> fromDynamic d
            ])

    -- 2. A function from [Text] to Text or [Text].

    let attempt2 :: Ghc SupportedType
        attempt2 = do
          d <- dynCompileExpr (ascribe s "[Text] -> _")
          pure $! head (catMaybes
            [ TextListToText     <$> fromDynamic d
            , TextListToTextList <$> fromDynamic d
            ])

    -- 3. A function from Text to a [Show]. This comes before a Text to Show
    -- because we want to output elements of lists on separate lines.
    --
    --     Show a => Text -> [a]  (e.g. "map ord . unpack")

    let attempt3 :: Ghc SupportedType
        attempt3 = do
          d <- dynCompileExpr
                 (ascribe
                   ("map (pack . show) . (" ++ s ++ ")")
                   "Text -> _")
          pure $! fromJust (TextToTextList <$> fromDynamic d)

    -- 4. A function from Text to a Show.
    --
    --     Show a => Text -> a  (e.g. "T.length")

    let attempt4 :: Ghc SupportedType
        attempt4 = do
          d <- dynCompileExpr
                 (ascribe
                   ("pack . show . (" ++ s ++ ")")
                   "Text -> _")
          pure $! fromJust (TextToText <$> fromDynamic d)

    -- 5. A function from [Text] to a [Show]. This comes before a [Text] to
    --
    --     Show a => [Text] -> [a]  (e.g. "replicate 2 . length")

    let attempt5 :: Ghc SupportedType
        attempt5 = do
          d <- dynCompileExpr
                 (ascribe
                   ("map (pack . show) . (" ++ s ++ ")")
                   "[Text] -> _")
          pure $! fromJust (TextListToTextList <$> fromDynamic d)

    -- 6. A function from [Text] to a Show.
    --
    --     Show a => [Text] -> a  (e.g. "length")

    let attempt6 :: Ghc SupportedType
        attempt6 = do
          d <- dynCompileExpr
                 (ascribe
                   ("pack . show . (" ++ s ++ ")")
                   "[Text] -> _")
          pure $! fromJust (TextListToText <$> fromDynamic d)

    -- 7. A function from [Read] to a Text/[Text].
    --
    --     Read a => [a] -> Text
    --     Read a => [a] -> [Text]

    let attempt7 :: Ghc SupportedType
        attempt7 = do
          d <- dynCompileExpr ("(" ++ s ++ ") . map (read . unpack)")
          pure $! head (catMaybes
            [ TextListToText     <$> fromDynamic d
            , TextListToTextList <$> fromDynamic d
            ])

    -- 8. A function from [Read] to [Show]
    --
    --     (Read a, Show b) => [a] -> [b]

    let attempt8 :: Ghc SupportedType
        attempt8 = do
          d <- dynCompileExpr
                 ("map (pack . show) . (" ++ s ++ ") . map (read . unpack)")
          pure $! fromJust (TextListToTextList <$> fromDynamic d)

    -- 9. A function from [Read] to Show
    --
    --     (Read a, Show b) => [a] -> b

    let attempt9 :: Ghc SupportedType
        attempt9 = do
          d <- dynCompileExpr
                 ("pack . show . (" ++ s ++ ") . map (read . unpack)")
          pure $! fromJust (TextListToText <$> fromDynamic d)

    -- 10. A function from Read to a Text/[Text].
    --
    --     Read a => a -> Text
    --     Read a => a -> [Text]

    let attempt10 :: Ghc SupportedType
        attempt10 = do
          d <- dynCompileExpr ("(" ++ s ++ ") . read . unpack")
          pure $! head (catMaybes
            [ TextToText     <$> fromDynamic d
            , TextToTextList <$> fromDynamic d
            ])

    -- 11. A function from Read to [Show].
    --
    --     (Read a, Show b) => a -> [b]  (e.g. "\n -> [n+1,n+2]")

    let attempt11 :: Ghc SupportedType
        attempt11 = do
          d <- dynCompileExpr
                 ("map (pack . show) . (" ++ s ++ ") . read . unpack")
          pure $! fromJust (TextToTextList <$> fromDynamic d)

    -- 12. A function from Read to Show.
    --
    --     (Read a, Show b) => a -> b  (e.g. "\n -> n+1")

    let attempt12 :: Ghc SupportedType
        attempt12 = do
          d <- dynCompileExpr ("pack . show . (" ++ s ++ ") . read . unpack")
          pure $! fromJust (TextToText <$> fromDynamic d)

    gtryAll [ attempt1, attempt2, attempt3, attempt4, attempt5, attempt6,
              attempt7, attempt8, attempt9, attempt10, attempt11, attempt12 ]

ascribe :: String -> String -> String
ascribe s typ = "(" ++ s ++ ") :: " ++ typ

-------------------------------------------------------------------------------
-- Misc. Ghc API helpers

-- | import Module
importModule :: String -> InteractiveImport
importModule = IIDecl . simpleImportDecl . mkModuleName

-- | import Module (Tycon1, Tycon2, var1, var2)
importModule'
  :: String   -- ^ Module name
  -> [String] -- ^ Type constructors
  -> [String] -- ^ Variables
  -> InteractiveImport
importModule' s tcs vars =
  IIDecl (ImportDecl
    { ideclSourceSrc = Nothing
    , ideclName      = noLoc (mkModuleName s)
    , ideclPkgQual   = Nothing
    , ideclSource    = False
    , ideclSafe      = False
    , ideclQualified = False
    , ideclImplicit  = False
    , ideclAs        = Nothing
    , ideclHiding    = Just (False, noLoc (map importTypeConstructor tcs ++
                                           map importVar vars))
    })

-- | import Module hiding (var1, var2)
importModuleQualified :: String -> String -> InteractiveImport
importModuleQualified s1 s2 =
  IIDecl (ImportDecl
    { ideclSourceSrc = Nothing
    , ideclName      = noLoc (mkModuleName s1)
    , ideclPkgQual   = Nothing
    , ideclSource    = False
    , ideclSafe      = False
    , ideclQualified = True
    , ideclImplicit  = False
    , ideclAs        = Just (mkModuleName s2)
    , ideclHiding    = Nothing
    })

-- | import Module (var1, var2)
importModuleHiding
  :: String   -- ^ Module name
  -> [String] -- ^ Variables
  -> InteractiveImport
importModuleHiding s vars =
  IIDecl (ImportDecl
    { ideclSourceSrc = Nothing
    , ideclName      = noLoc (mkModuleName s)
    , ideclPkgQual   = Nothing
    , ideclSource    = False
    , ideclSafe      = False
    , ideclQualified = False
    , ideclImplicit  = False
    , ideclAs        = Nothing
    , ideclHiding    = Just (True, noLoc (map importVar vars))
    })

-- | import ___ (TypeConstructor)
importTypeConstructor :: String -> LIE RdrName
importTypeConstructor = noLoc . IEVar . noLoc . mkRdrUnqual . mkTcOcc

-- | import ___ (variable)
importVar :: String -> LIE RdrName
importVar = noLoc . IEVar . noLoc . mkRdrUnqual . mkVarOcc

-- | Try each action in a list, mapping every exception to Nothing.
gtryAll :: [Ghc a] -> Ghc (Maybe a)
gtryAll [] = pure Nothing
gtryAll (x:xs) =
  gtry x >>= \case
    Left (_ :: SomeException) -> gtryAll xs
    Right y -> pure (Just y)
