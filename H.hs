{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

import Control.DeepSeq     (force)
import Control.Exception   (evaluate)
import Data.Dynamic        (fromDynamic)
import Data.Maybe          (catMaybes)
import DynFlags            (ExtensionFlag(..), xopt_set)
import Exception           (SomeException, gtry, try)
import GHC
import GhcMonad            (withSession)
import GHC.Paths           (libdir)
import GHC.Prim            (unsafeCoerce#)
import HscMain             (hscStmt)
import RdrName             (mkRdrUnqual)
import OccName             (mkTcOcc, mkVarOcc)
import Options.Applicative (execParser, headerDoc, helper, headerDoc, info)
import Turtle              hiding (d, f, s, x)

import qualified Data.Text                    as Text
import qualified Control.Foldl                as Foldl
import qualified Text.PrettyPrint.ANSI.Leijen as Doc


-- | A 'SupportedType' is the result of parsing the given string.
data SupportedType
  = TextToText           (Text   -> Text)
  | TextToString         (Text   -> String)
  | TextToTextList       (Text   -> [Text])
  | TextToStringList     (Text   -> [String])
  | TextListToText       ([Text] -> Text)
  | TextListToString     ([Text] -> String)
  | TextListToTextList   ([Text] -> [Text])
  | TextListToStringList ([Text] -> [String])

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
  TextToText       f     -> Linewise  (Text.lines . f)
  TextToString     f     -> Linewise  (Text.lines . Text.pack . f)
  TextToTextList   f     -> Linewise  f
  TextToStringList f     -> Linewise  (map Text.pack . f)
  TextListToText   f     -> Blockwise (Text.lines . f)
  TextListToString f     -> Blockwise (Text.lines . Text.pack . f)
  TextListToTextList f   -> Blockwise f
  TextListToStringList f -> Blockwise (map Text.pack . f)


main :: IO ()
main = do
  s <- Text.unwords <$>
         execParser
           (info (helper <*> some (argText "function" "A Haskell function"))
                 (headerDoc (Just helpText)))

  let act :: IO Transformation
      act = supportedTypeTransformation <$>
              compileString (Text.unpack s)

  try act >>= \case
    -- Some ugly compiler error, don't bother printing it.
    Left (_ :: SomeException) -> do
      err (Text.unlines
        [ "Could not compile \"" <> s <> "\" as any of the following types:"
        , ""
        , "  Text   -> Text"
        , "  Text   -> String"
        , "  Text   -> [Text]"
        , "  Text   -> [String]"
        , "  [Text] -> Text"
        , "  [Text] -> String"
        , "  [Text] -> [Text]"
        , "  [Text] -> [String]"
        ])
      exit (ExitFailure 1)

    -- Linewise transformation: apply it to each line in stdin
    --
    -- Note: the simpler "stdout (fmap f stdin)" applies f strictly to each
    -- line, but we want to translate exceptions to dropped lines rather than
    -- die. This is useful for using partial functions strategically.
    Right (Linewise f) -> sh (do
      line <- stdin
      liftIO (try (evaluate (force (f line))) >>= \case
        Left (_ :: SomeException) -> pure ()
        Right txts -> mapM_ echo txts))

    -- Blockwise transformation: read all of stdin into memory, apply
    -- transformation, and stream it all out.
    Right (Blockwise f) -> fold stdin Foldl.list >>= stdout . select . f
 where
  helpText :: Doc.Doc
  helpText = Doc.vcat
    [ "Execute an arbitrary Haskell function on stdin and write the results to\
      \ stdout."
    , "The function must have one of the following types:"
    , ""
    , "  Text   -> Text"
    , "  Text   -> String"
    , "  Text   -> [Text]"
    , "  Text   -> [String]"
    , "  [Text] -> Text"
    , "  [Text] -> String"
    , "  [Text] -> [Text]"
    , "  [Text] -> [String]"
    , ""
    , "The function is compiled in the following context:"
    , ""
    , "  {-# LANGUAGE ExtendedDefaultRules #-}"
    , "  {-# LANGUAGE OverloadedStrings    #-}"
    , "  import Data.Char"
    , "  import Data.Either"
    , "  import Data.List   hiding (lines, unlines, unwords, words)"
    , "  import Data.Maybe"
    , "  import Data.Monoid"
    , "  import Data.Text   (Text, lines, pack, unlines, unpack, unwords, words)"
    , "  import Prelude     hiding (lines, unlines, unwords, words)"
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
    ]

-- | Compile a string to a 'SupportedType'. Throws a Ghc error if no supported
-- type could be successfully parsed.
compileString :: String -> IO SupportedType
compileString s =
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags
           (dflags
             { ghcLink   = LinkInMemory
             , hscTarget = HscInterpreted
             } `xopt_set` Opt_ExtendedDefaultRules
               `xopt_set` Opt_OverloadedStrings)

    setContext
      [ importModule "Data.Char"
      , importModule "Data.Either"
      , importModuleHiding "Data.List" ["lines", "unlines", "unwords", "words"]
      , importModule "Data.Maybe"
      , importModule "Data.Monoid"
      , importModule' "Data.Text" [] ["lines", "pack", "unlines", "unpack", "unwords", "words"]
      , importModule' "Data.Text.Internal" ["Text"] []
      , importModuleQualified "Data.Text" "T"
      , importModuleHiding "Prelude" ["lines", "unlines", "unwords", "words"]
      ]

    -- The "fast path": compile to a Dynamic, then see if it matches any of the
    -- supported types.
    let fastPath :: Ghc SupportedType
        fastPath = do
          d <- dynCompileExpr s
          case catMaybes
                 [ TextToText           <$> fromDynamic d
                 , TextToString         <$> fromDynamic d
                 , TextToTextList       <$> fromDynamic d
                 , TextToStringList     <$> fromDynamic d
                 , TextListToText       <$> fromDynamic d
                 , TextListToString     <$> fromDynamic d
                 , TextListToTextList   <$> fromDynamic d
                 , TextListToStringList <$> fromDynamic d
                 ] of
            -- Exception contents are totally ignored, it could be anything.
            -- Just has to be forced here or sequenced so as to not escape the
            -- enclosing 'try'.
            []    -> liftIO (evaluate undefined)
            (t:_) -> pure t

    gtryAll
      [ fastPath
      -- Here, the fast path failed, so try ascribing each supported type's
      -- type signature and recompile. Well, we don't need to try *every*
      -- supported type, because not all of them have much room for
      -- polymorphism. For example, what sort of useful function of type
      -- "forall a. Text -> [a]" could one possibly write that requires an
      -- explicit ":: Text -> String"?
      , TextToText         <$> compileExpr' (ascribe s "Text -> Text")
      , TextListToTextList <$> compileExpr' (ascribe s "[Text] -> [Text]")
      ]

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

-- | Like 'compileExpr', but we don't bother updating the fixity env because
-- we aren't defining any fixities. Caller is responsible for requesting the
-- right type from this function (it calls unsafeCoerce).
compileExpr' :: String -> Ghc a
compileExpr' s =
  withSession (\session -> liftIO (do
    Just ([_], hvals, _) <-
      hscStmt session ("let __h_compile_expr = " ++ s)
    [hval] <- hvals
    pure (unsafeCoerce# hval)))

-- | Try each action in turn, returning the first one to succeed (or the last
-- one in the list). List must be non-empty.
gtryAll :: [Ghc a] -> Ghc a
gtryAll [] = error "gtryAll: empty list"
gtryAll [x] = x
gtryAll (x:xs) =
  gtry x >>= \case
    Left (_ :: SomeException) -> gtryAll xs
    Right y -> pure y
