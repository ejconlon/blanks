{-# LANGUAGE DeriveAnyClass #-}

module Test.Blanks.Exp
  ( Ident (..)
  , CExp (..)
  , cexpLoc
  , cexpParser
  , Exp (..)
  , ExpScope
  , ExpLocScope
  , nameless
  , named
  ) where

import Blanks (LocScope, pattern LocScopeBinder, pattern LocScopeBound, pattern LocScopeEmbed, pattern LocScopeFree,
               NameOnly, pattern NameOnly, Scope, locScopeAbstract1, locScopeUnAbstract1, runColocated)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Test.Blanks.Parsing

-- A newtype indicating an identifier in our language
newtype Ident = Ident { unIdent :: String } deriving newtype (Eq, Show, Ord, NFData)

-- The type of concrete expressions, labeled with source location
data CExp l =
    CExpBool !l !Bool
  | CExpInt !l !Int
  | CExpApp !l (CExp l) (CExp l)
  | CExpAdd !l (CExp l) (CExp l)
  | CExpIf !l (CExp l) (CExp l) (CExp l)
  | CExpIsZero !l (CExp l)
  | CExpVar !l !Ident
  | CExpAbs !l !Ident (CExp l)
  deriving (Eq, Show)

-- Extracts the location from a concrete expression
cexpLoc :: CExp l -> l
cexpLoc ce =
  case ce of
    CExpBool l _ -> l
    CExpInt l _ -> l
    CExpApp l _ _ -> l
    CExpAdd l _ _ -> l
    CExpIf l _ _ _ -> l
    CExpIsZero l _ -> l
    CExpVar l _ -> l
    CExpAbs l _ _ -> l

-- Parsers a concrete expression from a string
cexpParser :: Parser (CExp SourceSpan)
cexpParser = result where
  result = branch
    [ trueParser
    , falseParser
    , intParser
    , addParser
    , ifParser
    , isZeroParser
    , absParser
    , appParser
    , varParser
    ]

  trueParser = around (const . flip CExpBool True) (symbol "#t")

  falseParser = around (const . flip CExpBool False) (symbol "#f")

  intParser = around CExpInt signed

  addParser = around2 CExpAdd (parens (symbol "+" >> double cexpParser))

  ifParser = around3 CExpIf (parens (symbol "if" >> triple cexpParser))

  isZeroParser = around CExpIsZero (parens (symbol "zero?" >> cexpParser))

  absParser = around2 CExpAbs $ parens $ do
    _ <- symbol "lambda"
    n <- parens (fmap Ident identifier)
    b <- cexpParser
    pure (n, b)

  appParser = around2 CExpApp (parens (double cexpParser))

  varParser = around CExpVar (fmap Ident identifier)

-- Just the expressions of our language that have nothing to do with naming
data Exp a =
    ExpBool !Bool
  | ExpInt !Int
  | ExpApp a a
  | ExpAdd a a
  | ExpIf a a a
  | ExpIsZero a
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

-- An ExpScope without locations
type ExpScope = Scope (NameOnly Ident) Exp Ident

-- A nameless equivalent to 'CExp'
type ExpLocScope l = LocScope l (NameOnly Ident) Exp Ident

-- Convert to nameless representation
nameless :: CExp l -> ExpLocScope l
nameless ce =
  case ce of
    CExpBool l b -> LocScopeEmbed l (ExpBool b)
    CExpInt l i -> LocScopeEmbed l (ExpInt i)
    CExpApp l a b -> LocScopeEmbed l (ExpApp (nameless a) (nameless b))
    CExpAdd l a b -> LocScopeEmbed l (ExpAdd (nameless a) (nameless b))
    CExpIf l a b c -> LocScopeEmbed l (ExpIf (nameless a) (nameless b) (nameless c))
    CExpIsZero l a -> LocScopeEmbed l (ExpIsZero (nameless a))
    CExpVar l x -> LocScopeFree l x
    CExpAbs l x a -> runColocated (locScopeAbstract1 (NameOnly x) x (nameless a)) l

-- Convert back to named representation. Usually this isn't a necessary operation,
-- but we want to do round-trip testing
named :: ExpLocScope l -> Maybe (CExp l)
named e =
  case e of
    LocScopeBound _ _ -> Nothing
    LocScopeFree l a -> pure (CExpVar l a)
    LocScopeBinder l _ (NameOnly x) b -> CExpAbs l x <$> named (locScopeUnAbstract1 x b)
    LocScopeEmbed l fe ->
      case fe of
        ExpBool b -> pure (CExpBool l b)
        ExpInt i -> pure (CExpInt l i)
        ExpApp a b -> CExpApp l <$> named a <*> named b
        ExpAdd a b -> CExpAdd l <$> named a <*> named b
        ExpIf a b c -> CExpIf l <$> named a <*> named b <*> named c
        ExpIsZero a -> CExpIsZero l <$> named a
