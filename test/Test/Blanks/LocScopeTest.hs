module Test.Blanks.LocScopeTest where

import Blanks
import Data.String (IsString)
import Test.Blanks.Parsing
import Test.Tasty
import Test.Tasty.HUnit

-- A newtype indicating an identifier in our language
newtype Ident = Ident { unIdent :: String } deriving (Eq, Show, Ord, IsString)

-- The type of concrete expressions, labeled with source location
data CExp l =
    CExpTrue !l
  | CExpFalse !l
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
    CExpTrue l -> l
    CExpFalse l -> l
    CExpInt l _ -> l
    CExpApp l _ _ -> l
    CExpAdd l _ _ -> l
    CExpIf l _ _ _ -> l
    CExpIsZero l _ -> l
    CExpVar l _ -> l
    CExpAbs l _ _ -> l

-- Just the expressions of our language that have nothing to do with naming
data Exp a =
    ExpTrue
  | ExpFalse
  | ExpInt !Int
  | ExpApp a a
  | ExpAdd a a
  | ExpIf a a a
  | ExpIsZero a
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- A nameless equivalent to 'CExp'
type ExpScope l = LocScope l (NameOnly Ident) Exp Ident

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

  trueParser = around (const . CExpTrue) (symbol "#t")

  falseParser = around (const . CExpFalse) (symbol "#f")

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

-- Convert to nameless representation
nameless :: CExp l -> ExpScope l
nameless ce =
  case ce of
    CExpTrue l -> LocScopeEmbed l ExpTrue
    CExpFalse l -> LocScopeEmbed l ExpFalse
    CExpInt l i -> LocScopeEmbed l (ExpInt i)
    CExpApp l a b -> LocScopeEmbed l (ExpApp (nameless a) (nameless b))
    CExpAdd l a b -> LocScopeEmbed l (ExpAdd (nameless a) (nameless b))
    CExpIf l a b c -> LocScopeEmbed l (ExpIf (nameless a) (nameless b) (nameless c))
    CExpIsZero l a -> LocScopeEmbed l (ExpIsZero (nameless a))
    CExpVar l x -> LocScopeFree l x
    CExpAbs l x a -> runColocated (blankAbstract1 (NameOnly x) x (nameless a)) l

-- Convert back to named representation. Usually this isn't a necessary operation,
-- but we want to do round-trip testing
named :: ExpScope l -> Maybe (CExp l)
named e =
  case e of
    LocScopeBound _ _ -> Nothing
    LocScopeFree l a -> pure (CExpVar l a)
    LocScopeBinder l _ (NameOnly x) b -> CExpAbs l x <$> named (blankUnAbstract1 x b)
    LocScopeEmbed l fe ->
      case fe of
        ExpTrue -> pure (CExpTrue l)
        ExpFalse -> pure (CExpFalse l)
        ExpInt i -> pure (CExpInt l i)
        ExpApp a b -> CExpApp l <$> named a <*> named b
        ExpAdd a b -> CExpAdd l <$> named a <*> named b
        ExpIf a b c -> CExpIf l <$> named a <*> named b <*> named c
        ExpIsZero a -> CExpIsZero l <$> named a

type ExpSimpleScope = Scope (NameOnly Ident) Exp Ident

testSingle :: TestName -> String -> ExpSimpleScope -> TestTree
testSingle name input expected = testCase name $ do
  namedExp <- runParserIO cexpParser input
  let namelessExp = nameless namedExp
  cexpLoc namedExp @?= locScopeLocation namelessExp
  let actual = locScopeForget namelessExp
  expected @?= actual
  let renamedExp = named namelessExp
  Just namedExp @?= renamedExp

testLocScope :: TestTree
testLocScope = testGroup "LocScope" cases where
  xIdent = Ident "x"
  yIdent = Ident "y"
  xExp = ScopeFree xIdent
  yExp = ScopeFree yIdent
  trueExp = ScopeEmbed ExpTrue
  intExp = ScopeEmbed (ExpInt 42)
  negIntExp = ScopeEmbed (ExpInt (-42))
  cases =
    [ testSingle "var" "x" xExp
    , testSingle "true" "#t" trueExp
    , testSingle "false" "#f" (ScopeEmbed ExpFalse)
    , testSingle "int" "42" intExp
    , testSingle "neg int" "-42" negIntExp
    , testSingle "add" "(+ 42 -42)" (ScopeEmbed (ExpAdd intExp negIntExp))
    , testSingle "if" "(if #t 42 -42)" (ScopeEmbed (ExpIf trueExp intExp negIntExp))
    , testSingle "add var" "(+ 42 x)" (ScopeEmbed (ExpAdd intExp xExp))
    , testSingle "iszero" "(zero? 42)" (ScopeEmbed (ExpIsZero intExp))
    , testSingle "app" "(x y)" (ScopeEmbed (ExpApp xExp yExp))
    , testSingle "abs yy" "(lambda (y) y)" (ScopeBinder 1 (NameOnly yIdent) (ScopeBound 0))
    , testSingle "abs xyy" "(lambda (x) (lambda (y) y))" (ScopeBinder 1 (NameOnly xIdent) (ScopeBinder 1 (NameOnly yIdent) (ScopeBound 0)))
    , testSingle "abs xyx" "(lambda (x) (lambda (y) x))" (ScopeBinder 1 (NameOnly xIdent) (ScopeBinder 1 (NameOnly yIdent) (ScopeBound 1)))
    , testSingle "app abs" "((lambda (x) x) 42)" (ScopeEmbed (ExpApp (ScopeBinder 1 (NameOnly xIdent) (ScopeBound 0)) intExp))
    ]
