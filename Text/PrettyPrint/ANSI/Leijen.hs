{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.ANSI.Leijen
-- Copyright   :  Daan Leijen (c) 2000, http://www.cs.uu.nl/~daan
--                Max Bolingbroke (c) 2008, http://blog.omega-prime.co.uk
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  batterseapower@hotmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Pretty print module based on Philip Wadler's \"prettier printer\"
--
-- @
--      \"A prettier printer\"
--      Draft paper, April 1997, revised March 1998.
--      <http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf>
-- @
--
-- PPrint is an implementation of the pretty printing combinators
-- described by Philip Wadler (1997). In their bare essence, the
-- combinators of Wadler are not expressive enough to describe some
-- commonly occurring layouts. The PPrint library adds new primitives
-- to describe these layouts and works well in practice.
--
-- The library is based on a single way to concatenate documents,
-- which is associative and has both a left and right unit.  This
-- simple design leads to an efficient and short implementation. The
-- simplicity is reflected in the predictable behaviour of the
-- combinators which make them easy to use in practice.
--
-- A thorough description of the primitive combinators and their
-- implementation can be found in Philip Wadler's paper
-- (1997). Additions and the main differences with his original paper
-- are:
--
-- * The nil document is called empty.
--
-- * The above combinator is called '<$>'. The operator '</>' is used
-- for soft line breaks.
--
-- * There are three new primitives: 'align', 'fill' and
-- 'fillBreak'. These are very useful in practice.
--
-- * Lots of other useful combinators, like 'fillSep' and 'list'.
--
-- * There are two renderers, 'renderPretty' for pretty printing and
-- 'renderCompact' for compact output. The pretty printing algorithm
-- also uses a ribbon-width now for even prettier output.
--
-- * There are two displayers, 'displayS' for strings and 'displayIO' for
-- file based output.
--
-- * There is a 'Pretty' class.
--
-- * The implementation uses optimised representations and strictness
-- annotations.
--
-- Full documentation for the original wl-pprint library available at
-- <http://www.cs.uu.nl/~daan/download/pprint/pprint.html>.
--
-- The library has been extended to allow formatting text for output
-- to ANSI style consoles. New combinators allow:
--
-- * Control of foreground and background color of text
--
-- * The abliity to make parts of the text bold or underlined
--
-- This functionality is, as far as possible, portable across platforms
-- with their varying terminals.  However, one thing to be particularly
-- wary of is that console colors will not be displayed on Windows unless
-- the 'Doc' value is output using the 'putDoc' function or one of it's
-- friends.  Rendering the 'Doc' to a 'String' and then outputing /that/
-- will only work on Unix-style operating systems.
-----------------------------------------------------------
module Text.PrettyPrint.ANSI.Leijen (
   -- * Documents
   Doc, putDoc, hPutDoc,

   -- * Basic combinators
   empty, char, text, (<>), nest, line, linebreak, group, softline,
   softbreak, hardline, flatAlt, renderSmart,

   -- * Alignment
   --
   -- The combinators in this section can not be described by Wadler's
   -- original combinators. They align their output relative to the
   -- current output position - in contrast to @nest@ which always
   -- aligns to the current nesting level. This deprives these
   -- combinators from being \`optimal\'. In practice however they
   -- prove to be very useful. The combinators in this section should
   -- be used with care, since they are more expensive than the other
   -- combinators. For example, @align@ shouldn't be used to pretty
   -- print all top-level declarations of a language, but using @hang@
   -- for let expressions is fine.
   align, hang, indent, encloseSep, list, tupled, semiBraces,

   -- * Operators
   (<+>), (Text.PrettyPrint.ANSI.Leijen.<$>), (</>), (<$$>), (<//>),

   -- * List combinators
   hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat, punctuate,

   -- * Fillers
   fill, fillBreak,

   -- * Bracketing combinators
   enclose, squotes, dquotes, parens, angles, braces, brackets,

   -- * Character documents
   lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket,
   squote, dquote, semi, colon, comma, space, dot, backslash, equals,

   -- * Colorisation combinators
   black, red, green, yellow, blue, magenta, cyan, white,
   dullblack, dullred, dullgreen, dullyellow, dullblue, dullmagenta, dullcyan, dullwhite,
   onblack, onred, ongreen, onyellow, onblue, onmagenta, oncyan, onwhite,
   ondullblack, ondullred, ondullgreen, ondullyellow, ondullblue, ondullmagenta, ondullcyan, ondullwhite,

   -- * Emboldening combinators
   bold, debold,

   -- * Underlining combinators
   underline, deunderline,

   -- * Removing formatting
   plain,

   -- * Primitive type documents
   string, int, integer, float, double, rational,

   -- * Pretty class
   Pretty(..),

   -- * Rendering
   SimpleDoc(..), renderPretty, renderCompact, displayS, displayIO

   -- * Undocumented
        , bool

        , column, columns, nesting, width

        ) where

import System.IO (Handle,hPutStr,stdout)
import qualified Text.PrettyPrint.Free.Internal as P
import           Text.PrettyPrint.Free.Internal
  hiding (Doc, renderPretty, renderSmart, displayS, displayIO, putDoc, hPutDoc)

import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..),
                            Underlining(..), ConsoleIntensity(..),
                            SGR(..), setSGRCode)

import Data.Maybe (catMaybes, maybeToList)

-- | The document @(x \<$\> y)@ concatenates document @x@ and @y@ with a
-- 'line' in between. (infixr 5)
(<$>) :: Doc -> Doc -> Doc
x <$> y         = x <> line <> y

-- | The document @(x \<$$\> y)@ concatenates document @x@ and @y@ with
-- a @linebreak@ in between. (infixr 5)
(<$$>) :: Doc -> Doc -> Doc
x <$$> y        = x <> linebreak <> y


-----------------------------------------------------------
-- Combinators for prelude types
-----------------------------------------------------------

-- string is like "text" but replaces '\n' by "line"

-- | The document @(string s)@ concatenates all characters in @s@
-- using @line@ for newline characters and @char@ for all other
-- characters. It is used instead of 'text' whenever the text contains
-- newline characters.
string :: String -> Doc
string ""       = empty
string ('\n':s) = line <> string s
string s        = case (span (/='\n') s) of
                    (xs,ys) -> text xs <> string ys

bool :: Bool -> Doc
bool b          = text (show b)

-- | The document @(int i)@ shows the literal integer @i@ using
-- 'text'.
int :: Int -> Doc
int i           = text (show i)

-- | The document @(integer i)@ shows the literal integer @i@ using
-- 'text'.
integer :: Integer -> Doc
integer i       = text (show i)

-- | The document @(float f)@ shows the literal float @f@ using
-- 'text'.
float :: Float -> Doc
float f         = text (show f)

-- | The document @(double d)@ shows the literal double @d@ using
-- 'text'.
double :: Double -> Doc
double d        = text (show d)

-- | The document @(rational r)@ shows the literal rational @r@ using
-- 'text'.
rational :: Rational -> Doc
rational r      = text (show r)

-----------------------------------------------------------
-- Primitives
-----------------------------------------------------------

data AnsiEffect = AEColor ConsoleLayer ColorIntensity Color
                | AEIntensify ConsoleIntensity
                | AEItalicize Bool
                | AEUnderline Underlining

type Doc = P.Doc AnsiEffect ()

type SimpleDoc' = SimpleDoc () (Either [SGR] ())

-----------------------------------------------------------
-- Colors
-----------------------------------------------------------

-- | Displays a document with the black forecolor
black :: Doc -> Doc
-- | Displays a document with the red forecolor
red :: Doc -> Doc
-- | Displays a document with the green forecolor
green :: Doc -> Doc
-- | Displays a document with the yellow forecolor
yellow :: Doc -> Doc
-- | Displays a document with the blue forecolor
blue :: Doc -> Doc
-- | Displays a document with the magenta forecolor
magenta :: Doc -> Doc
-- | Displays a document with the cyan forecolor
cyan :: Doc -> Doc
-- | Displays a document with the white forecolor
white :: Doc -> Doc
-- | Displays a document with the dull black forecolor
dullblack :: Doc -> Doc
-- | Displays a document with the dull red forecolor
dullred :: Doc -> Doc
-- | Displays a document with the dull green forecolor
dullgreen :: Doc -> Doc
-- | Displays a document with the dull yellow forecolor
dullyellow :: Doc -> Doc
-- | Displays a document with the dull blue forecolor
dullblue :: Doc -> Doc
-- | Displays a document with the dull magenta forecolor
dullmagenta :: Doc -> Doc
-- | Displays a document with the dull cyan forecolor
dullcyan :: Doc -> Doc
-- | Displays a document with the dull white forecolor
dullwhite :: Doc -> Doc
(black, dullblack)     = colorFunctions Black
(red, dullred)         = colorFunctions Red
(green, dullgreen)     = colorFunctions Green
(yellow, dullyellow)   = colorFunctions Yellow
(blue, dullblue)       = colorFunctions Blue
(magenta, dullmagenta) = colorFunctions Magenta
(cyan, dullcyan)       = colorFunctions Cyan
(white, dullwhite)     = colorFunctions White

-- | Displays a document with a forecolor given in the first parameter
color :: Color -> Doc -> Doc
-- | Displays a document with a dull forecolor given in the first parameter
dullcolor :: Color -> Doc -> Doc
color     = Annotate . AEColor Foreground Vivid
dullcolor = Annotate . AEColor Foreground Dull

colorFunctions :: Color -> (Doc -> Doc, Doc -> Doc)
colorFunctions what = (color what, dullcolor what)

-- | Displays a document with the black backcolor
onblack :: Doc -> Doc
-- | Displays a document with the red backcolor
onred :: Doc -> Doc
-- | Displays a document with the green backcolor
ongreen :: Doc -> Doc
-- | Displays a document with the yellow backcolor
onyellow :: Doc -> Doc
-- | Displays a document with the blue backcolor
onblue :: Doc -> Doc
-- | Displays a document with the magenta backcolor
onmagenta :: Doc -> Doc
-- | Displays a document with the cyan backcolor
oncyan :: Doc -> Doc
-- | Displays a document with the white backcolor
onwhite :: Doc -> Doc
-- | Displays a document with the dull block backcolor
ondullblack :: Doc -> Doc
-- | Displays a document with the dull red backcolor
ondullred :: Doc -> Doc
-- | Displays a document with the dull green backcolor
ondullgreen :: Doc -> Doc
-- | Displays a document with the dull yellow backcolor
ondullyellow :: Doc -> Doc
-- | Displays a document with the dull blue backcolor
ondullblue :: Doc -> Doc
-- | Displays a document with the dull magenta backcolor
ondullmagenta :: Doc -> Doc
-- | Displays a document with the dull cyan backcolor
ondullcyan :: Doc -> Doc
-- | Displays a document with the dull white backcolor
ondullwhite :: Doc -> Doc
(onblack, ondullblack)     = oncolorFunctions Black
(onred, ondullred)         = oncolorFunctions Red
(ongreen, ondullgreen)     = oncolorFunctions Green
(onyellow, ondullyellow)   = oncolorFunctions Yellow
(onblue, ondullblue)       = oncolorFunctions Blue
(onmagenta, ondullmagenta) = oncolorFunctions Magenta
(oncyan, ondullcyan)       = oncolorFunctions Cyan
(onwhite, ondullwhite)     = oncolorFunctions White

-- | Displays a document with a backcolor given in the first parameter
oncolor :: Color -> Doc -> Doc
-- | Displays a document with a dull backcolor given in the first parameter
ondullcolor :: Color -> Doc -> Doc
oncolor     = Annotate . AEColor Background Vivid
ondullcolor = Annotate . AEColor Background Dull

oncolorFunctions :: Color -> (Doc -> Doc, Doc -> Doc)
oncolorFunctions what = (oncolor what, ondullcolor what)


-----------------------------------------------------------
-- Console Intensity
-----------------------------------------------------------

-- | Displays a document in a heavier font weight
bold :: Doc -> Doc
bold = Annotate (AEIntensify BoldIntensity)

-- | Displays a document in the normal font weight
debold :: Doc -> Doc
debold = Annotate (AEIntensify NormalIntensity)

-- NB: I don't support FaintIntensity here because it is not widely supported by terminals.


-----------------------------------------------------------
-- Italicization
-----------------------------------------------------------

{-

I'm in two minds about providing these functions, since italicization is so rarely implemented.
It is especially bad because "italicization" may cause the meaning of colors to flip, which will
look a bit weird, to say the least...


-- | Displays a document in italics. This is not widely supported, and it's use is not recommended
italicize :: Doc -> Doc
italicize = Italicize True

-- | Displays a document with no italics
deitalicize :: Doc -> Doc
deitalicize = Italicize False

-}

-----------------------------------------------------------
-- Underlining
-----------------------------------------------------------

-- | Displays a document with underlining
underline :: Doc -> Doc
underline = Annotate (AEUnderline SingleUnderline)

-- | Displays a document with no underlining
deunderline :: Doc -> Doc
deunderline = Annotate (AEUnderline NoUnderline)

-- NB: I don't support DoubleUnderline here because it is not widely supported by terminals.

-----------------------------------------------------------
-- Removing formatting
-----------------------------------------------------------

-- | Removes all colorisation, emboldening and underlining from a document
plain :: Doc -> Doc
plain Fail            = Fail
plain e@Empty         = e
plain c@(Char _)      = c
plain t@(Text _ _)    = t
plain l@Line          = l
plain (FlatAlt x y)   = FlatAlt (plain x) (plain y)
plain (Cat x y)       = Cat (plain x) (plain y)
plain (Nest i x)      = Nest i (plain x)
plain (Union x y)     = Union (plain x) (plain y)
plain (Column f)      = Column (plain . f)
plain (Columns f)     = Columns (plain . f)
plain (Ribbon f)      = Ribbon (plain . f)
plain (Nesting f)     = Nesting (plain . f)
plain (Effect x)      = Effect x
plain (Annotate _ x)  = x

-----------------------------------------------------------
-- Renderers
-----------------------------------------------------------

-----------------------------------------------------------
-- renderPretty: the default pretty printing algorithm
-----------------------------------------------------------

data AnsiState = AS
  { as_fc :: Maybe (ColorIntensity, Color)
  , as_bc :: Maybe (ColorIntensity, Color)
  , as_in :: Maybe ConsoleIntensity
  , as_it :: Maybe Bool
  , as_un :: Maybe Underlining

  , -- | A delta to be applied in the "push" direction
    as_delta :: Maybe SGR
{-
  , -- | A delta to be applied in the "pop" direction
    as_undelta :: Maybe SGR
-}
  }

-- Push all annotations down to the effect layer for easier processing!
scanANSI :: P.SimpleDoc AnsiEffect e -> P.SimpleDoc () (Either [SGR] e)
scanANSI = sdocAE (\_ e -> SEffect (Right e))
                  merge
                  (\as -> SEffect (Left $ maybeToList (as_delta as)))
                  (\as -> SEffect (Left $ reset as))
                  (AS Nothing Nothing Nothing Nothing Nothing Nothing)
 where
  merge :: AnsiState -> AnsiEffect -> AnsiState
  merge as0 ae = (\x -> x { as_delta = Just $ de ae }) $ go ae
   where
    go (AEColor Foreground i c) = as0 { as_fc = Just (i,c) }
    go (AEColor Background i c) = as0 { as_bc = Just (i,c) }
    go (AEIntensify i)          = as0 { as_in = Just i }
    go (AEItalicize i)          = as0 { as_it = Just i }
    go (AEUnderline u)          = as0 { as_un = Just u }

    de (AEColor l i c) = SetColor l i c
    de (AEIntensify i) = SetConsoleIntensity i
    de (AEItalicize i) = SetItalicized i
    de (AEUnderline u) = SetUnderlining u

  reset :: AnsiState -> [SGR]
  reset as = Reset : catMaybes
    [ (uncurry (SetColor Foreground)) `fmap` as_fc as
    , (uncurry (SetColor Background)) `fmap` as_bc as
    , SetConsoleIntensity             `fmap` as_in as
    , SetItalicized                   `fmap` as_it as
    , SetUnderlining                  `fmap` as_un as
    ]

-- | This is the default pretty printer which is used by 'show',
-- 'putDoc' and 'hPutDoc'. @(renderPretty ribbonfrac width x)@ renders
-- document @x@ with a page width of @width@ and a ribbon width of
-- @(ribbonfrac * width)@ characters. The ribbon width is the maximal
-- amount of non-indentation characters on a line. The parameter
-- @ribbonfrac@ should be between @0.0@ and @1.0@. If it is lower or
-- higher, the ribbon width will be 0 or @width@ respectively.
renderPretty :: Float -> Int -> Doc -> SimpleDoc'
renderPretty f i = scanANSI . P.renderPretty f i

-- | A slightly smarter rendering algorithm with more lookahead. It provides
-- provide earlier breaking on deeply nested structures.
-- For example, consider this python-ish pseudocode:
-- @fun(fun(fun(fun(fun([abcdefg, abcdefg])))))@
-- If we put a softbreak (+ nesting 2) after each open parenthesis, and align
-- the elements of the list to match the opening brackets, this will render with
-- @renderPretty@ and a page width of 20c as:
-- @
-- fun(fun(fun(fun(fun([
--                     | abcdef,
--                     | abcdef,
--                     ]
--   )))))             |
-- @
-- Where the 20c. boundary has been marked with |. Because @renderPretty@ only
-- uses one-line lookahead, it sees that the first line fits, and is stuck
-- putting the second and third lines after the 20c mark. In contrast,
-- @renderSmart@ will continue to check the potential document up to the end of
-- the indentation level. Thus, it will format the document as:
--
-- @
-- fun(                |
--   fun(              |
--     fun(            |
--       fun(          |
--         fun([       |
--               abcdef,
--               abcdef,
--             ]       |
--   )))))             |
-- @
-- Which fits within the 20c. mark.
-- In addition, @renderSmart@ uses this lookahead to minimize the number of
-- lines printed, leading to more compact and visually appealing output.
-- Consider this example using the same syntax as above:
-- @aaaaaaaaaaa([abc, def, ghi])@
-- When rendered with @renderPretty@ and a page width of 20c, we get:
-- @
-- aaaaaaaaaaa([ abc
--             , def
--             , ghi ])
-- @
-- Whereas when rendered with @renderSmart@ and a page width of 20c, we get:
-- @
-- aaaaaaaaaaa(
--   [abc, def, ghi])
-- @

renderSmart :: Int -> Doc -> SimpleDoc'
renderSmart i = scanANSI . P.renderSmart i

{- From renderFits:
    -- I used to do a @SSGR [Reset]@ here, but if you do that it will result
    -- in any rendered @Doc@ containing at least some ANSI control codes. This
    -- may be undesirable if you want to render to non-ANSI devices by simply
    -- not making use of the ANSI color combinators I provide.
    --
    -- What I "really" want to do here is do an initial Reset iff there is some
    -- ANSI color within the Doc, but that's a bit fiddly. I'll fix it if someone
    -- complains!
-}



-----------------------------------------------------------
-- Displayers:  displayS and displayIO
-----------------------------------------------------------


-- | @(displayS simpleDoc)@ takes the output @simpleDoc@ from a
-- rendering function and transforms it to a 'ShowS' type (for use in
-- the 'Show' class).
--
-- > showWidth :: Int -> Doc -> String
-- > showWidth w x   = displayS (renderPretty 0.4 w x) ""
displayS :: SimpleDoc' -> ShowS
displayS = displayDecorated ci ci ef showString
 where
  ci = const id

  ef (Right _) = id
  ef (Left x)  = showString (setSGRCode x)

-- | @(displayIO handle simpleDoc)@ writes @simpleDoc@ to the file
-- handle @handle@, discarding all effects and annotations. This function
-- is used for example by 'hPutDoc':
--
-- > hPutDoc handle doc  = displayIO handle (renderPretty 0.4 80 doc)
--
-- Any ANSI colorisation in @simpleDoc@ will be output.
displayIO :: Handle -> SimpleDoc' -> IO ()
displayIO handle = displayDecorated cpu cpu cpu (hPutStr handle)
 where cpu = const (pure ())

-----------------------------------------------------------
-- default pretty printers: show, putDoc and hPutDoc
-----------------------------------------------------------

instance Show Doc where
  showsPrec _ doc       = displayS $ renderPretty 0.4 80 doc

-- | The action @(putDoc doc)@ pretty prints document @doc@ to the
-- standard output, with a page width of 100 characters and a ribbon
-- width of 40 characters.
--
-- > main :: IO ()
-- > main = do{ putDoc (text "hello" <+> text "world") }
--
-- Which would output
--
-- @
-- hello world
-- @
--
-- Any ANSI colorisation in @doc@ will be output.
putDoc :: Doc -> IO ()
putDoc doc              = hPutDoc stdout doc

-- | @(hPutDoc handle doc)@ pretty prints document @doc@ to the file
-- handle @handle@ with a page width of 80 characters and a ribbon
-- width of 32 characters.
--
-- > main = do{ handle <- openFile "MyFile" WriteMode
-- >          ; hPutDoc handle (vcat (map text
-- >                            ["vertical","text"]))
-- >          ; hClose handle
-- >          }
--
-- Any ANSI colorisation in @doc@ will be output.
hPutDoc :: Handle -> Doc -> IO ()
hPutDoc handle doc = displayIO handle $ renderPretty 0.4 80 doc

--  LocalWords:  PPrint combinators Wadler Wadler's encloseSep
