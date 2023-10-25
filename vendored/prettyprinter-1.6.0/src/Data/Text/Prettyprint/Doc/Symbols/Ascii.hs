{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Common symbols composed out of the ASCII subset of Unicode. For non-ASCII
-- symbols, see "Data.Text.Prettyprint.Doc.Symbols.Unicode".
module Data.Text.Prettyprint.Doc.Symbols.Ascii where



import Data.Text.Prettyprint.Doc.Internal



-- | >>> squotes "·"
-- '·'
squotes :: Doc ann -> Doc ann
squotes = enclose squote squote

-- | >>> dquotes "·"
-- "·"
dquotes :: Doc ann -> Doc ann
dquotes = enclose dquote dquote

-- | >>> parens "·"
-- (·)
parens :: Doc ann -> Doc ann
parens = enclose lparen rparen

-- | >>> angles "·"
-- <·>
angles :: Doc ann -> Doc ann
angles = enclose langle rangle

-- | >>> brackets "·"
-- [·]
brackets :: Doc ann -> Doc ann
brackets = enclose lbracket rbracket

-- | >>> braces "·"
-- {·}
braces :: Doc ann -> Doc ann
braces = enclose lbrace rbrace

-- | >>> squote
-- '
squote :: Doc ann
squote = "'"

-- | >>> dquote
-- "
dquote :: Doc ann
dquote = "\""

-- | >>> lparen
-- (
lparen :: Doc ann
lparen = "("

-- | >>> rparen
-- )
rparen :: Doc ann
rparen = ")"

-- | >>> langle
-- <
langle :: Doc ann
langle = "<"

-- | >>> rangle
-- >
rangle :: Doc ann
rangle = ">"

-- | >>> lbracket
-- [
lbracket :: Doc ann
lbracket = "["
-- | >>> rbracket
-- ]
rbracket :: Doc ann
rbracket = "]"

-- | >>> lbrace
-- {
lbrace :: Doc ann
lbrace = "{"
-- | >>> rbrace
-- }
rbrace :: Doc ann
rbrace = "}"

-- | >>> semi
-- ;
semi :: Doc ann
semi = ";"

-- | >>> colon
-- :
colon :: Doc ann
colon = ":"

-- | >>> comma
-- ,
comma :: Doc ann
comma = ","

-- | >>> "a" <> space <> "b"
-- a b
--
-- This is mostly used via @'<+>'@,
--
-- >>> "a" <+> "b"
-- a b
space :: Doc ann
space = " "

-- | >>> dot
-- .
dot :: Doc ann
dot = "."

-- | >>> slash
-- /
slash :: Doc ann
slash = "/"

-- | >>> backslash
-- \\

backslash :: Doc ann
backslash = "\\"

-- | >>> equals
-- =
equals :: Doc ann
equals = "="

-- | >>> pipe
-- |
pipe :: Doc ann
pipe = "|"



-- $setup
--
-- (Definitions for the doctests)
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Semigroup
-- >>> import Data.Text.Prettyprint.Doc.Render.Text
-- >>> import Data.Text.Prettyprint.Doc.Util
