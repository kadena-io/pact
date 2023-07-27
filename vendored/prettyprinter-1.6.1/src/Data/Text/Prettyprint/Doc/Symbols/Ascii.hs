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
squote = Char '\''

-- | >>> dquote
-- "
dquote :: Doc ann
dquote = Char '"'

-- | >>> lparen
-- (
lparen :: Doc ann
lparen = Char '('

-- | >>> rparen
-- )
rparen :: Doc ann
rparen = Char ')'

-- | >>> langle
-- <
langle :: Doc ann
langle = Char '<'

-- | >>> rangle
-- >
rangle :: Doc ann
rangle = Char '>'

-- | >>> lbracket
-- [
lbracket :: Doc ann
lbracket = Char '['
-- | >>> rbracket
-- ]
rbracket :: Doc ann
rbracket = Char ']'

-- | >>> lbrace
-- {
lbrace :: Doc ann
lbrace = Char '{'
-- | >>> rbrace
-- }
rbrace :: Doc ann
rbrace = Char '}'

-- | >>> semi
-- ;
semi :: Doc ann
semi = Char ';'

-- | >>> colon
-- :
colon :: Doc ann
colon = Char ':'

-- | >>> comma
-- ,
comma :: Doc ann
comma = Char ','

-- | >>> "a" <> space <> "b"
-- a b
--
-- This is mostly used via @'<+>'@,
--
-- >>> "a" <+> "b"
-- a b
space :: Doc ann
space = Char ' '

-- | >>> dot
-- .
dot :: Doc ann
dot = Char '.'

-- | >>> slash
-- /
slash :: Doc ann
slash = Char '/'

-- | >>> backslash
-- \\

backslash :: Doc ann
backslash = "\\"

-- | >>> equals
-- =
equals :: Doc ann
equals = Char '='

-- | >>> pipe
-- |
pipe :: Doc ann
pipe = Char '|'



-- $setup
--
-- (Definitions for the doctests)
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Semigroup
-- >>> import Data.Text.Prettyprint.Doc.Render.Text
-- >>> import Data.Text.Prettyprint.Doc.Util
