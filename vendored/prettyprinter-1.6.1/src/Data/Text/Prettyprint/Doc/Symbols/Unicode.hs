-- | A collection of predefined Unicode values outside of ASCII range. For
-- ASCII, see "Data.Text.Prettyprint.Doc.Symbols.Ascii".
module Data.Text.Prettyprint.Doc.Symbols.Unicode (
    -- * Quotes

    -- ** Enclosing
    d9966quotes,
    d6699quotes,
    s96quotes,
    s69quotes,
    dGuillemetsOut,
    dGuillemetsIn,
    sGuillemetsOut,
    sGuillemetsIn,

    -- ** Standalone
    b99dquote,
    t66dquote,
    t99dquote,
    b9quote,
    t6quote,
    t9quote,

    rdGuillemet,
    ldGuillemet,
    rsGuillemet,
    lsGuillemet,

    -- * Various typographical symbols
    bullet,
    endash,

    -- * Currencies
    euro,
    cent,
    yen,
    pound,
) where



import Data.Text.Prettyprint.Doc.Internal



-- | Double „99-66“ quotes, as used in German typography.
--
-- >>> putDoc (d9966quotes "·")
-- „·“
d9966quotes :: Doc ann -> Doc ann
d9966quotes = enclose b99dquote t66dquote

-- | Double “66-99” quotes, as used in English typography.
--
-- >>> putDoc (d6699quotes "·")
-- “·”
d6699quotes :: Doc ann -> Doc ann
d6699quotes = enclose t66dquote t99dquote

-- | Single ‚9-6‘ quotes, as used in German typography.
--
-- >>> putDoc (s96quotes "·")
-- ‚·‘
s96quotes :: Doc ann -> Doc ann
s96quotes = enclose b9quote t6quote

-- | Single ‘6-9’ quotes, as used in English typography.
--
-- >>> putDoc (s69quotes "·")
-- ‘·’
s69quotes :: Doc ann -> Doc ann
s69quotes = enclose t6quote t9quote

-- | Double «guillemets», pointing outwards (without adding any spacing).
--
-- >>> putDoc (dGuillemetsOut "·")
-- «·»
dGuillemetsOut :: Doc ann -> Doc ann
dGuillemetsOut = enclose ldGuillemet rdGuillemet

-- | Double »guillemets«, pointing inwards (without adding any spacing).
--
-- >>> putDoc (dGuillemetsIn "·")
-- »·«
dGuillemetsIn :: Doc ann -> Doc ann
dGuillemetsIn = enclose rdGuillemet ldGuillemet

-- | Single ‹guillemets›, pointing outwards (without adding any spacing).
--
-- >>> putDoc (sGuillemetsOut "·")
-- ‹·›
sGuillemetsOut :: Doc ann -> Doc ann
sGuillemetsOut = enclose lsGuillemet rsGuillemet

-- | Single ›guillemets‹, pointing inwards (without adding any spacing).
--
-- >>> putDoc (sGuillemetsIn "·")
-- ›·‹
sGuillemetsIn :: Doc ann -> Doc ann
sGuillemetsIn = enclose rsGuillemet lsGuillemet

-- | Bottom „99“ style double quotes.
--
-- >>> putDoc b99dquote
-- „
b99dquote :: Doc ann
b99dquote = Char '„'

-- | Top “66” style double quotes.
--
-- >>> putDoc t66dquote
-- “
t66dquote :: Doc ann
t66dquote = Char '“'

-- | Top “99” style double quotes.
--
-- >>> putDoc t99dquote
-- ”
t99dquote :: Doc ann
t99dquote = Char '”'

-- | Bottom ‚9‘ style single quote.
--
-- >>> putDoc b9quote
-- ‚
b9quote :: Doc ann
b9quote = Char '‚'

-- | Top ‘66’ style single quote.
--
-- >>> putDoc t6quote
-- ‘
t6quote :: Doc ann
t6quote = Char '‘'

-- | Top ‘9’ style single quote.
--
-- >>> putDoc t9quote
-- ’
t9quote :: Doc ann
t9quote = Char '’'

-- | Right-pointing double guillemets
--
-- >>> putDoc rdGuillemet
-- »
rdGuillemet :: Doc ann
rdGuillemet = Char '»'

-- | Left-pointing double guillemets
--
-- >>> putDoc ldGuillemet
-- «
ldGuillemet :: Doc ann
ldGuillemet = Char '«'

-- | Right-pointing single guillemets
--
-- >>> putDoc rsGuillemet
-- ›
rsGuillemet :: Doc ann
rsGuillemet = Char '›'

-- | Left-pointing single guillemets
--
-- >>> putDoc lsGuillemet
-- ‹
lsGuillemet :: Doc ann
lsGuillemet = Char '‹'

-- | >>> putDoc bullet
-- •
bullet :: Doc ann
bullet = Char '•'

-- | >>> putDoc endash
-- –
endash :: Doc ann
endash = Char '–'

-- | >>> putDoc euro
-- €
euro :: Doc ann
euro = Char '€'

-- | >>> putDoc cent
-- ¢
cent :: Doc ann
cent = Char '¢'

-- | >>> putDoc yen
-- ¥
yen :: Doc ann
yen = Char '¥'

-- | >>> putDoc pound
-- £
pound :: Doc ann
pound = Char '£'



-- $setup
--
-- (Definitions for the doctests)
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Text.Prettyprint.Doc.Render.Text
-- >>> import Data.Text.Prettyprint.Doc.Util
