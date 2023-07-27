{-# LANGUAGE OverloadedStrings #-}

-- | This module is adapted from the package prettyprinter-ansi-terminal-1.1.2
-- to work with prettyprinter-1.6.1 in the context of Pact.
--
-- prettyprinter-ansi-terminal-1.1.2 is Copyright 2008, Daan Leijen and Max
-- Bolingbroke, 2016 David Luposchainsky.
--
module Data.Text.Prettyprint.Doc.Render.Terminal (
    -- * Styling
    AnsiStyle(..),
    Color(..),

    -- ** Font color
    color, colorDull,

    -- ** Background color
    bgColor, bgColorDull,

    -- ** Font style
    bold, italicized, underlined,

    -- ** Internal markers
    Intensity(..),
    Bold(..),
    Underlined(..),
    Italicized(..),

    -- * Conversion to ANSI-infused 'Text'
    renderLazy, renderStrict,

    -- * Render directly to 'stdout'
    renderIO,

    -- ** Convenience functions
    putDoc, hPutDoc,
) where



import           Control.Applicative
import           Data.IORef
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified System.Console.ANSI    as ANSI
import           System.IO              (Handle, hPutChar, stdout)

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Util.Panic

-- | The 8 ANSI terminal colors.
data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    deriving (Eq, Ord, Show)

-- | Dull or vivid coloring, as supported by ANSI terminals.
data Intensity = Vivid | Dull
    deriving (Eq, Ord, Show)

-- | Foreground (text) or background (paper) color
data Layer = Foreground | Background
    deriving (Eq, Ord, Show)

data Bold       = Bold       deriving (Eq, Ord, Show)
data Underlined = Underlined deriving (Eq, Ord, Show)
data Italicized = Italicized deriving (Eq, Ord, Show)

-- | Style the foreground with a vivid color.
color :: Color -> AnsiStyle
color c = mempty { ansiForeground = Just (Vivid, c) }

-- | Style the background with a vivid color.
bgColor :: Color -> AnsiStyle
bgColor c =  mempty { ansiBackground = Just (Vivid, c) }

-- | Style the foreground with a dull color.
colorDull :: Color -> AnsiStyle
colorDull c =  mempty { ansiForeground = Just (Dull, c) }

-- | Style the background with a dull color.
bgColorDull :: Color -> AnsiStyle
bgColorDull c =  mempty { ansiBackground = Just (Dull, c) }

-- | Render in __bold__.
bold :: AnsiStyle
bold = mempty { ansiBold = Just Bold }

-- | Render in /italics/.
italicized :: AnsiStyle
italicized = mempty { ansiItalics = Just Italicized }

-- | Render underlined.
underlined :: AnsiStyle
underlined = mempty { ansiUnderlining = Just Underlined }

-- | @('renderLazy' doc)@ takes the output @doc@ from a rendering function
-- and transforms it to lazy text, including ANSI styling directives for things
-- like colorization.
--
-- ANSI color information will be discarded by this function unless you are
-- running on a Unix-like operating system. This is due to a technical
-- limitation in Windows ANSI support.
--
-- With a bit of trickery to make the ANSI codes printable, here is an example
-- that would render colored in an ANSI terminal:
--
-- >>> let render = TL.putStrLn . TL.replace "\ESC" "\\e" . renderLazy . layoutPretty defaultLayoutOptions
-- >>> let doc = annotate (color Red) ("red" <+> align (vsep [annotate (color Blue <> underlined) ("blue+u" <+> annotate bold "bold" <+> "blue+u"), "red"]))
-- >>> render (unAnnotate doc)
-- red blue+u bold blue+u
--     red
-- >>> render doc
-- \e[0;91mred \e[0;94;4mblue+u \e[0;94;1;4mbold\e[0;94;4m blue+u\e[0;91m
--     red\e[0m
--
-- Run the above via @echo -e '...'@ in your terminal to see the coloring.
renderLazy :: SimpleDocStream AnsiStyle -> TL.Text
renderLazy =
    let push x = (x :)

        unsafePeek []    = panicPeekedEmpty
        unsafePeek (x:_) = x

        unsafePop []     = panicPoppedEmpty
        unsafePop (x:xs) = (x, xs)

        go :: [AnsiStyle] -> SimpleDocStream AnsiStyle -> TLB.Builder
        go s sds = case sds of
            SFail -> panicUncaughtFail
            SEmpty -> mempty
            SChar c rest -> TLB.singleton c <> go s rest
            SText _ t rest -> TLB.fromText t <> go s rest
            SLine i rest -> TLB.singleton '\n' <> TLB.fromText (T.replicate i " ") <> go s rest
            SAnnPush style rest ->
                let currentStyle = unsafePeek s
                    newStyle = style <> currentStyle
                in  TLB.fromText (styleToRawText newStyle) <> go (push style s) rest
            SAnnPop rest ->
                let (_currentStyle, s') = unsafePop s
                    newStyle = unsafePeek s'
                in  TLB.fromText (styleToRawText newStyle) <> go s' rest

    in  TLB.toLazyText . go [mempty]


-- | @('renderIO' h sdoc)@ writes @sdoc@ to the handle @h@.
--
-- >>> let render = renderIO System.IO.stdout . layoutPretty defaultLayoutOptions
-- >>> let doc = annotate (color Red) ("red" <+> align (vsep [annotate (color Blue <> underlined) ("blue+u" <+> annotate bold "bold" <+> "blue+u"), "red"]))
--
-- We render the 'unAnnotate'd version here, since the ANSI codes don’t display
-- well in Haddock,
--
-- >>> render (unAnnotate doc)
-- red blue+u bold blue+u
--     red
--
-- This function behaves just like
--
-- @
-- 'renderIO' h sdoc = 'TL.hPutStr' h ('renderLazy' sdoc)
-- @
--
-- but will not generate any intermediate text, rendering directly to the
-- handle.
renderIO :: Handle -> SimpleDocStream AnsiStyle -> IO ()
renderIO h sdoc = do
    styleStackRef <- newIORef [mempty]

    let push x = modifyIORef' styleStackRef (x :)
        unsafePeek = readIORef styleStackRef >>= \tok -> case tok of
            [] -> panicPeekedEmpty
            x:_ -> pure x
        unsafePop = readIORef styleStackRef >>= \tok -> case tok of
            [] -> panicPoppedEmpty
            x:xs -> writeIORef styleStackRef xs >> pure x

    let go = \sds -> case sds of
            SFail -> panicUncaughtFail
            SEmpty -> pure ()
            SChar c rest -> do
                hPutChar h c
                go rest
            SText _ t rest -> do
                T.hPutStr h t
                go rest
            SLine i rest -> do
                hPutChar h '\n'
                T.hPutStr h (T.replicate i (T.singleton ' '))
                go rest
            SAnnPush style rest -> do
                currentStyle <- unsafePeek
                let newStyle = style <> currentStyle
                push newStyle
                T.hPutStr h (styleToRawText newStyle)
                go rest
            SAnnPop rest -> do
                _currentStyle <- unsafePop
                newStyle <- unsafePeek
                T.hPutStr h (styleToRawText newStyle)
                go rest
    go sdoc
    readIORef styleStackRef >>= \stack -> case stack of
        []  -> panicStyleStackFullyConsumed
        [_] -> pure ()
        xs  -> panicStyleStackNotFullyConsumed (length xs)

panicStyleStackFullyConsumed :: void
panicStyleStackFullyConsumed
  = error ("There is no empty style left at the end of rendering" ++
           " (but there should be). Please report this as a bug.")

panicStyleStackNotFullyConsumed :: Int -> void
panicStyleStackNotFullyConsumed len
  = error ("There are " <> show len <> " styles left at the" ++
           "end of rendering (there should be only 1). Please report" ++
           " this as a bug.")

-- $
-- >>> let render = renderIO System.IO.stdout . layoutPretty defaultLayoutOptions
-- >>> let doc = annotate (color Red) ("red" <+> align (vsep [annotate (color Blue <> underlined) ("blue+u" <+> annotate bold "bold" <+> "blue+u"), "red"]))
-- >>> render (unAnnotate doc)
-- red blue+u bold blue+u
--     red
--
-- This test won’t work since I don’t know how to type \ESC for doctest :-/
-- -- >>> render doc
-- -- \ESC[0;91mred \ESC[0;94;4mblue+u \ESC[0;94;1;4mbold\ESC[0;94;4m blue+u\ESC[0;91m
-- --     red\ESC[0m

-- | Render the annotated document in a certain style. Styles not set in the
-- annotation will use the style of the surrounding document, or the terminal’s
-- default if none has been set yet.
--
-- @
-- style = 'color' 'Green' '<>' 'bold'
-- styledDoc = 'annotate' style "hello world"
-- @
data AnsiStyle = SetAnsiStyle
    { ansiForeground  :: Maybe (Intensity, Color) -- ^ Set the foreground color, or keep the old one.
    , ansiBackground  :: Maybe (Intensity, Color) -- ^ Set the background color, or keep the old one.
    , ansiBold        :: Maybe Bold               -- ^ Switch on boldness, or don’t do anything.
    , ansiItalics     :: Maybe Italicized         -- ^ Switch on italics, or don’t do anything.
    , ansiUnderlining :: Maybe Underlined         -- ^ Switch on underlining, or don’t do anything.
    } deriving (Eq, Ord, Show)

-- | Keep the first decision for each of foreground color, background color,
-- boldness, italication, and underlining. If a certain style is not set, the
-- terminal’s default will be used.
--
-- Example:
--
-- @
-- 'color' 'Red' '<>' 'color' 'Green'
-- @
--
-- is red because the first color wins, and not bold because (or if) that’s the
-- terminal’s default.
instance Semigroup AnsiStyle where
    cs1 <> cs2 = SetAnsiStyle
        { ansiForeground  = ansiForeground  cs1 <|> ansiForeground  cs2
        , ansiBackground  = ansiBackground  cs1 <|> ansiBackground  cs2
        , ansiBold        = ansiBold        cs1 <|> ansiBold        cs2
        , ansiItalics     = ansiItalics     cs1 <|> ansiItalics     cs2
        , ansiUnderlining = ansiUnderlining cs1 <|> ansiUnderlining cs2 }

-- | 'mempty' does nothing, which is equivalent to inheriting the style of the
-- surrounding doc, or the terminal’s default if no style has been set yet.
instance Monoid AnsiStyle where
    mempty = SetAnsiStyle Nothing Nothing Nothing Nothing Nothing
    mappend = (<>)

styleToRawText :: AnsiStyle -> Text
styleToRawText = T.pack . ANSI.setSGRCode . stylesToSgrs
  where
    stylesToSgrs :: AnsiStyle -> [ANSI.SGR]
    stylesToSgrs (SetAnsiStyle fg bg b i u) = catMaybes
        [ Just ANSI.Reset
        , fmap (\(intensity, c) -> ANSI.SetColor ANSI.Foreground (convertIntensity intensity) (convertColor c)) fg
        , fmap (\(intensity, c) -> ANSI.SetColor ANSI.Background (convertIntensity intensity) (convertColor c)) bg
        , fmap (\_              -> ANSI.SetConsoleIntensity ANSI.BoldIntensity) b
        , fmap (\_              -> ANSI.SetItalicized True) i
        , fmap (\_              -> ANSI.SetUnderlining ANSI.SingleUnderline) u
        ]

    convertIntensity :: Intensity -> ANSI.ColorIntensity
    convertIntensity = \i -> case i of
        Vivid -> ANSI.Vivid
        Dull  -> ANSI.Dull

    convertColor :: Color -> ANSI.Color
    convertColor = \c -> case c of
        Black   -> ANSI.Black
        Red     -> ANSI.Red
        Green   -> ANSI.Green
        Yellow  -> ANSI.Yellow
        Blue    -> ANSI.Blue
        Magenta -> ANSI.Magenta
        Cyan    -> ANSI.Cyan
        White   -> ANSI.White



-- | @('renderStrict' sdoc)@ takes the output @sdoc@ from a rendering and
-- transforms it to strict text.
renderStrict :: SimpleDocStream AnsiStyle -> Text
renderStrict = TL.toStrict . renderLazy

-- | @('putDoc' doc)@ prettyprints document @doc@ to standard output using
-- 'defaultLayoutOptions'.
--
-- >>> putDoc ("hello" <+> "world")
-- hello world
--
-- @
-- 'putDoc' = 'hPutDoc' 'stdout'
-- @
putDoc :: Doc AnsiStyle -> IO ()
putDoc = hPutDoc stdout

-- | Like 'putDoc', but instead of using 'stdout', print to a user-provided
-- handle, e.g. a file or a socket using 'defaultLayoutOptions'.
--
-- > main = withFile "someFile.txt" (\h -> hPutDoc h (vcat ["vertical", "text"]))
--
-- @
-- 'hPutDoc' h doc = 'renderIO' h ('layoutPretty' 'defaultLayoutOptions' doc)
-- @
hPutDoc :: Handle -> Doc AnsiStyle -> IO ()
hPutDoc h doc = renderIO h (layoutPretty defaultLayoutOptions doc)
