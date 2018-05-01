{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
--

module Main where

------------------------------------------------------------------------------
import           Control.Monad.State.Strict
import           Control.Monad.Trans
import           Data.Char
import           Data.Monoid
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Pact.ReplCommon
import           Pact.Repl.Types
import           Pact.Types.Lang
------------------------------------------------------------------------------

main :: IO ()
main = mainWidgetWithHead headWidget app

headWidget :: MonadWidget t m => m ()
headWidget = do
  el "title" $ text "Try Pact In The Browser!"
  elAttr "meta" ("content" =: "text/html;charset=utf-8" <> "hhtp-equiv" =: "Content-Type") blank
  elAttr "meta" ("content" =: "utf-8" <> "hhtp-equiv" =: "enconding") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0") blank
  elAttr "meta" ("http-equiv" =: "CacheControl" <> "content" =: "no-cache, no-store, must-revalidate") blank
  elAttr "meta" ("http-equiv" =: "Pragma" <> "content" =: "no-cache") blank
  elAttr "meta" ("http-equiv" =: "Expires" <> "content" =: "0") blank
  let relLink r s h = elAttr "link" ("rel" =: r <> "sizes" =: s <> "href" =: h) blank
  relLink "apple-touch-icon" "57x57" "/img/favicon/apple-icon-57x57.png"
  relLink "apple-touch-icon" "60x60" "/img/favicon/apple-icon-60x60.png"
  relLink "apple-touch-icon" "72x72" "/img/favicon/apple-icon-72x72.png"
  relLink "apple-touch-icon" "76x76" "/img/favicon/apple-icon-76x76.png"
  relLink "apple-touch-icon" "114x114" "/img/favicon/apple-icon-114x114.png"
  relLink "apple-touch-icon" "120x120" "/img/favicon/apple-icon-120x120.png"
  relLink "apple-touch-icon" "144x144" "/img/favicon/apple-icon-144x144.png"
  relLink "apple-touch-icon" "152x152" "/img/favicon/apple-icon-152x152.png"
  relLink "apple-touch-icon" "180x180" "/img/favicon/apple-icon-180x180.png"
  let relLink2 s h = elAttr "link" ("rel" =: "icon" <> "type" =: "image/png" <> "sizes" =: s <> "href" =: h) blank
  relLink2 "192x192" "/img/favicon/android-icon-192x192.png"
  relLink2 "32x32" "/img/favicon/android-icon-32x32.png"
  relLink2 "96x96" "/img/favicon/android-icon-96x96.png"
  relLink2 "16x16" "/img/favicon/android-icon-16x16.png"
  elAttr "link" ("rel" =: "manifest" <> "href" =: "/img/favicon/manifest.json") blank
  elAttr "meta" ("name" =: "msapplication-TileColor" <> "content" =: "#ffffff") blank
  elAttr "meta" ("name" =: "msapplication-TileImage" <> "content" =: "/ms-icon-144x144.png") blank
  elAttr "meta" ("name" =: "theme-color" <> "content" =: "#ffffff") blank

  elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: "http://fonts.googleapis.com/css?family=Roboto:300,600") blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "http://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css") blank
  elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: "css/index.css") blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "css/font-awesome.min.css") blank

  let js s = elAttr "script" ("type" =: "text/javascript"<> "src" =: s) blank
  js "http://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.8.3/underscore-min.js"
  js "http://cdnjs.cloudflare.com/ajax/libs/jquery/1.10.2/jquery.js"
  js "https://code.jquery.com/ui/1.12.1/jquery-ui.js"
  js "http://cdnjs.cloudflare.com/ajax/libs/ace/1.2.5/ace.js"
  js "js/mode-pact.js"
  js "http://cdnjs.cloudflare.com/ajax/libs/ace/1.2.5/theme-solarized_light.js"


app :: MonadWidget t m => m ()
app = do
    elAttr "div" ("id" =: "editor_view") $ do
      code <- elAttr "div" ("id" =: "column1") $ codeWidget iv never
      elAttr "div" ("id" =: "separator" <> "class" =: "separator") blank
      elAttr "div" ("id" =: "column2") replWidget
  where
    iv = ";; Simple accounts model"

codeWidget :: MonadWidget t m => Text -> Event t Text -> m (Dynamic t Text)
codeWidget iv sv = do
    elAttr "div" ("id" =: "code") $ do
      ta <- textArea $ def
        & textAreaConfig_initialValue .~ iv
        & textAreaConfig_setValue .~ sv
        & textAreaConfig_attributes .~ constDyn ("class" =: "code-input")
      return $ value ta

data DisplayedSnippet
  = InputSnippet Text
  | OutputSnippet Text
  deriving (Eq,Ord,Show,Read)

dummyData :: Seq DisplayedSnippet
dummyData = S.fromList
      [ OutputSnippet ";; Welcome to the Pact interactive repl"
      , OutputSnippet ";; Enter pact commands here"
      ]

snippetWidget :: MonadWidget t m => DisplayedSnippet -> m ()
snippetWidget (InputSnippet t) = el "pre" $ text t
snippetWidget (OutputSnippet t) = el "pre" $ text t

replWidget :: MonadWidget t m => m ()
replWidget = do
    initState <- liftIO $ initReplState Interactive
    let runInput input snippets =
          snippets <> S.singleton (InputSnippet input) -- <> runExpr input

    elAttr "div" ("id" =: "code") $ mdo
      replState <- holdDyn (Left "init", initState) evalResult
      snippets <- holdUniqDyn =<< foldDyn ($) dummyData (leftmost
        [ runInput <$> newInput
        , (\r ss -> ss <> S.singleton (OutputSnippet (showResult $ fst r))) <$> evalResult
        ])
      --history <- foldDyn ($) emptyHistory $
      --  (addHistoryUnlessConsecutiveDupe <$> newInput)
      dyn $ mapM_ snippetWidget <$> snippets
      ti <- textArea $ def
        & attributes .~ constDyn mempty -- ("class" =: "repl-input")
        & setValue .~ (mempty <$ enterPressed)
      (b,_) <- el' "button" $ text "Evaluate"
      let enterPressed = domEvent Click b
      let newInput = tag (current $ value ti) enterPressed
      --let filterKey k = ffilter (==k) $ _textArea_keydown ti
      --    enterPressed = filterKey 13
      --    upPressed = filterKey 38
      --    downPressed = filterKey 40
      --evalResult <- performEvent (liftIO . myEvalString <$> newInput)
      evalResult <- performEvent (attachWith runExpr (current replState) newInput)
      return ()

runExpr
    :: MonadIO m
    => (Either String (Term Name), ReplState)
    -> Text
    -> m (Either String (Term Name), ReplState)
runExpr (a,s) e = liftIO $ runStateT (evalRepl' $ T.unpack e) s

myEvalString :: Text -> IO Text
myEvalString cmd = do
  (er,s) <- initReplState StringEval >>= runStateT (evalRepl' $ T.unpack cmd)
  return $ case er of
    (Right v) -> T.pack $ show v
    (Left e) -> "Error: " <> T.pack e

showResult (Right v) = T.pack $ show v
showResult (Left e) = "Error: " <> T.pack e
