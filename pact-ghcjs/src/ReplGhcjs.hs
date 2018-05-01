{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- |
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
--

module Main where

------------------------------------------------------------------------------
import           Control.Monad.State.Strict
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Examples
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
  relLink "apple-touch-icon" "57x57" "img/favicon/apple-icon-57x57.png"
  relLink "apple-touch-icon" "60x60" "img/favicon/apple-icon-60x60.png"
  relLink "apple-touch-icon" "72x72" "img/favicon/apple-icon-72x72.png"
  relLink "apple-touch-icon" "76x76" "img/favicon/apple-icon-76x76.png"
  relLink "apple-touch-icon" "114x114" "img/favicon/apple-icon-114x114.png"
  relLink "apple-touch-icon" "120x120" "img/favicon/apple-icon-120x120.png"
  relLink "apple-touch-icon" "144x144" "img/favicon/apple-icon-144x144.png"
  relLink "apple-touch-icon" "152x152" "img/favicon/apple-icon-152x152.png"
  relLink "apple-touch-icon" "180x180" "img/favicon/apple-icon-180x180.png"
  let relLink2 s h = elAttr "link" ("rel" =: "icon" <> "type" =: "image/png" <> "sizes" =: s <> "href" =: h) blank
  relLink2 "192x192" "img/favicon/android-icon-192x192.png"
  elAttr "link" ("rel" =: "manifest" <> "href" =: "img/favicon/manifest.json") blank
  elAttr "meta" ("name" =: "msapplication-TileColor" <> "content" =: "#ffffff") blank
  elAttr "meta" ("name" =: "msapplication-TileImage" <> "content" =: "ms-icon-144x144.png") blank
  elAttr "meta" ("name" =: "theme-color" <> "content" =: "#ffffff") blank

  elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: "http://fonts.googleapis.com/css?family=Roboto:300,600") blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "http://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css") blank
  elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: "css/index.css") blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "css/font-awesome.min.css") blank

  --let js s = elAttr "script" ("type" =: "text/javascript"<> "src" =: s) blank


data ControlOut t = ControlOut
    { controlDropdown  :: Dropdown t Int
    , controlLoadEvent :: Event t ()
    }

app :: MonadWidget t m => m ()
app = do
    ControlOut d le <- controlBar
    elAttr "div" ("id" =: "editor_view") $ do
      let getDemo = snd . (demos M.!)
      let changeExample = getDemo <$> updated (value d)
      code <- elAttr "div" ("id" =: "column1") $ codeWidget hello changeExample
      elAttr "div" ("id" =: "separator" <> "class" =: "separator") blank
      elAttr "div" ("id" =: "column2") $ replWidget $ tag (current code) le

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

replWidget
    :: MonadWidget t m
    => Event t Text
    -> m ()
replWidget newCode = do
    initState <- liftIO $ initReplState Interactive
    let runInput input snippets =
          snippets <> S.singleton (InputSnippet input)

    elAttr "div" ("id" =: "code") $ mdo
      let start = (Left "init", initState)
      replState <- holdDyn start $ leftmost
        [ evalResult
        , start <$ newCode
        ]
      snippets <- holdUniqDyn =<< foldDyn ($) dummyData (leftmost
        [ runInput <$> newInput
        , (\r ss -> ss <> S.singleton (OutputSnippet (showResult $ fst r))) <$> evalResult
        , const dummyData <$ newCode
        ])
      _ <- dyn $ mapM_ snippetWidget <$> snippets
      (ti, goEvent) <- divClass "repl-input-controls" $ mdo
        ti <- textArea $ def
          & attributes .~ constDyn mempty -- ("class" =: "repl-input")
          & setValue .~ (mempty <$ buttonClicked)
        (b,_) <- el' "button" $ text "Evaluate"
        let buttonClicked = domEvent Click b
        return (ti, buttonClicked)
      let newInput = tag (current $ value ti) goEvent
      evalResult <- performEvent $ leftmost
        [ attachWith runExpr (current replState) newInput
        , attachPromptlyDynWith runExpr replState newCode
        ]
      return ()

runExpr
    :: MonadIO m
    => (Either String (Term Name), ReplState)
    -> Text
    -> m (Either String (Term Name), ReplState)
runExpr (_,s) e = liftIO $ runStateT (evalRepl' $ T.unpack e) s

myEvalString :: Text -> IO Text
myEvalString cmd = do
  (er,_) <- initReplState StringEval >>= runStateT (evalRepl' $ T.unpack cmd)
  return $ case er of
    (Right v) -> T.pack $ show v
    (Left e) -> "Error: " <> T.pack e

showResult :: Show a => Either String a -> Text
showResult (Right v) = T.pack $ show v
showResult (Left e) = "Error: " <> T.pack e

demos :: Map Int (Text, Text)
demos = M.fromList $ zip [0..] exampleData

controlBar :: MonadWidget t m => m (ControlOut t)
controlBar = do
    elAttr "div" ("id" =: "options") $ do
      elAttr "div" ("style" =: "display: block;") $ do
        o <- elAttr "ul" ("class" =: "view_mode") $ do
          d <- elAttr "li" ("class" =: "code-example-dropdown") $ do
            elAttr "fieldset" ("class" =: "code_examples_fieldset") $ do
              elAttr "li" ("id" =: "code_examples_fieldset") $ do
                dropdown 0 (constDyn $ fmap fst demos) def
          load <- elAttr "li" ("style" =: "padding-left: 10px;" <> "class" =: "tooltip") $ do
            (e,_) <- elAttr' "label" ("id" =: "compile_now") $ text "Load"
            return $ domEvent Click e
            --elClass "span" "tooltiptext" $ text "Click or Shift-Enter"
          return (ControlOut d load)
        elAttr "ul" ("class" =: "view_mode" <> "style" =: "float: right") $ do
          elAttr "img" ("src" =: "img/kadena-logo84x20.png" <> "class" =: "logo-image") blank
        return o
