-- |

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Commonmark.Editor where

import qualified Commonmark
import qualified Commonmark.Extensions                  as Commonmark
import qualified Commonmark.Html
import qualified Control.Applicative                    as Applicative
import           Control.Concurrent                     (threadDelay)
import qualified Control.Concurrent                     as Concurrent
import           Control.Concurrent.STM                 as STM
import           Control.Concurrent.STM.TChan           as TChan
import           Control.Lens                           ((^.))
import           Control.Lens                           ((&))
import qualified Control.Monad                          as Monad
import qualified Control.Monad.Fix                      as Fix
import qualified Control.Monad.IO.Class                 as IO
import qualified Data.Text                              as Text
import qualified Data.Text.Lazy                         as T
import qualified Language.Javascript.JSaddle            as JSaddle
import           Reflex.Dom.Core

#ifndef ghcjs_HOST_OS
import qualified Language.Javascript.JSaddle.Debug      as JSaddle
import qualified Language.Javascript.JSaddle.WebSockets as JSaddle
#endif

customSyntax = mconcat
  [ Commonmark.mathSpec
  -- , Commonmark.emojiSpec  -- TODO: emojiSpec causes linking to hang
  , Commonmark.defaultSyntaxSpec
  ]

editor :: forall t m.(DomBuilder t m) => m (Event t Text.Text)
editor = divClass "commonmark-editor" $ do
  te <- textAreaElement $
        def & textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("rows" =: "20")
  return $ _textAreaElement_input te


data ViewerConfig t = ViewerConfig
  { renderMarkdown :: Event t Text.Text
  }


-- The plan is to return an event of SourcePositions from commonmark
-- that indicates click events on the render result. We could catch
-- these clicks to place the cursor at the corresponding part of
-- the comment editor. Not implemented.
data Viewer t = Viewer
  { renders :: Event t (Commonmark.Html Commonmark.SourceRange)
  }

render
  :: Text.Text
  -> Either Commonmark.ParseError (Commonmark.Html Commonmark.SourceRange)
render t = Monad.join $ Commonmark.commonmarkWith customSyntax "editor-text" t


-- | Viewer will do a MathJax.typeset() after each update.
--   To reduce the jank resulting from content reflowing as
--   a math div changes size during typesetting, we freeze
--   the previous render, and render the parse result anew
--   on each change into an invisible div, and ramp up opacity
--   after MathJax is done
viewer
  :: forall t m
  .( DomBuilder t m
   , TriggerEvent t m
   , PerformEvent t m
   , MonadHold t m
   , PostBuild t m
   , IO.MonadIO m
   , Fix.MonadFix m
   , IO.MonadIO (Performable m)
   , JSaddle.MonadJSM (Performable m)
   )
  => ViewerConfig t
  -> m (Viewer t)
viewer ViewerConfig{renderMarkdown} = divClass "commonmark-viewer" $ do
  -- TODO: debounce? throttle?
  html <- (fmap.fmap)  render $ debounce 0.5 =<< throttle 0.2 renderMarkdown
  numberedHtmls <- numberOccurrences html
  let

    drawFrame (Right r) = do
      pb <- getPostBuild
      startTypeset <- delay 0.1 pb
      finishTypeset <- performEvent $
        ffor startTypeset -- html -- renderMarkdown
        (\_ -> JSaddle.liftJSM $ do
            JSaddle.jsg ("MathJax" :: String) ^. JSaddle.js1 ("typeset" :: String) JSaddle.JSNull
            JSaddle.jsg ("console" :: String) ^. JSaddle.js1 ("log" :: String) ("typeset3" :: String)
        )
      hidden <- holdDyn True (False <$ finishTypeset)
      let attrs = ffor hidden $ \b ->
            ("class" =: "frame fade-in" <> "style" =: "background-color:white; width:100%;") <>
            (if False then ("hidden" =: "") else mempty)
      elDynAttr "div" attrs $
        toReflexDom (const (return ())) r
    drawFrame _ = text "render error" >> return never

    adjustments = ffor numberedHtmls $ \(i,r) ->
      i =: Just (drawFrame r)
      <> (i-2) =: Nothing

  stack <- elAttr "div" ("class" =: "frame-stack" <> "style" =: "position:relative;") $ do
    el "style" (text ".fade-in { animation: fadeIn ease 1s; }")
    el "style" (text ".frame { position: absolute; left: 0px; top: 0px;  }")
    el "style" (text "@keyframes fadeIn { 0% {opacity:0;}\n 100% {opacity:1}\n}")
    listWithKeyShallowDiff mempty adjustments (\k v _ -> v)

  return $ Viewer never



toReflexDom :: (DomBuilder t m, PostBuild t m) => (Text.Text -> m ()) -> Commonmark.Html a -> m (Event t ())
toReflexDom domPrefix node = case node of
  Commonmark.HtmlNull ->
    domPrefix "null: " >> return never
  Commonmark.HtmlConcat x y ->
    domPrefix "concat: " *>
    (Applicative.liftA2 (,)
     (toReflexDom domPrefix x)
     (toReflexDom domPrefix y) >>=
     \(a,b) -> return (leftmost [a,b])
    )
  Commonmark.HtmlRaw t ->
    domPrefix "raw: " >> text (t) >> return never
  Commonmark.HtmlText t ->
    domPrefix "text: " >> text (t) >> return never
  Commonmark.HtmlElement eltType tagname attrs mbcontents -> do
    domPrefix "element: "
    -- case lookup "xmlns" attrs of
    --   Nothing -> text "No xmlns"
    --   Just ns -> text ns
    elDynAttrNS (lookup "xmlns" attrs) tagname (constDyn $ mconcat $ (uncurry (=:)) <$> attrs) $ do
      case mbcontents of
        Nothing -> return never
        Just c  -> toReflexDom domPrefix c
    return never


#ifndef ghcjs_HOST_OS
debug = JSaddle.debug 8080 (mainWidgetWithHead header mainApp)
#endif

header :: DomBuilder t m => m ()
header = do
  elAttr "script"
    ("id" =: "MathJax-script"
     <> "async" =: ""
     <> "src" =: "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    ) (return ())
  el "style" (text css)


escapeHtml :: Text.Text -> Text.Text
escapeHtml t =
  case Text.uncons post of
    Just (c, rest) -> pre <> escapeHtmlChar c <> escapeHtml rest
    Nothing        -> pre
 where
  (pre,post)        = Text.break needsEscaping t
  needsEscaping '<' = True
  needsEscaping '>' = True
  needsEscaping '&' = True
  needsEscaping '"' = True
  needsEscaping _   = False
escapeHtmlChar :: Char -> Text.Text
escapeHtmlChar '<' = "&lt;"
escapeHtmlChar '>' = "&gt;"
escapeHtmlChar '&' = "&amp;"

mainApp
  :: forall t m
  .( DomBuilder t m
   , TriggerEvent t m
   , PerformEvent t m
   , MonadHold t m
   , PostBuild t m
   , IO.MonadIO m
   , Fix.MonadFix m
   , IO.MonadIO (Performable m)
   , JSaddle.MonadJSM (Performable m)
   )
  => m ()
mainApp = divClass "content" $ do
  let grid r c = Text.concat
        [ "grid-column-start: "
        , c
        , "; grid-column-end: span 1"
        , "; grid-row-start: "
        , r
        , "; grid-row-end: span 1"
        , ";"
        ]
  elAttr "div" ("class" =: "pad10" <> "style" =: grid "1" "1") (text "Markdown")
  markdownUpdates <- elAttr "div"
    ("class" =: "pad10" <>
     "style" =: grid "2" "1"
    ) $ editor
  elAttr "div" ("class" =: "pad10" <> "style" =: grid "1" "2") (text "Rendered")
  _ <- elAttr "div" ("class" =: "pad10" <> "style" =: grid "2" "2") $
       viewer (ViewerConfig { renderMarkdown = markdownUpdates })
  return ()

viewerCss :: Text.Text
viewerCss = mconcat
  [ ".commonmark-viewer {"
  , "  width:  100%;"
  , "  height: 100%;"
  , "}"
  , "body {"
  , "  background-color: hsla(0,0%,95%,1);"
  , "}"
  , ".pad10 {"
  , "  padding: 10px;"
  , "}"
  ]

editorCss :: Text.Text
editorCss = mconcat
  [ ".commonmark-editor {"
  , "  width:  80%;"
  , "  height: 100%;"
  , "}"
  , ".commonmark-editor > textarea {"
  , "  width: 100%;"
  , "}"
  ]

css :: Text.Text
css = Text.unlines
  [ ".content{"
  , "  display: grid;"
  , "}"
  ] <> editorCss <> viewerCss
