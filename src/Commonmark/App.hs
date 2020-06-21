-- |

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Commonmark.App where

import qualified Commonmark
import Control.Arrow
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import qualified Data.Functor.Identity as Identity
import qualified Commonmark.Extensions                  as Commonmark
import qualified Commonmark.Html
import qualified Control.Applicative                    as Applicative
import           Control.Concurrent                     (threadDelay)
import qualified Control.Concurrent                     as Concurrent
import           Control.Concurrent.STM                 as STM
import           Control.Concurrent.STM.TChan           as TChan
import           Control.Lens                           ((^.),(%~))
import           Control.Lens                           ((&))
import qualified Control.Monad                          as Monad
import Text.Read (readMaybe)
import qualified Text.Parsec.Pos as Pos
import qualified Control.Monad.Fix                      as Fix
import qualified Control.Monad.IO.Class                 as IO
import qualified Data.Text                              as Text
import qualified Data.Text.Lazy                         as T
import qualified GHCJS.DOM.HTMLTextAreaElement              as DOM
import qualified GHCJS.DOM.HTMLElement              as DOM
import qualified Language.Javascript.JSaddle            as JSaddle
import           Reflex.Dom.Core

#ifndef ghcjs_HOST_OS
import qualified Language.Javascript.JSaddle.Debug      as JSaddle
import qualified Language.Javascript.JSaddle.WebSockets as JSaddle
#endif

import Commonmark.Editor
import Commonmark.Viewer

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
   , DomBuilderSpace m ~ GhcjsDomSpace
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

  rec
    e@Editor {contentUpdates} <- elAttr "div"
      ("class" =: "pad10" <>
       "style" =: grid "2" "1"
      ) $ do
      e <- editor (EditorConfig { initialContent = "", sourceMap, setSelection = elClicks })
      display $ cursorN e
      display $ cursorXY e
      return e

    elAttr "div" ("class" =: "pad10" <> "style" =: grid "1" "2") (text "Rendered")
    Viewer{renders,elClicks} <- elAttr "div" ("class" =: "pad10" <> "style" =: grid "2" "2") $
         viewer (ViewerConfig { renderMarkdown = contentUpdates, initialMarkdown = "", renderSpec = syntax})
         (Just <$> cursorXY e)
    sourceMap <- holdDyn mempty (snd <$> renders)

    syntax <- elAttr "div" ("class" =: "pad10" <> "style" =: grid "3" "1") $
      customSyntaxPicker

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
