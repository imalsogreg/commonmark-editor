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

module Commonmark.Viewer where

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
import qualified JSDOM.HTMLTextAreaElement              as DOM
import qualified JSDOM.HTMLElement              as DOM
import qualified Language.Javascript.JSaddle            as JSaddle
import           Reflex.Dom.Core

data ViewerConfig t = ViewerConfig
  { renderMarkdown :: Event t Text.Text
  , initialMarkdown :: Text.Text
  }


customSyntax = mconcat
  [ Commonmark.mathSpec
  -- , Commonmark.emojiSpec  -- TODO: emojiSpec causes linking to hang
  , Commonmark.defaultSyntaxSpec
  ]

-- The plan is to return an event of SourcePositions from commonmark
-- that indicates click events on the render result. We could catch
-- these clicks to place the cursor at the corresponding part of
-- the comment editor. Not implemented.
data Viewer t = Viewer
  { renders  :: Event t (Commonmark.Html Commonmark.SourceRange, Commonmark.SourceMap)
  , elClicks :: Event t Commonmark.SourceRange
  }

render
  :: Text.Text
  -> Either Commonmark.ParseError (Commonmark.Html Commonmark.SourceRange, Commonmark.SourceMap)
render t =
  fmap (,mempty) $ Monad.join $ Commonmark.commonmarkWith customSyntax "editor-text" t

  -- Commonmark.runWithSourceMap <$> Identity.runIdentity (Commonmark.parseCommonmarkWith customSyntax (Commonmark.tokenize "editor-text" t))


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
  -> Dynamic t (Maybe (Int,Int))
  -> m (Viewer t)
viewer ViewerConfig{renderMarkdown, initialMarkdown} cursorPos = divClass "commonmark-viewer" $ do
  -- TODO: debounce? throttle?
  let initialHtml = render initialMarkdown
  -- html <- (fmap.fmap)  render $ debounce 0.5 =<< throttle 0.2 renderMarkdown
  html <- (fmap.fmap) render $ throttle 0.5 renderMarkdown
  numberedHtmls <- numberOccurrences html
  let

    -- drawFrame (Right (Commonmark.HtmlNull, _)) = text "(no content)" >> return never
    drawFrame (Right (r,sm)) = do
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
        case r of
          Commonmark.HtmlNull -> text "(no content)" >> return never
          _ -> toReflexDom (const (return ())) cursorPos r
    drawFrame _ = text "render error" >> return never

    adjustments = ffor numberedHtmls $ \(i, r) ->
      i =: Just (drawFrame r)
      <> (i-2) =: Nothing

  stack <- elAttr "div" ("class" =: "frame-stack" <> "style" =: "position:relative;") $ do
    el "style" (text ".fade-in { animation: fadeIn ease 1s; }")
    el "style" (text ".frame { position: absolute; left: 0px; top: 0px;  }")
    el "style" (text "@keyframes fadeIn { 0% {opacity:0;}\n 100% {opacity:1}\n}")
    listWithKeyShallowDiff (0 =: drawFrame initialHtml) adjustments (\k v _ -> v)

  let clicks = switchDyn $ fmap (leftmost . Map.elems) stack
  -- display =<< holdDyn " " (fmap (Text.pack . show) clicks)

  return $ Viewer never clicks


toReflexDom
  :: forall t m a.(DomBuilder t m, PostBuild t m)
  => (Text.Text -> m ())
  -> Dynamic t (Maybe (Int,Int))
  -> Commonmark.Html a
  -> m (Event t Commonmark.SourceRange)
toReflexDom domPrefix cursorPos node = case node of
  Commonmark.HtmlNull ->
    domPrefix "null: " >> return never
  Commonmark.HtmlConcat x y ->
    domPrefix "concat: " *>
    (Applicative.liftA2 (,)
     (toReflexDom domPrefix cursorPos x)
     (toReflexDom domPrefix cursorPos y) >>=
     \(a,b) -> return (leftmost [b,a])
    )
  Commonmark.HtmlRaw t ->
    domPrefix "raw: " >> text (t) >> return never
  Commonmark.HtmlText t ->
    domPrefix "text: " >> text (t) >> return never
  Commonmark.HtmlElement eltType tagname attrs mbcontents -> do
    domPrefix "element: "
    let
      myRange   = lookup "data-sourcepos" attrs >>= parseSourceRange

      containsSourcePos _ Nothing = False
      containsSourcePos (Commonmark.SourceRange rs) (Just (row,col)) =
        let cp = Pos.newPos "whatever" row col
        in  all (\(p0,p1) -> p0 <= cp && cp <= p1) rs

      -- highlight :: Dynamic t Bool
      highlight = case myRange of
        Nothing  -> constDyn False
        Just rng -> (rng `containsSourcePos`) <$> cursorPos


      modifyHighlights = ffor (updated highlight) $ \isHighlighted ->
        "style" =: if isHighlighted then Just "background-color: rgba(255,100,0,0.2);" else Nothing

    let
      cfg :: ElementConfig EventResult t (DomBuilderSpace m)
      cfg = ElementConfig
        { _elementConfig_namespace         = lookup "xmlns" attrs
        , _elementConfig_initialAttributes =
          mconcat ((\(k,v) -> AttributeName Nothing k =: v) <$> attrs)
        , _elementConfig_modifyAttributes  = Just modifyHighlights
        , _elementConfig_eventSpec         =
          addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const stopPropagation) def
        }
    --
    -- (e, childEvents) <- elDynAttrNS' (lookup "xmlns" attrs) tagname (dattrs) $ do
    (e, childEvents) <- element tagname cfg $ do
      case mbcontents of
        Nothing -> return never
        Just c  -> toReflexDom domPrefix cursorPos c
    let
      myClicks = maybe never (<$ domEvent Click e) myRange
    return $ leftmost [myClicks, childEvents]

parseSourceRange :: Text.Text -> Maybe Commonmark.SourceRange
parseSourceRange t =
  let (start, Text.drop 1 -> end) =
        t & Text.dropWhile (/= '@')
        & Text.drop 1
        & Text.breakOn "-"
      parsePos :: Text.Text -> Maybe Pos.SourcePos
      parsePos s = let (rStr,Text.drop 1 -> colStr) = Text.breakOn ":" s
                   in Pos.newPos "whatever" <$> readMaybe (Text.unpack rStr) <*> readMaybe (Text.unpack colStr)
  in
    (\a b -> Commonmark.SourceRange [(a,b)])
    <$> parsePos start
    <*> parsePos (end :: Text.Text)
