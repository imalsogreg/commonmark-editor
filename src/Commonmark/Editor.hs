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

module Commonmark.Editor where

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

-- import Commonmark.DOM

#ifndef ghcjs_HOST_OS
import qualified Language.Javascript.JSaddle.Debug      as JSaddle
import qualified Language.Javascript.JSaddle.WebSockets as JSaddle
#endif



data EditorConfig t = EditorConfig
  { initialContent :: Text.Text
  , sourceMap      :: Dynamic t Commonmark.SourceMap
  , setSelection   :: Event t Commonmark.SourceRange
  }

data Editor t = Editor
  { contentUpdates :: Event t Text.Text
  , cursorXY       :: Dynamic t (Int,Int)
  , cursorN        :: Dynamic t Int
  , keypresses     :: Event t Char
  }

editor
  :: forall t m
  .( DomBuilder t m
   , PerformEvent t m
   , PostBuild t m
   , DomBuilderSpace m ~ GhcjsDomSpace
   , JSaddle.MonadJSM (Performable m)
   , TriggerEvent t m
   , Fix.MonadFix m
   , MonadHold t m
   ) => EditorConfig t -> m (Editor t)
editor EditorConfig {initialContent, setSelection} = divClass "commonmark-editor" $ do
  te <- textAreaElement $
        def & textAreaElementConfig_elementConfig
              . elementConfig_initialAttributes
              .~ ("rows" =: "20")
            & textAreaElementConfig_initialValue
              .~ initialContent
  let
    contentUpdates  = _textAreaElement_input te
    textAreaElement = _textAreaElement_raw   te

    motionEvents = leftmost [domEvent Click te, () <$ domEvent Keyup te]
  content <- holdDyn initialContent contentUpdates
  cursorN <- holdDyn 1 =<< performEvent
    (ffor motionEvents $ \() -> JSaddle.liftJSM $
                                DOM.getSelectionStart textAreaElement
    )

  let
    pos2d t n = go 1 (Text.lines t) n
      where go currentRow tLines currentCol =
              case tLines of
                []     -> (currentRow, currentCol)
                (l:ls) -> if currentCol <= Text.length l
                          then (currentRow, currentCol)
                          else go (currentRow + 1) ls (currentCol - Text.length l - 1)

    cursorXY = pos2d <$> content <*> cursorN
    keypresses = fmap (toEnum . fromIntegral) (domEvent Keypress te)

  selectionResults <- do
    let
      pos1d t (Commonmark.SourceRange sr) = case sr of
        [] -> Left "Empty list"
        ((start,end):xs)
          | length xs > 0 -> Left "Multi-list"
          | otherwise ->
            let
              contentLines = Text.lines t
              (r1,c1) = (Pos.sourceLine start, Pos.sourceColumn start)
              (r2,c2) = (Pos.sourceLine end  , Pos.sourceColumn end  )

            in Right (Text.length (Text.unlines (take (r1 - 1) contentLines)) + c1,
                 Text.length (Text.unlines (take (r2 - 1) contentLines)) + c2)

    setSelection' <- throttle 0.5 setSelection
    performEvent
      (ffor (attachPromptlyDynWith pos1d content setSelection') $ \pair ->
          case pair of
            Left e -> pure e
            Right (start,end) -> do
              DOM.focus textAreaElement
              DOM.setSelectionStart textAreaElement (start - 1)
              DOM.setSelectionEnd   textAreaElement (end   - 1)
              pure $ "click " <> Text.pack (show (start,end))
      )

  return $ Editor
    { contentUpdates
    , cursorXY
    , cursorN
    , keypresses
    }




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

