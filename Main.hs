
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Reflex.Dom.Main
import Reflex.Dom (mainWidgetWithHead, MonadWidget, text, mainWidget)
import Commonmark.Editor

main :: IO ()
main = mainWidgetWithHead header $ do
  updates <- editor
  viewer (ViewerConfig ((1,) <$> updates))
  return ()
