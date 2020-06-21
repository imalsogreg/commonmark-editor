
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Commonmark.App    (header, mainApp)
import           Commonmark.Editor
import           Reflex.Dom        (MonadWidget, mainWidget, mainWidgetWithHead,
                                    text)

main :: IO ()
main = mainWidgetWithHead header mainApp
