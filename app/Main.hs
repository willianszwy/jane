{-# LANGUAGE OverloadedStrings #-}
module Main where

import           SDL
import           Linear                         ( V4(..) )
import           Control.Monad                  ( unless )

main :: IO ()
main = do
    initializeAll
    window   <- createWindow "J.A.N.E" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer
    appLoop renderer

appLoop :: Renderer -> IO ()
appLoop renderer = do
    events <- pollEvents
    let eventIsQPress event = case eventPayload event of
            KeyboardEvent keyboardEvent ->
                keyboardEventKeyMotion keyboardEvent
                    == Pressed
                    && keysymKeycode (keyboardEventKeysym keyboardEvent)
                    == KeycodeQ
            _ -> False
        qPressed = any eventIsQPress events
    rendererDrawColor renderer $= V4 255 255 255 255
    clear renderer
    present renderer
    unless qPressed (appLoop renderer)
