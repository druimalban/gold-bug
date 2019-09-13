{- file src/Main.hs

This is a chording keyboard demo using reactive-banana and sdl2.
We define a series of combinators for combining input.
This is certainly not perfect. On the whole, the combinators seem
reasonbale, but I don't have much experience writing software with
this approach, so they can probably be improved somewhat.

My hope is this can be useful for writing similar programs for
combining various input events, because there are many different
approaches to chording keyboards, and a flexible approach to them is
no doubt desirable.

I'd like to thank my friend Colin Paton for the example three-key
approach as well as a reasonable working JavaScript implementation. -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Keyboard.Combinators

import Control.Concurrent (threadDelay)
import Control.Monad (foldM, forever, unless)
import Lens.Micro (set)
import Lens.Micro.Extras (view)
import Reactive.Banana hiding (empty)
import Reactive.Banana.Frameworks
import SDL (($=), V4(..))
import qualified SDL
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)

main :: IO ()
main = do
  let _keyMap = KeyMap
        { _keyMapName = "default"
        , _keyP = SDL.ScancodeJ
        , _keyQ = SDL.ScancodeK
        , _keyR = SDL.ScancodeL
        , _keySpace = SDL.ScancodeSpace
        , _keyShift = SDL.ScancodeN
        , _keyNumSym = SDL.ScancodeM }
        
      _keysState = KeysState
        { _keyPStatus = Released
        , _keyQStatus = Released
        , _keyRStatus = Released
        , _keySpaceStatus = Released
        , _keyShiftStatus = Released
        , _keyNumSymStatus = Released }

      _currentInput = Neutral
      startingState = SystemState {..}

  SDL.initializeAll
  window <- SDL.createWindow "Chording keyboard demo" SDL.defaultWindow  
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  (addKeyEvent, fireKey) <- newAddHandler
  network <- compile (makeNetworkDescription renderer addKeyEvent startingState)
  actuate network
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering

  run renderer fireKey
  
  where
    run :: SDL.Renderer -> Handler (SDL.Scancode -> Bool) -> IO ()
    run renderer fireKey = do
      -- Catch events for SDL. This code is from the documentation and is filler.
      events <- SDL.pollEvents

      -- Catch a press of the 'q' for quit button.
      let eventIsQPress event =
            case SDL.eventPayload event of
              SDL.KeyboardEvent keyboardEvent ->
                SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
                SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
              _ -> False
          qPressed = any eventIsQPress events
          
      -- Input window for our demo - not clear how to focus input without this. A mellow teal colour.
      SDL.rendererDrawColor renderer $= V4 0 128 128 255
      SDL.clear renderer
      SDL.present renderer

      -- Feed keyboard state into our handler and delay thread by 1 second (value in microseconds), then loop.
      SDL.getKeyboardState >>= fireKey
      unless qPressed (run renderer fireKey)

    -- Produce our network description and handle transtitions between states.
    makeNetworkDescription :: SDL.Renderer -> AddHandler (SDL.Scancode -> Bool) -> SystemState -> MomentIO ()
    makeNetworkDescription renderer keysStateEvent currentState = do
      eKeys <- fromAddHandler keysStateEvent

      let eSystemStates = fmap (toSystemState currentState) eKeys
      bKeysStates <- accumB currentState (advanceInput <$> eSystemStates)
      eResultingStates <- changes bKeysStates
    
      reactimate' $ fmap (putStrLn . prettyInput . view currentInput) <$> eResultingStates

    toKeysState :: (SDL.Scancode -> Bool) -> KeyMap -> KeysState
    toKeysState f KeyMap {..} = KeysState {..} where
      _keyPStatus      = getPress f _keyP
      _keyQStatus      = getPress f _keyQ
      _keyRStatus      = getPress f _keyR
      _keySpaceStatus  = getPress f _keySpace
      _keyShiftStatus  = getPress f _keyShift
      _keyNumSymStatus = getPress f _keyNumSym

    toSystemState :: SystemState -> (SDL.Scancode -> Bool) -> SystemState
    toSystemState pastState f = SystemState {..} where
      _keyMap       = view keyMap pastState
      _keysState    = toKeysState f _keyMap
      _currentInput = Neutral
      
    getPress :: (SDL.Scancode -> Bool) -> SDL.Scancode -> KeyStatus
    getPress f scancode
      | f scancode = Pressed
      | otherwise  = Released

advanceInput :: SystemState -> SystemState -> SystemState
advanceInput newState pastState
  -- Lift modifiers...
  | modifiersReleased (view keysState newState) = 
      SystemState { _currentInput = view currentInput pastState <> Neutral
                  , _keysState    = view keysState newState
                  , _keyMap       = view keyMap newState }
  | otherwise = set currentInput (appendKeys pastState newPresses) newState
        where
          appendKeys :: SystemState -> [Input] -> Input
          appendKeys pastState keyPresses
            | modifiersReleased (view keysState pastState) = 
                view currentInput pastState <> foldl (+>) Neutral keyPresses
            | otherwise =
                foldl (+>) (view currentInput pastState) keyPresses

          modifiersReleased :: KeysState -> Bool
          modifiersReleased (KeysState {..}) = isReleased _keyPStatus &&
                                               isReleased _keyQStatus &&
                                               isReleased _keyRStatus &&
                                               isReleased _keySpaceStatus
          
          p     = wrapChange (view (keysState . keyPStatus)     pastState) (view (keysState . keyPStatus)     newState) P
          q     = wrapChange (view (keysState . keyQStatus)     pastState) (view (keysState . keyQStatus)     newState) Q
          r     = wrapChange (view (keysState . keyRStatus)     pastState) (view (keysState . keyRStatus)     newState) R
          space = wrapChange (view (keysState . keySpaceStatus) pastState) (view (keysState . keySpaceStatus) newState) Space

          -- Compare key statuses and wrap a given key-code into input.
          wrapChange :: KeyStatus -> KeyStatus -> KeyCode -> Input
          wrapChange Released Pressed keyCode = Key keyCode
          wrapChange _ _ _ = Neutral

          -- Compare two key statuses and return true if suddenly pressed.
          nowPressed :: KeyStatus -> KeyStatus -> Bool
          nowPressed Released Pressed = True
          nowPressed _ _ = False

          oldKeys = view keysState pastState
          newKeys = view keysState newState

          shiftStatus  = nowPressed (view keyShiftStatus  oldKeys) (view keyShiftStatus  newKeys)
          numSymStatus = nowPressed (view keyNumSymStatus oldKeys) (view keyNumSymStatus newKeys)

          newPresses   = filter (isKey) [p, q, r, space]
