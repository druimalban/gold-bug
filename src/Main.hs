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

import Control.Concurrent (threadDelay)
import Control.Monad (foldM, forever, unless)
import Data.Char (toLower, toUpper)
import Data.List (group)
import Data.List.NonEmpty ((<|), NonEmpty, fromList, toList)
import qualified Data.List.NonEmpty as NE
import Data.Monoid (mappend)
import Data.Tuple (swap)
import Lens.Micro (over, set)
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)
import Reactive.Banana hiding (empty)
import Reactive.Banana.Frameworks
import SDL (($=), V4(..))
import qualified SDL
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)
import Test.QuickCheck

-- Non-modifying keys.
data KeyCode = P | Q | R | Space
  deriving (Show, Eq)

-- Define an instance of arbitrary for NonEmpty.
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = fromList <$> suchThat arbitrary (\x -> length x >= 1)

-- Generator for arbitrary key-codes.
instance Arbitrary KeyCode where
  arbitrary = elements [P, Q, R, Space]

-- Basic key-map, corresponding to non-modifying keys on the real input device.
data KeyMap = KeyMap
  { _keyMapName :: String
  , _keyP       :: SDL.Scancode
  , _keyQ       :: SDL.Scancode
  , _keyR       :: SDL.Scancode
  , _keySpace   :: SDL.Scancode
  , _keyShift   :: SDL.Scancode
  , _keyNumSym  :: SDL.Scancode }
  deriving (Show, Eq)

printPrettyKeyMap :: KeyMap -> IO ()
printPrettyKeyMap (KeyMap {..}) = do
  p <-  SDL.getScancodeName _keyP
  q <-  SDL.getScancodeName _keyQ
  r <-  SDL.getScancodeName _keyR
  sp <- SDL.getScancodeName _keySpace
  sh <- SDL.getScancodeName _keyShift
  ns <- SDL.getScancodeName _keyNumSym

  putStrLn $ mconcat
    [ "Key-map: ", _keyMapName, "\n"
    , "------------------",   "-|-----------------\n"
    , "Primary, secondary",   " | Additional and\n"
    , "and tertiary keys:",   " | modifier keys:\n"
    , "------------------",   "-|-----------------\n"
    , "         ", q,         "\t Space ' ' key: ", sp, "\n"
    , "        / \\",         "\t Shift   ⇧ key: ", sh, "\n"
    , "       ", p, "___", r, "\t Num-sym ⎇ key: ", ns ]
    
-- Track the status of a given key. Analogous to Boolean true/false.
data KeyStatus = Pressed | Released
  deriving (Show, Eq)

isPressed :: KeyStatus -> Bool
isPressed Pressed  = True
isPressed Released = False

isReleased :: KeyStatus -> Bool
isReleased = not . isPressed

-- Track the status of our keys.
data KeysState = KeysState
  { _keyPStatus      :: KeyStatus
  , _keyQStatus      :: KeyStatus
  , _keyRStatus      :: KeyStatus
  , _keySpaceStatus  :: KeyStatus
  , _keyShiftStatus  :: KeyStatus
  , _keyNumSymStatus :: KeyStatus }
  deriving (Show, Eq)

-- Function snoc, opposite of cons. Append an item to some nonempty list.
snoc :: NonEmpty a -> a -> NonEmpty a
snoc xs x = NE.reverse (x <| NE.reverse xs)
-- Infix version of snoc
(|>) = snoc

-- Append two lists together.
append :: NonEmpty a -> NonEmpty a -> NonEmpty a
append xs ys = fromList (toList xs ++ toList ys)

-- Generate the contents of a given chord. Don't wrap this yet, for use with the append function above.
singleChordContents :: KeyCode -> NonEmpty (NonEmpty KeyCode, String)
singleChordContents = \case
  Space   -> fromList [(fromList [Space], " ")]
  keyCode -> case lookupKeyCodes keyCodes of
    Nothing -> fromList [(keyCodes, "")]
    Just c  -> fromList [(keyCodes, [c])]
    where keyCodes = fromList [keyCode]

-- Generate and wrap contents into a chord.
singleChord :: KeyCode -> Input
singleChord = Chord . singleChordContents

-- Take a number of keys and produce a single chord.
buildChord :: NonEmpty KeyCode -> Input
buildChord keyCodes = case lookupKeyCodes keyCodes of
  Nothing -> Chord (fromList [(keyCodes, "")])
  Just c  -> Chord (fromList [(keyCodes, [c])])

-- Concat two chords together into new chord.
concatChord :: Input -> Input -> Input
concatChord (Key ch)    (Key ch')    = Chord $ append (singleChordContents ch) (singleChordContents ch')
concatChord (Chord chs) (Key ch)     = Chord $ append chs (singleChordContents ch)
concatChord (Key ch)    (Chord chs)  = Chord $ append (singleChordContents ch) chs
concatChord (Chord chs) (Chord chs') = Chord $ append chs chs'
concatChord _ _ = Neutral
-- Infix version of concatChord.
(<+>) = concatChord

-- Prepend a key to a chord.
prependKey :: Input -> Input -> Input
prependKey (Key ch) (Chord chs)
  | NE.length chs == 1 = ch'
  | otherwise = ch' <+> chs'
  where
    ch'  = buildChord (ch <| fst (NE.head chs))
    chs' = Chord (fromList . NE.tail $ chs)
prependKey (Key ch) (Key ch') = Key ch <+ singleChord ch'
prependKey (Key ch) Neutral   = singleChord ch
prependKey _ _ = Neutral
(<+) = prependKey

-- Append a key to a chord.
appendKey :: Input -> Input -> Input
appendKey (Chord chs) (Key ch)
  | NE.length chs == 1 = ch'
  | otherwise = chs' <+> ch'
  where
    ch'  = buildChord (fst (NE.last chs) |> ch)
    chs' = Chord (fromList . NE.init $ chs)
appendKey (Key ch) (Key ch') = singleChord ch +> Key ch'
appendKey Neutral  (Key ch)  = singleChord ch
appendKey _ _ = Neutral
(+>) = appendKey

-- This is probably not awfully fast, but it is a reasonable function
-- provided in base.
lookupKeyCodes :: NonEmpty KeyCode -> Maybe Char
lookupKeyCodes ks = lookup ks chords where
  chords =
    [ (fromList [P], 'e') , (fromList [Q], 't') , (fromList [R], 'a')
    
    , (fromList [P, Q], 'o') , (fromList [P, R], 'i') , (fromList [Q, P], 'n')
    , (fromList [Q, R], 's') , (fromList [R, P], 'h') , (fromList [R, Q], 'r')

    , (fromList [P, Q, R], 'd') , (fromList [P, R, Q], 'l') , (fromList [Q, P, R], 'c')
    , (fromList [Q, R, P], 'u') , (fromList [R, P, Q], 'm') , (fromList [R, Q, P], 'w')
    , (fromList [P, Q, Q], 'f') , (fromList [P, R, R], 'g') , (fromList [Q, P, P],'y')
    , (fromList [Q, R, R], 'p') , (fromList [R, P, P], 'b')

    , (fromList [P, Q, R, R], 'v') , (fromList [P, R, Q, R], 'k') , (fromList [Q, P, R, R], 'j')
    , (fromList [Q, R, P, P], 'x') , (fromList [R, P, Q, P], 'q') , (fromList [R, P, Q, Q], 'z') ]

data Input
  -- A given key with a base value of a char.
  -- For chording, we are particularly concerned with combinations of
  -- buttons being held down. Therefore, it makes sense to define a
  -- neutral state which a given key or chord can go back to.
  = Key KeyCode
  -- A chord is a string of arbitrary tokens produced by a key or
  -- otherwise.
  -- It is important to note some things.
  -- We may combine chords to produce longer chords.
  -- In reality, this is to help implement Semigroup and Monoid rather
  -- than reflecting a possibility of physically combining them.
  -- What it does allow us to do is define chords of arbitrary length.
  -- Each chord is a pair. The first element of the pair is the keys
  -- used  to produce the chord. This must be non-empty. The second
  -- element is the character the chord produces.
  | Chord ( NonEmpty (NonEmpty KeyCode, String) )
  -- Finally, we define a neutral position, that is cognant with the
  -- identity property.
  | Neutral
  deriving (Eq)

instance Show Input where
  show Neutral     = "Neutral"
  show (Key c)     = "Key " ++ show c
  show (Chord chs) = foldr getCh [] (toList chs) where
    getCh :: (NonEmpty KeyCode, String) -> String -> String 
    getCh (ne, s) [] = mconcat [ foldr getKs [] (group . toList $ ne)
                               , " ⟶  ", s ]
    getCh (ne, s) xs = mconcat [ foldr getKs [] (group . toList $ ne)
                               , " ⟶  ", s
                               , " | ", xs ]

    getKs :: [KeyCode] -> String -> String
    getKs [] ks = ""
    getKs kc []
      | length kc == 1 = show (head kc)
      | otherwise      = mconcat [ show (head kc), "↺", show (length kc) ]
    getKs kc ks
      | length kc == 1 = mconcat [ show (head kc), " → ", ks ]
      | otherwise      = mconcat [ show (head kc), "↺", show (length kc), " → ", ks ]

instance Semigroup Input where
  Neutral <> mod     = mod
  mod     <> Neutral = mod

  Key ch    <> key   = Key ch    <+> key
  Chord chs <> key   = Chord chs <+> key

instance Monoid Input where
  mempty = Neutral

instance Arbitrary Input where
  arbitrary = oneof [ Key   <$> arbitrary
                    , Chord <$> arbitrary
                    , return Neutral ]


prettyInput :: Input -> String
prettyInput (Chord chs) = foldr getC [] (toList chs) where
  getC (ne, s) xs = s ++ xs
prettyInput input = show input

isKey :: Input -> Bool
isKey (Key _) = True
isKey _ = False

data SystemState = SystemState
  { _keyMap       :: KeyMap
  , _keysState    :: KeysState
  , _currentInput :: Input }
  deriving (Show, Eq)

-- Define some reasonable lenses.
makeLenses ''KeyMap
makeLenses ''KeysState
makeLenses ''SystemState

toUpper' :: Char -> Char
toUpper' c
  | toUpper c /= c = toUpper c
  | otherwise = case lookup c upperSyms of
      Nothing -> c
      Just c' -> c'

toLower' :: Char -> Char
toLower' c
  | toLower c /= c = toLower c
  | otherwise = case lookup c (map swap upperSyms) of
      Nothing -> c
      Just c' -> c'

upperSyms =
  [ (',', '<') , ('.', '>') , ('-', '_')
  
  , ('1', '!') , ('2', '@') , ('3', '#')
  , ('4', '$') , ('5', '%') , ('6', '^')

  , ('7', '&') , ('8', '*') , ('9', '(')
  , ('0', ')') , ('=', '+') , ('/', '?')
  , (';', ':') , ('\\', '|') , ('[', '{')
  , (']', '}') , ('B', '`') ]
  
toNumSym :: Char -> Char
toNumSym c = case lookup c numSyms of
  Nothing -> c
  Just c' -> c'

fromNumSym :: Char -> Char
fromNumSym c = case lookup c (map swap numSyms) of
  Nothing -> c
  Just c' -> c'

numSyms =
  -- Base set. Needs some work as this corresponds most closely
  -- to a traditional PC keyboard, not the most frequent keys.
  [ ('e', ',') , ('t', '.') , ('a', '-')
  
  , ('o', '1') , ('i', '2') , ('n', '3')
  , ('s', '4') , ('h', '5') , ('r', '6')
  
  , ('d', '7') , ('l', '8') , ('c', '9') 
  , ('u', '0') , ('m', '=') , ('w', '/')
  , ('f', ';') , ('g', '\\'), ('y', '[')
  , ('p', ']') , ('b', '`')

  -- Shifted letter set.
  -- Currently exactly the same as PC keyboard as above.
  , ('E', '<') , ('T', '>') , ('A', '_')

  , ('O', '!') , ('I', '@') , ('N', '#')
  , ('S', '$') , ('H', '%') , ('R', '^')

  , ('D', '&') , ('L', '*') , ('C', '(')
  , ('U', ')') , ('M', '+') , ('W', '?')
  , ('F', ':') , ('G', '|') , ('Y', '{')
  , ('P', '}') , ('B', '~')
  ]

changeSymLast :: (Char -> Char) -> Input -> Input
changeSymLast f (Key ch) = changeSymLast f (singleChord ch)
changeSymLast f (Chord chs) = Chord (fromList (r ++ [q])) where
  (keys, s) = NE.last chs
  q = (keys, init s ++ [f (last s)])
  r = NE.init chs
changeSymLast _ input = input

changeSymFirst :: (Char -> Char) -> Input -> Input
changeSymFirst f (Key ch) = changeSymFirst f (singleChord ch)
changeSymFirst f (Chord chs) = Chord (fromList (q : r)) where
  (keys, s) = NE.head chs
  q = (keys, tail s ++ [f (head s)])
  r = NE.tail chs
changeSymFirst _ input = input

shiftLast     = changeSymLast toUpper'
numSymLast    = changeSymLast toNumSym
relShiftLast  = changeSymLast toLower'
relNumSymLast = changeSymLast fromNumSym

shiftFirst     = changeSymFirst toUpper'
numSymFirst    = changeSymFirst toNumSym
relShiftFirst  = changeSymFirst toLower'
relNumSymFirst = changeSymFirst fromNumSym

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
      SDL.rendererDrawColor renderer $= V4 0   128 128 255
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

          ---- Test associativity and identity laws ----
prop_Input :: Input -> Input -> Input -> Bool
prop_Input p q r = p <> (q <> r) == (p <> q) <> r &&
                   p <> mempty == p && mempty <> p == p &&
                   q <> mempty == q && mempty <> q == q &&
                   r <> mempty == r && mempty <> r == r
