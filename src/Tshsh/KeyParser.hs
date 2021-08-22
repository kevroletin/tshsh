module Tshsh.KeyParser
  ( KeyAction (..),
    KeyParserState,
    mkKeyParser,
    keyParserReset,
    KeyParserRes (..),
    keyParserRun,
  )
where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Protolude

data KeyAction a
  = KeyPrefix ByteString Text [KeyAction a]
  | KeyAct ByteString Text a
  deriving (Show)

data KeyActionSt a
  = KeyPrefixSt Text Word8 (Map Word8 (KeyActionSt a))
  | KeyActSt Text Word8 a
  deriving (Show)

data KeyParserState a = KeyParserState
  { _ks_config :: KeyActionSt a,
    _ks_currAct :: KeyActionSt a,
    _ks_currPath :: [Word8]
  }
  deriving (Show)

mkKeyParser :: forall a. [KeyAction a] -> Either Text (KeyParserState a)
mkKeyParser acts0 = do
  case initState of
    Left err -> Left err
    Right x ->
      pure $
        KeyParserState
          { _ks_config = x,
            _ks_currAct = x,
            _ks_currPath = []
          }
  where
    initState :: Either Text (KeyActionSt a)
    initState = do
      acts <- go acts0
      pure (KeyPrefixSt "root node" 0 acts)

    go :: [KeyAction a] -> Either Text (Map Word8 (KeyActionSt a))
    go xs = do
      xs' <- traverse toKeySt xs
      pure (Map.fromList xs')

    getCode :: ByteString -> Either Text Word8
    getCode str =
      case BS.uncons str of
        Nothing -> Left "empty key code in KeyBindings config"
        Just (c, rest) ->
          if not (BS.null rest)
            then Left (decodeUtf8 rest <> " in KeyBindings config is longer than 1 character")
            else Right c

    toKeySt :: KeyAction a -> Either Text (Word8, KeyActionSt a)
    toKeySt (KeyPrefix txt name acts) = do
      c <- getCode txt
      acts' <- go acts
      pure (c, KeyPrefixSt name c acts')
    toKeySt (KeyAct txt name a) = do
      c <- getCode txt
      pure (c, KeyActSt name c a)

keyParserReset :: KeyParserState a -> KeyParserState a
keyParserReset kp0 =
  KeyParserState
    { _ks_config = _ks_config kp0,
      _ks_currAct = _ks_config kp0,
      _ks_currPath = []
    }

data KeyParserRes a
  = KeyParserData ByteString ~(KeyParserRes a)
  | KeyParserAction a ~(KeyParserRes a)
  | KeyParserNull (KeyParserState a)
  deriving (Show)

keyParserRun :: KeyParserState a -> ByteString -> KeyParserRes a
keyParserRun st0@(KeyParserState _ (KeyActSt _ _ act) _) str =
  KeyParserAction act (keyParserRun (keyParserReset st0) str)
keyParserRun st0@(KeyParserState _ (KeyPrefixSt _ _ nextActs) path0) str
  | null path0 =
    case BS.findIndex (`Map.member` nextActs) str of
      Nothing ->
        mOut str $
          KeyParserNull (keyParserReset st0)
      Just i ->
        let c = BS.index str i
            (Just nextA) = Map.lookup c nextActs
         in mOut (BS.take i str) $
              keyParserRun
                (st0 {_ks_currAct = nextA, _ks_currPath = [c]})
                (BS.drop (i + 1) str)
  | otherwise =
    case BS.uncons str of
      Nothing ->
        KeyParserNull st0
      Just (c, rest) ->
        case Map.lookup c nextActs of
          Nothing ->
            mOut (BS.pack . reverse $ (c : path0)) $
              keyParserRun (keyParserReset st0) rest
          Just nextAct -> do
            keyParserRun
              (st0 {_ks_currAct = nextAct, _ks_currPath = (c : path0)})
              rest
  where
    mOut bs cont
      | BS.null bs = cont
      | otherwise = KeyParserData bs cont
