{-# LANGUAGE TypeFamilies #-}

-- https://hackage.haskell.org/package/strip-ansi-escape-0.1.0.0/docs/src/Data.String.AnsiEscapeCodes.Strip.Internal.html#stripAnsiEscapeCodesP
-- specialized for ByteString

module Data.String.AnsiEscapeCodes.Strip.ByteString
  ( stripAnsiEscapeCodes,
  )
where

import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.Combinator as AC
import Data.Attoparsec.Types (Parser)
import qualified Data.ByteString as BS
import GHC.Word
import Protolude
import Prelude (id)

fromChar :: (Num c, Enum a) => a -> c
fromChar = fromIntegral . fromEnum

{-
-- From: https://github.com/chalk/ansi-regex/blob/166a0d5eddedacf0db7ccd7ee137b862ab1dae70/index.js
  [\x001B\x009B]
  [[\]()#;?]*
    (?:
      (?:
        (?:
          [a-zA-Z\d]*
          (?:
            ;[-a-zA-Z\d\/#&.:=?%@~_]*
          )*
        )?
        \x0007
      )
      |
      (?:
        (?:
          \d{1,4}
          (?:;\d{0,4})*
        )?
        [\dA-PR-TZcf-ntqry=><~]
      )
    )
-}

{-# INLINE stripAnsiEscapeCodesP #-}
stripAnsiEscapeCodesP ::
  Env -> Parser ByteString ByteString
stripAnsiEscapeCodesP e =
  mconcat <$> (AC.many' escapeSequencesSkipped <* skipLeftEscapeSequence)
  where
    escapeSequencesSkipped = do
      skipEscapeSequence
      takeWhile1 e (not . isEsc)

    isEsc :: Word8 -> Bool
    isEsc = (== 0x1B)

    esc = skip isEsc

    skipEscapeSequence = AC.skipMany $ do
      esc
      beginsWithOpenSquareBracket
        <|> beginsWithClosingSquareBracket
        <|> beginsWithParenthesis
        <|> beginsWithHash
        <|> singleChar
        <|> beginsWithDigit
      where
        singleChar =
          skip (`BS.elem` ("ABCDHIJKSTZ=>12<78HcNOME" :: ByteString))

        beginsWithDigit = do
          skip (`BS.elem` ("5036" :: ByteString))
          skip (== (fromChar 'n'))

        beginsWithClosingSquareBracket = do
          skip (== (fromChar ']'))

          skipWhile e (/= 0x7) <* skipAny

        beginsWithOpenSquareBracket = do
          skip (== (fromChar '['))
          _ <- optional $ skip (`BS.elem` ("?;" :: ByteString))
          AC.skipMany $ do
            AC.skipMany1 digit
            AC.skipMany $ do
              skip (== (fromChar ';'))
              AC.skipMany1 digit
          skip isEndChar
          where
            isEndChar :: Word8 -> Bool
            isEndChar c =
              isDigit ((toEnum . fromIntegral) c)
                || between 'A' 'P'
                || between 'R' 'T'
                || c == (fromChar 'Z')
                || c == (fromChar 'c')
                || between 'f' 'n'
                || c `BS.elem` ("tqry=><~" :: ByteString)
              where
                between x y = (fromChar x) <= c && c <= (fromChar y)

        beginsWithParenthesis = do
          skip (`BS.elem` ("()" :: ByteString))
          skip (`BS.elem` ("AB012" :: ByteString))

        beginsWithHash = do
          skip (== (fromChar '#'))
          skip (`BS.elem` ("34568" :: ByteString))

    skipLeftEscapeSequence = do
      skipEscapeSequence
      AC.endOfInput

data Env = Env
  { skipWhile :: (Word8 -> Bool) -> Parser ByteString (),
    takeWhile1 :: (Word8 -> Bool) -> Parser ByteString ByteString
  }

{-# INLINE skip #-}
skip :: (Word8 -> Bool) -> Parser ByteString ()
skip = void . AC.satisfyElem

{-# INLINE skipAny #-}
skipAny :: Parser ByteString ()
skipAny = skip (const True)

{-# INLINE digit #-}
digit :: Parser ByteString ()
digit = skip $ \c -> (fromChar '0') <= c && c <= (fromChar '9')

{-# INLINE stripAnsiEscapeCodes #-}
stripAnsiEscapeCodes :: ByteString -> ByteString
stripAnsiEscapeCodes str =
  either (const str) id $ AB.parseOnly (stripAnsiEscapeCodesP e) str
  where
    e = Env AB.skipWhile AB.takeWhile1
