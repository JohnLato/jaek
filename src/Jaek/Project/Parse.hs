{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Jaek.Project.Parse (
  Parse (..)
)

where

import           Jaek.Base
import           Jaek.Gen
import           Jaek.StreamExpr as SE
import           Jaek.StreamT    as ST
import           Jaek.Tree

import           Sound.Iteratee (AudioFormat (..))
import           Data.Attoparsec as A
import           Data.Attoparsec.Binary
import           Data.Attoparsec.Char8
import           Data.ByteString.UTF8 (toString)

import           Data.Tree

-- | a small helper function for reading UTF8-encoded text.
parseStr :: Parser String
parseStr = toString <$> (anyWord64le >>= A.take . fI)

class Parse a where
  jparse :: Parser a

instance Parse a => Parse [a] where
  jparse = anyWord64le >>= \n -> replicateM (fI n) jparse

instance Parse Int where
  jparse = fI <$> anyWord64le

instance Parse SampleCount where
  jparse = fI <$> anyWord64le

instance Parse Integer where
  jparse = fI <$> anyWord64le

instance Parse AudioFormat where
  jparse = AudioFormat <$> jparse <*> jparse <*> jparse

instance Parse Double where
  jparse = try (double <* char 'z')

instance Parse GenFunc where
  jparse = (Null <$ word8 0)
    <|> (word8 1 *> (ConstF <$> jparse))

instance Parse StreamExpr where
  jparse =
        (word8 0 *> (FileSource <$> parseStr <*> jparse <*>
                                    jparse <*> jparse <*> jparse))
    <|> (word8 1 *> (GenSource <$> jparse <*> jparse))
    <|> (word8 2 *> (Region <$> jparse <*> jparse <*> jparse))
    <|> (word8 3 *> (StreamSeq <$> jparse))
    <|> (word8 4 *> (SE.Mix <$> jparse <*> jparse))

instance Parse NodeRef where
  jparse = (word8 0 *> (AbsPath <$> jparse))
       <|> (word8 1 *> (RelPath <$> jparse <*> jparse))

instance Parse StreamT where
  jparse = (word8 0 *> (Cut  <$> jparse <*> jparse <*> jparse))
       <|> (word8 1 *> (Mute <$> jparse <*> jparse <*> jparse))
       <|> (word8 2 *> (Insert <$> jparse <*> jparse <*> jparse <*>
                                   jparse <*> jparse <*> jparse))
       <|> (word8 3 *> (ST.Mix <$> jparse <*> jparse <*> jparse <*>
                                   jparse <*> jparse <*> jparse))

instance Parse Node where
  jparse = Root <$ word8 0
      <|> (word8 1 *> (Init <$> parseStr <*> jparse <*> jparse))
      <|> (word8 2 *> (Mod  <$> jparse <*> jparse <*> jparse))

instance Parse HTree where
  jparse = Node <$> jparse <*> jparse
