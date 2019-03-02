-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Content.Block
-- Copyright   :  © 2019 Avi Weinstock
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode markup and paragraphs.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Parse.Attoparsec.Content.Block
(
  parseBlock,
  parseGreaterBlockType
)
where

import           Control.Monad                                   (guard)
import           Data.Attoparsec.Text                            (Parser,
                                                                  string,
                                                                  choice,
                                                                  option,
                                                                  manyTill,
                                                                  takeTill,
                                                                  isEndOfLine,
                                                                  isHorizontalSpace,
                                                                  endOfLine)
import           Data.OrgMode.Types                              (Content (..), BlockType (..), GreaterBlockType (..))
import           Data.OrgMode.Parse.Attoparsec.Util              (nonHeadline, skipOnlySpace)
--import           Data.Text                                       (Text)

parseBlockType :: Parser BlockType
parseBlockType = choice [
    Comment <$ string "COMMENT",
    Example <$ string "EXAMPLE",
    Export <$ string "EXPORT",
    Src <$ string "SRC"
    ]

parseGreaterBlockType :: Parser GreaterBlockType
parseGreaterBlockType = choice [
    Verse <$ string "VERSE",
    Center <$ string "CENTER",
    Quote <$ string "QUOTE",
    OtherGreaterBlockType <$> takeTill isHorizontalSpace
    ]

parseBlock :: Parser Content
parseBlock = do
    string "#+BEGIN_"
    type1 <- parseBlockType
    skipOnlySpace
    data_ <- option Nothing (Just <$> takeTill isEndOfLine <* endOfLine)
    contents_ <- manyTill nonHeadline (string "#+END_")
    type2 <- parseBlockType
    guard (type1 == type2) -- TODO: should this be lenient?
    pure $ Block type1 data_ contents_
