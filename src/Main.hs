{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


data Line = TLine T.Text
          | ImportLine { ilText:: T.Text
                       , ilQualified:: Bool
                       }
          deriving (Eq, Show)

instance Ord Line where
  compare (TLine _) (TLine _) = EQ
  compare ImportLine{..} (TLine _) = GT
  compare (TLine _) ImportLine{..} = LT
  compare (ImportLine ilt1 _) (ImportLine ilt2 _) = compare ilt1 ilt2


main :: IO ()
main = do
  txt <- TIO.getContents
  let ls = sort $ map parseLine (T.lines txt)
  TIO.putStr $ T.unlines $ map (lToText) ls


lToText:: Line -> T.Text
lToText l = case l
              of TLine t -> t
                 ImportLine{..} -> "import" <> q <> ilText
                                   where q = if ilQualified
                                             then " qualified "
                                             else "           "


parseLine:: T.Text -> Line
parseLine l = if T.isPrefixOf "import" (T.strip l)
              then ImportLine t (T.isPrefixOf "qualified" qT)
              else TLine l
  where qT = maybeStripPrefix "import" l
        t  = maybeStripPrefix "qualified" qT


maybeStripPrefix:: T.Text -> T.Text -> T.Text
maybeStripPrefix p t = T.strip $ fromMaybe t $ T.stripPrefix p (T.strip t)
