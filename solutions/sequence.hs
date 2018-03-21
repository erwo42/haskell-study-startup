{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as BS.Char8
import System.Environment
import Data.Foldable
import qualified Data.Sequence as Sequence
-- import Debug.Trace

data Table = Table { table_name :: BS.Char8.ByteString
                   , table_columns :: Sequence.Seq BS.Char8.ByteString
                   } deriving (Eq, Show)

data Schema = Schema (Sequence.Seq Table) deriving (Eq, Show)

data Class = Class { class_name :: Sequence.Seq BS.Char8.ByteString
                   , class_members :: Sequence.Seq BS.Char8.ByteString
                   } deriving (Eq)

sToList :: Sequence.Seq a -> [a]
sToList = foldl' (\b a -> a : b) []

instance Show Class where
  show (Class names fields) = BS.Char8.unpack (BS.Char8.intercalate "_" (sToList names)) ++ ": " ++ BS.Char8.unpack (BS.Char8.intercalate " " (sToList fields))

instance Ord Class where
  Class names _ `compare` Class names2 _ = names `compare` names2

data SearchResult a = SRLT Int | SRGT Int | SREQ Int a deriving (Eq, Show)

data Field_Classes = FC BS.Char8.ByteString (Sequence.Seq BS.Char8.ByteString) deriving (Eq, Show)

instance Ord Field_Classes where
  FC n _ `compare` FC m _ = n `compare` m 

searchSorted :: Ord a => a -> Sequence.Seq a -> Int -> Int -> SearchResult a
searchSorted key list min max
  | min == max = SRLT 0
  | mid == min = case cmp of
    EQ -> SREQ mid listElem
    LT -> SRLT min
    GT -> SRGT min
  | otherwise = case cmp of
    EQ -> SREQ mid listElem
    LT -> searchSorted key list min mid
    GT -> searchSorted key list mid max
 where
  mid = (min + max) `div` 2
  listElem = Sequence.index list mid 
  cmp = (listElem `compare` key)

insertSortedWith :: (Show a, Ord a) => a -> Sequence.Seq a -> (a -> a) -> Sequence.Seq a
insertSortedWith elem list insert = case searchSorted elem list 0 l of
  SRLT ind -> Sequence.insertAt ind elem list 
  SRGT ind -> if ind == l - 1 then list Sequence.|> elem else Sequence.insertAt (ind+1) elem list
  SREQ ind listElem -> Sequence.update ind (insert listElem) list
 where
  l = Sequence.length list

toTable :: BS.Char8.ByteString -> Table
toTable line = let f = BS.Char8.words line
               in Table (head f) (Sequence.fromList (tail f))

loadSchemaFromFile :: String -> Int -> IO Schema
loadSchemaFromFile fname num = do
  content <- BS.Char8.readFile fname
  return $ Schema $ fmap toTable $ Sequence.fromList $ take num $ BS.Char8.lines content

addTableFieldToIntermediateResult :: BS.Char8.ByteString -> Sequence.Seq Field_Classes -> BS.Char8.ByteString -> Sequence.Seq Field_Classes
addTableFieldToIntermediateResult cla fcs field = insertSortedWith (FC field (Sequence.singleton cla)) fcs (\(FC f cs) -> FC f (insertSortedWith cla cs id))

addTableToIntermediateResult :: Sequence.Seq Field_Classes -> Table -> Sequence.Seq Field_Classes
addTableToIntermediateResult fcs (Table cla fields) = foldl' (addTableFieldToIntermediateResult cla) fcs fields

addFieldClassesToClassList :: Sequence.Seq Class -> Field_Classes -> Sequence.Seq Class
addFieldClassesToClassList cs (FC name classes) = insertSortedWith (Class classes (Sequence.singleton name)) cs (\(Class names fields) -> Class names (insertSortedWith name fields id))

doIt :: String -> Int -> IO ()
doIt fname num = do
  (Schema ts) <- loadSchemaFromFile fname num
  let intermediateResult = foldl' addTableToIntermediateResult Sequence.empty  ts
  let classList = foldl' addFieldClassesToClassList Sequence.empty intermediateResult
  mapM_ print classList

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x, n] -> doIt x (read n)
    _   -> putStrLn "Usage: mySchema2Class <inputfilename> <num>"
