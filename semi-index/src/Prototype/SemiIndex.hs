module Prototype.SemiIndex where

import Control.Monad
import Data.List

popCount1 :: String -> Int
popCount1 bs = length (filter (== '1') bs)

rank1 :: String -> Int -> Int
rank1 bs n = popCount1 (take n bs)

select1 :: String -> Int -> Int
select1 bs n = length (head (dropWhile ((< n) . popCount1) (inits (filter isBinary bs))))
  where isBinary c = c == '0' || c == '1'

data Cursor = Cursor
  { balancedParens :: String
  , position       :: Int
  } deriving (Eq, Show)

printCursor :: Cursor -> IO ()
printCursor (Cursor bp n) = do
  putStrLn bp
  putStrLn $ replicate (n - 1) ' ' <> "^"

nextSibling :: Cursor -> Maybe Cursor
nextSibling (Cursor s n) = case drop (n - 1) s of
  ('(':rs) -> Cursor s <$> go 1 rs n
  _        -> Nothing
  where go :: Int -> String -> Int -> Maybe Int
        go 0     ('(': _) count = Just (count + 1)
        go 0           _  _     = Nothing
        go depth ('(':as) count = go (depth + 1) as (count + 1)
        go depth (')':as) count = go (depth - 1) as (count + 1)
        go depth (_  :as) count = go  depth      as  count
        go _     []       _     = Nothing

firstChild :: Cursor -> Maybe Cursor
firstChild (Cursor s n) = case drop (n - 1) s of
  ('(':'(':_) -> Just (Cursor s (n + 1))
  _           -> Nothing

(>^) :: Monad m => (a -> m a) -> Int -> (a -> m a)
m >^ 0 = return
m >^ n = m >=> (m >^ (n - 1))
