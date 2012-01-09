module ParsingSupport
where

import Text.XML.HXT.Core
import Text.Parsec
import Data.Maybe
import qualified Data.Foldable

liftArrow :: Arrow a => (c -> d) -> a b c -> a b d
liftArrow = flip (>>^)
liftArrow2 :: Arrow a => (c -> c' -> d) -> a b c -> a b c' -> a b d
liftArrow2 f a1 a2 = arr (\a -> (a, a)) >>> (a1 *** a2) >>> arr (\(a, b) -> f a b)
liftArrow3 :: Arrow a => (c -> c' -> c'' -> d) -> a b c -> a b c' -> a b c'' -> a b d
liftArrow3 f a1 a2 a3 = arr (\a -> (a, (a, a))) >>> (a1 *** a2 *** a3) >>> arr (\(a, (b, c)) -> f a b c)
liftArrow4 :: Arrow a => (c -> c' -> c'' -> c''' -> d) -> a b c -> a b c' -> a b c'' -> a b c''' -> a b d
liftArrow4 f a1 a2 a3 a4 = arr (\a -> (a, (a, (a, a)))) >>> (a1 *** a2 *** a3 *** a4) >>> arr (\(a, (b, (c, d))) -> f a b c d)
liftArrow5 :: Arrow a => (c -> c' -> c'' -> c''' -> c'''' -> d) -> a b c -> a b c' -> a b c'' -> a b c''' -> a b c'''' -> a b d
liftArrow5 f a1 a2 a3 a4 a5 = arr (\a -> (a, (a, (a, (a, a))))) >>> (a1 *** a2 *** a3 *** a4 *** a5) >>> arr (\(a, (b, (c, (d, e)))) -> f a b c d e)

maybeA :: ArrowList a => a b c -> a b (Maybe c)
maybeA a = listToMaybe ^<< listA a

arrMaybe :: ArrowList a => (b -> Maybe c) -> a b c
arrMaybe f = arrL (maybeToList . f)

a >>? b = a >>> arrMaybe b

combinedChildText :: ArrowXml a => a XmlTree String
combinedChildText = getChildren >>> getText >. concat

readCombinedChildText :: (ArrowXml a, Read b) => a XmlTree b
readCombinedChildText = combinedChildText >>^ read

rightOrFail msg (Left err) = error $ msg ++ show err
rightOrFail _ (Right v) = v

parseCombinedChildText :: ArrowXml a => Parsec String () o -> a XmlTree o
parseCombinedChildText p = combinedChildText >>^ (\v -> rightOrFail "Parsing text node: " (parse p "XML text node" v))

chunkList _ [] = []
chunkList dim l = let
    (h, t) = splitAt dim l
  in
     h:(chunkList dim t)

(!!?) :: Data.Foldable.Foldable f => f a -> Int -> Maybe a
f !!? i = either Just (const Nothing) $
            Data.Foldable.foldl (\a v -> a >>=
                                         \idx -> if idx == 0 then Left v else Right (idx - 1))
                                 (Right i) f
