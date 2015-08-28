module Lists where

import List

foldlFrom: s -> (x -> s -> s) -> List x -> s
foldlFrom s f xs = List.foldl f s xs
