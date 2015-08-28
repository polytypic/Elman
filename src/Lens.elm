module Lens where

type alias Lens s x =
  { get: s -> x
  , set: s -> x -> s
  }

(=>): Lens x y -> Lens y z -> Lens x z
(=>) xy yz =
  { get = xy.get >> yz.get
  , set = \x -> xy.set x << yz.set (xy.get x) }
