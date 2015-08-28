module Signals where

foldpFrom: s -> (x -> s -> s) -> Signal x -> Signal s
foldpFrom s x2s2s xs =
  Signal.foldp x2s2s s xs
