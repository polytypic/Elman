module StateM where

import Lens exposing (Lens)

--

type alias StateM s x = s -> (x, s)

--

run: StateM s () -> s -> s
run uS s = uS s |> snd

--

infixl 0 >>=

(>>=): StateM s x -> (x -> StateM s y) -> StateM s y
(>>=) xS x2yS s = xS s |> uncurry x2yS

infixl 0 >>.

(>>.): StateM s () -> StateM s y -> StateM s y
(>>.) xS yS = xS >>= \() -> yS

infixl 0 >=>

(>=>): (x -> StateM s y) -> (y -> StateM s z) -> (x -> StateM s z)
(>=>) x2yS y2zS x = x2yS x >>= y2zS

return: x -> StateM s x
return x s = (x, s)

--

getState: StateM s s
getState s = (s, s)

setState: s -> StateM s ()
setState s _ = ((), s)

--

set: Lens s x -> x -> StateM s ()
set {set} x s = ((), set s x)

infixl 1 :=

(:=): Lens s x -> x -> StateM s ()
(:=) = set

get: Lens s x -> StateM s x
get {get} s = (get s, s)

upd: Lens s x -> (x -> x) -> StateM s ()
upd {get, set} x2x s = ((), set s (x2x (get s)))

infixl 1 :>

(:>): Lens s x -> (x -> x) -> StateM s ()
(:>) = upd

updM: Lens s x -> (x -> StateM s x) -> StateM s ()
updM sLx x2xS = get sLx >>= x2xS >>= set sLx

--

seq: List (StateM s ()) -> StateM s ()
seq ops =
  case ops of
    []      -> return ()
    [op]    -> op
    op::ops -> op >>= \() -> seq ops

chooseM: (x -> StateM s (Maybe y)) -> List x -> StateM s (List y)
chooseM x2yMS =
  let lp ys xs =
        case xs of
          [] -> List.reverse ys |> return
          x::xs -> x2yMS x >>= \yM ->
                   case yM of
                     Nothing -> lp ys xs
                     Just y -> lp (y::ys) xs
  in lp []

mapM: (x -> StateM s y) -> List x -> StateM s (List y)
mapM x2yS = chooseM (x2yS >=> (Just >> return))

filterM: (x -> StateM s Bool) -> List x -> StateM s (List x)
filterM x2bS =
  chooseM <| \x ->
  x2bS x >>= \b ->
  return <| if b then Just x else Nothing

--

delay: (() -> StateM s x) -> StateM s x
delay u2xS s = u2xS () s

--

when: Bool -> (() -> StateM s ()) -> StateM s ()
when b uS = if b then uS () else return ()

repeat: StateM s () -> Int -> StateM s ()
repeat uS n = if 0 < n then uS >>. repeat uS (n-1) else return ()
