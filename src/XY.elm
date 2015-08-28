module XY where

import Graphics.Collage exposing (..)

type alias XY' s r = {r | x: s, y: s}
type alias XY s = XY' s {}

diag: s -> s -> XY (XY s)
diag z o = {x = {x = o, y = z},
            y = {x = z, y = o}}

toPair: XY' a r -> (a, a)
toPair {x, y} = (x, y)

ofPair: (a, a) -> XY a
ofPair (x, y) = {x = x, y = y}

ofScalar: a -> XY a
ofScalar n = {x = n, y = n}

ofCurry: a -> a -> XY a
ofCurry x y = {x = x, y = y}

curryTo: (a -> a -> b) -> XY' a r -> b
curryTo k {x, y} = k x y

move: XY' Float r -> Form -> Form
move = toPair >> Graphics.Collage.move

map: (a -> b) -> XY' a r -> XY' b r
map f v = {v | x <- f v.x, y <- f v.y}

zipWith: (a -> b -> c) -> XY' a d -> XY' b e -> XY c
zipWith f l r = {x = f l.x r.x, y = f l.y r.y}

sumWith: (a -> a -> b) -> XY' a c -> b
sumWith f v = f v.x v.y

type alias Bop s l r = XY' s l -> XY' s r -> XY s

(|+|): Bop number l r
(|+|) = zipWith (+)

(|-|): Bop number l r
(|-|) = zipWith (-)

(|*|): Bop number l r
(|*|) = zipWith (*)

(|/|): Bop number l r
(|/|) = zipWith (/)

(|*): XY' number r -> number -> XY number
(|*) v s = v |*| ofScalar s

(*|): number -> XY' number r -> XY number
(*|) s v = ofScalar s |*| v

(|/): XY' Float r -> Float -> XY Float
(|/) v s = v |* (1.0 / s)

(|<|): XY' number l -> XY' number r -> Bool
(|<|) l r = l.x < r.x && l.y < r.y

(|<=|): XY' number l -> XY' number r -> Bool
(|<=|) l r = l.x <= r.x && l.y <= r.y

dot: XY' number a -> XY' number b -> number
dot l r = l |*| r |> sumWith (+)

norm: XY' number r -> number
norm v = dot v v

abs: XY' Float r -> Float
abs v = norm v |> sqrt

neg: XY number -> XY number
neg = map ((*) -1)
