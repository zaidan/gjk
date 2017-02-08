{-| this module provides an implementation of the
Gilbert-Johnson-Keerthi (GJK) collision detection algorith for
convex objects in 2D. To deal with concave objects, simply
break your larger object into smaller convex shapes.

It is very efficient, usually converging in one or two iterations.
-}

module GJK.Collision
  ( collision
  ) where

import GJK.Point (Pt, dot, neg, from, perp, getDirectionVector, isSameDirection)
import GJK.Mink (Mink, calcMinkSupport)

{-| Determine if there is a collision between two objects.
Object information is given as a pair of: a boundary representation
of type a, and a support function for that representaion f :: a -> Pt -> Pt
which takes the boundary representation and a direction vector, and
returns the point of the boundary furthest along the direction.
Pt here is used as an alias for (Double, Double). The first argument
to collision is max recursion depth, which might come in handy in
the case where you are writing your own support functions.

  poly1 = [(-15,-10),(0,15),(12,-5)] 
  poly2 = [(-9,13),(6,13),(-2,22)] 

  collision 10 (poly1, polySupport) (poly2, polySupport) == Just True
-}
collision :: Int -> Mink a -> Mink b -> Maybe Bool
collision limit minkA minkB =
  let
    d1 = (1.0, 0.0)
    d2 = neg d1
    maybec = calcMinkSupport minkA minkB d1
    maybeb = calcMinkSupport minkA minkB d2
  in
    case (maybec, maybeb) of
      (Just c, Just b) -> collision2 limit minkA minkB c b
      _                -> Nothing

-- | Prepare the input for doSimplex and start the calculation.
collision2 :: Int -> Mink a -> Mink b -> (Double, Double) -> (Double, Double) -> Maybe Bool
collision2 limit minkA minkB c b =
  let
    -- simplex is cb and direction is (cb x c0 x cb)
    cb = from c b
    c0 = neg c
    d = getDirectionVector cb c0
    simplexResult = doSimplex limit 0 minkA minkB ([b,c], d)
  in
    case simplexResult of
      Just (intersects, _) -> Just intersects
      _                    -> Nothing

{-| The algorithm proceeds by continually trying to surround the origin with a
2-simplex (a triangle) using points on boundary of the Minkowski sum of the two
objects. If no such triangle can be drawn, there is no collision. Since we are
building a triangle, only three points ever need to be kept track of,
colloquially named a, b and c, with the most recently added point being called
a, next most recent b, and then c as the earliest added of the bunch. If a
triangle is built that DOESN'T contain the origin the algorithm finds the
component of the triangle closest to the origin (either a 0-simplex or
1-simplex (a point or a line segment), calls that the new working simplex, and
proceeds to add a new point to try and construct a new 2-simplex (that new
point is always called a, and used as the new point of reference or "origin" so
to speak) a is obtained by finding the direction of the origin from the
currently building simplex, and finding the extremal point on the boundry in
that direction.
-}
doSimplex :: Int -> Int -> Mink a  -> Mink b -> ([Pt], Pt) -> Maybe (Bool, ([Pt], Pt))
doSimplex limit depth minkA minkB (sim, d) =
  let
    maybea = calcMinkSupport minkA minkB d
  in
    case maybea of
      Just a -> doSimplex2 limit depth minkA minkB (sim, d) a
      _ -> Nothing

-- | Actual doSimplex calculation. doSimplex handles the error case of
-- calcMinkSupport and feeds a real a to doSimplex2.
doSimplex2 :: Int -> Int -> Mink a  -> Mink b -> ([Pt], Pt) -> (Double, Double) -> Maybe (Bool, ([Pt], Pt))
doSimplex2 limit depth minkA minkB (sim, d) a =
  let
    notPastOrig = (dot a d < 0)
    -- if not past origin, there is no intersection
    --b = unsafeHead sim
    (intersects, (newSim, newDir)) = enclosesOrigin a sim
  in
    if notPastOrig then
      Just (False, ([], (fromIntegral depth, fromIntegral depth)))
    else if intersects then
      Just (True, (sim, a))
    else if depth > limit then
      Just (False, (newSim, newDir))
    else
      doSimplex limit (depth+1) minkA minkB (newSim, newDir)

{-
 - The handleNSimplex functions are named for the size of the simplex BEFORE a is added :)
 - the "handle" functions are responsible for determining enclosure, but also providing
 - a new search direction based on the simplex's relation to the origin, in the case
 - where there is no containment.
 -}
enclosesOrigin :: Pt -> [Pt] -> (Bool, ([Pt], Pt))
enclosesOrigin a sim =
  case sim of
    -- 0-simplex case
    [b]     -> handle0Simplex a b
    [b, c] -> handle1Simplex a b c
    _          -> (False, (sim,(0,0)))


-- | Simplex is a single point, we will be adding a to the simplex one way or
-- another (the new simplex will be either [a,b] or [a])
handle0Simplex :: Pt -> Pt -> (Bool, ([Pt], Pt))
handle0Simplex a b =
  let
    -- line given by adding our new point
    ab = from a b
    -- direction of a from the origin 
    a0 = neg a
    (newSim, newDir) = if isSameDirection ab a0 then ([a,b], perp ab a0) else ([a], a0)
  in
    (False, (newSim, newDir))

-- | Simplex is a line segment [b,c], adding 'a' gives us a 2-Simplex. We now
-- either enclose the origin, or will be replacing the simplex with the closest
-- sub-component to the origin.
handle1Simplex :: Pt -> Pt -> Pt -> (Bool, ([Pt], Pt))
handle1Simplex a b c =
  let
    -- a is our new local point of reference
    a0 = neg a
    ab = from a b
    ac = from a c
    -- perpendicular to ab facing away from c
    abp = perp ab (neg ac)
    -- perpendicular to ac facing away from a
    acp = perp ac (neg ab)
  in
    if isSameDirection abp a0 then
      -- region 4 or 5n 
      if isSameDirection ab a0 then (False, ([a,b], abp)) else (False, ([a], a0))
    else if isSameDirection acp a0 then
      -- region 6 or 5
      if isSameDirection ac a0 then (False, ([a,c], acp)) else (False, ([a], a0))
    else
      (True, ([b,c], a0))
