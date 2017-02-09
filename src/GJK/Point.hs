{-| Point
-}

module GJK.Point
  ( Pt
  , dot
  , sub
  , from
  , add
  , mul
  , neg
  , cross2D
  , trip
  , perp
  , isSameDirection
  , getDirectionVector
  ) where

-- | Simple alias for a two dimentional point
type Pt = (Double, Double)

-- | Dot product of given points
dot :: Pt -> Pt -> Double
dot (x1,y1) (x2,y2) = (x1*x2) + (y1*y2)

-- | Subtract a from b
sub :: Pt -> Pt -> Pt
sub (ax, ay) (bx,by) = (bx-ax, by-ay)

-- | Alias for sub - reads better in some cases - as in "from a to b"
from :: Pt -> Pt -> Pt
from  = sub

-- | Add a and b
add :: Pt -> Pt -> Pt
add (ax, ay) (bx,by) = (bx+ax, by+ay)

-- | Multiply x and y with n
mul :: Double -> Pt -> Pt
mul n (x,y) = (n*x, n*y)

-- | Negation
neg :: Pt -> Pt
neg (x,y) = (-x,-y)

-- | 2D cross product
cross2D :: Pt -> Pt -> Double
cross2D (ax, ay) (bx, by) = ax*by - ay*bx

-- | Implements a x (b x c) = b(a.c) - c(a.b)
trip :: Pt -> Pt -> Pt -> Pt
trip a b c =
  let
    bac = mul (dot a c) b
    cab = mul (dot a b) c
  in
    sub cab bac

-- | Perpendicular to a in direction of b (2D case)
perp :: Pt -> Pt -> Pt
perp a b = trip a b a

-- | Check if the dot product of a and by is greater than 0 and the direction
-- is same.
isSameDirection :: Pt -> Pt -> Bool
isSameDirection a b = dot a b > 0

-- | Get the direction vector of given points.
-- Pass bc as first parameter.
getDirectionVector :: Pt -> Pt -> Pt
getDirectionVector (x1, y1) (x2, y2) =
  let
    -- try the triple cross product
    d = trip (x1,y1) (x2,y2) (x1,y1)
    collinear = (d == (0,0))

    -- bc and c0 are collinear and gave us a 0 cross product.
    -- inject bc and c into 3d and get ANY perp (the algorithm should
    -- not care in this case) so now crossing bc=(x1,y1,0) with c=(-x2, -y2, 1)
    -- remember the second parameter was c0 = -c
    --
    -- (u2v3 - u3v2)i - (u1v3 - u3v1)j + (who cares)k = bc x c
    -- so...
    -- px = (y1*1 - 0*(-y2)),  py = -(x1*1 - 0*(-x2))   
    -- px = y1                 py = -x1
    -- 
    -- which is a detailed derivation of the obvious :)
  in
    if collinear then (y1, -x1) else d
