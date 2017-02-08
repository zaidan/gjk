{-| Support functions
-}
module GJK.Support
  ( polySupport
  , isCollision
  ) where

import GJK.Point (Pt, dot)
import GJK.Collision (collision)

import Data.Maybe (fromMaybe)

-- | Support function
polySupport :: [Pt] -> Pt -> Maybe Pt
polySupport list d =
    let
        dotList = fmap (dot d) list
        decorated = zip dotList list
        maybemax = safeMaximum decorated
    in
        case maybemax of
        Just (_, p) -> Just p
        _  -> Nothing

-- | Find the maximum in given list and returns the result as Maybe.
safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum [] = Nothing
safeMaximum list = Just $ maximum list

-- | Check if a and b collide
isCollision :: [Pt] -> [Pt] -> Bool
isCollision a b =
  fromMaybe False $ collision 1 (a, polySupport) (b, polySupport)
