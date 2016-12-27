{-# LANGUAGE BangPatterns #-}

module Lib
( Body(..)
, randomBody

, System(..)
, randomSystem
, systemTick

, vertexFromBody
, vertexFromVec

) where


import Linear

import System.Random
import Control.Monad.State

import Graphics.UI.GLUT


data Body = Body
    { bodyId       :: Int
    , bodyMass     :: !Double
    , bodyPosition :: V3 Double
    , bodyVelocity :: V3 Double
    }
  deriving (Show)


instance Eq Body where
    b1 == b2  =  bodyId b1 == bodyId b2


randomBody :: (RandomGen g) => (Double, Double) -> g -> (Int -> Body, g)
randomBody (mLo, mHi) g0 =
    let (m,  g1) = randomR (mLo,   mHi) g0
        (t,  g2) = randomR (-10,    10) g1
        (p,  g3) = randomR (  0,    10) g2
        (r,  g4) = randomR (-maxDist, maxDist) g3
        x        = r * cos t * sin p
        y        = r * sin t * sin p
        z        = r * cos p
    in
        (\idx -> Body idx m (V3 x y z) (V3 0 0 0), g4)
  where
    maxDist = 1000


newtype System = System { systemBodies :: [Body] }



-- gravitational constant
bigG :: Double
bigG = 1
--bigG = 6.674 * 10**(-11)


randomSystem :: (RandomGen g) => Int -> g -> (System, g)
randomSystem n g0 =
    let (bodyBuilders, g1) = runState (replicateM n st) g0
        bodies             = zipWith ($) bodyBuilders [ 1 .. ] -- assign IDs to bodies
    --in  (System  (blackHole : bodies), g1)
    in  (System  bodies, g1)
  where
    st = state $ randomBody (10, 500)
    blackHole = Body 0 10000 (V3 0 0 0) (V3 0 0 0)


systemTick :: Double -> System -> System
systemTick tick (System s) = System $ map (updatePosition . applyBodiesForces s) s
  where
    -- apply forces exerced by the lisf of bodies 'bs' to the body 'b'
    applyBodiesForces bs !b = foldr aux b bs

    -- filter function so that we don't apply the gravity of a body on itself
    aux b thisBody
        | b == thisBody = thisBody
        | otherwise     = applyForce b thisBody

    -- change b2's parameters by applying the force exerced by b1
    applyForce !b1 !b2 = b2 { bodyVelocity = bodyVelocity b2 ^+^ tick *^ gravityAccel b1 b2 }

    -- change a body's position using the velocity vector
    updatePosition !b = b { bodyPosition = bodyPosition b ^+^ tick *^ bodyVelocity b }


-- acceleration resulting from the force exerced by b1 on b2
gravityAccel :: Body -> Body -> V3 Double
gravityAccel !b1 !b2 =
    let relativePosition = bodyPosition b1 ^-^ bodyPosition b2
        dist             = norm relativePosition
    in  (bigG * bodyMass b1 / (dist**2 + 30**2)**(3/2)) *^ relativePosition


vertexFromBody :: Body -> [GLfloat]
vertexFromBody b = vertexFromVec (bodyPosition b) ++ [realToFrac $ bodyMass b]

vertexFromVec :: V3 Double -> [GLfloat]
vertexFromVec (V3 x y z) = [(realToFrac x), (realToFrac y), (realToFrac z)]
