module Main where

import Lib

import Data.IORef
import System.Random
import Control.Monad
import Control.Concurrent

import Linear
import Graphics.UI.GLUT
import qualified Graphics.Rendering.OpenGL as GL

import qualified Data.ByteString as BS



main :: IO ()
main = do
    win  <- init
    --sys  <- newIORef $ System [ b1, b2 ]
    let (randSys, _) = randomSystem 300 (mkStdGen 2)
    sys <- newIORef $ randSys
    displayCallback $= display sys
    reshapeCallback $= Just (reshape sys)
    idleCallback    $= Just (idle    sys)
    mainLoop
 
  where
    b1 = Body 1 1000  (V3   1000  0 0) (V3 0 0 0)
    b2 = Body 2 1000  (V3 (-1000) 0 0) (V3 0 0 0)

    init :: IO Window
    init = do
        initialDisplayMode $= [DoubleBuffered]
        (progName, _) <- getArgsAndInitialize
        createWindow progName

    debug :: System -> IO ()
    debug sys = mapM_ (print) (systemBodies sys) >> putStrLn "---"

    display :: IORef System -> DisplayCallback
    display system = do
        sys <- readIORef system
        --debug sys
        clear [ ColorBuffer, DepthBuffer ]
        renderPrimitive Points $ mapM_ (vertex . vertexFromBody) (systemBodies sys)
        flush
        swapBuffers

    idle :: IORef System -> IdleCallback
    idle system = do
        modifyIORef system (systemTick 1)
        postRedisplay Nothing

    reshape :: IORef System -> ReshapeCallback
    reshape system size = do
        viewport $= (Position 0 0, size)
        postRedisplay Nothing

