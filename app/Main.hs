module Main where

import Lib
import GLUtils

import Data.IORef
import Text.Printf
import System.Clock
import System.Random

import Foreign.Ptr      (nullPtr)
import Foreign.Storable (sizeOf)

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import Graphics.Rendering.OpenGL (GLfloat, ($=))
import qualified Graphics.UI.GLUT          as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GL               as GLRaw


data Perf = Perf
    { perfTime :: TimeSpec
    , perfDiff :: TimeSpec
    }

nullPerf :: Perf
nullPerf = Perf
    { perfTime = 0
    , perfDiff = 0
    }

refreshPerf :: Perf -> IO Perf
refreshPerf perf = do
    tm <- getTime Monotonic
    return $ perf { perfTime = tm
                  , perfDiff = tm - perfTime perf
                  }

numStars :: (Num a) => a
numStars = 12*1024

groupSize :: (Num a) => a
groupSize = 1024


makePhysicsProgram :: IO GL.Program
makePhysicsProgram = do
    shader  <- mkShader GL.ComputeShader "shaders/compute.glsl"
    program <- GL.createProgram
    GL.attachShader program shader
    GL.linkProgram program
    GL.detachShader program shader
    checkProgram program
    return program


makeRenderProgram :: IO GL.Program
makeRenderProgram = do
    vShader <- mkShader GL.VertexShader   "shaders/vertex.glsl"
    fShader <- mkShader GL.FragmentShader "shaders/fragment.glsl"
    pgrm <- GL.createProgram
    GL.attachShader pgrm vShader
    GL.attachShader pgrm fShader
    GL.linkProgram pgrm
    GL.detachShader pgrm vShader
    GL.detachShader pgrm fShader
    checkProgram pgrm
    return pgrm


floatsArraySize :: (V.Storable a, Num b) => Vector a -> b
floatsArraySize v 
    | V.null v  = 0
    | otherwise = fromIntegral $ V.length v * sizeOf (V.head v)


makeSSBO :: Vector GLfloat -> IO GL.BufferObject
makeSSBO floats = do
    ssbo <- GL.genObjectName
    GL.bindBuffer GL.ShaderStorageBuffer $= Just ssbo
    V.unsafeWith floats $ \v -> do
        let size = floatsArraySize floats
        GL.bufferData GL.ShaderStorageBuffer $= (size, v, GL.StreamDraw)
    GL.bindBuffer GL.ShaderStorageBuffer $= Nothing
    return ssbo


makeEmptySSBO :: (Integral a ) => a -> IO GL.BufferObject
makeEmptySSBO size = do
    ssbo <- GL.genObjectName
    GL.bindBuffer GL.ShaderStorageBuffer $= Just ssbo
    GL.bufferData GL.ShaderStorageBuffer $= (fromIntegral size, nullPtr, GL.StreamDraw)
    GL.bindBuffer GL.ShaderStorageBuffer $= Nothing
    return ssbo


makeVaoForBuffer :: GL.BufferObject -> IO GL.VertexArrayObject
makeVaoForBuffer ssbo = do
    vao <- GL.genObjectName
    GL.bindVertexArrayObject     $= Just vao
    GL.bindBuffer GL.ArrayBuffer $= Just ssbo
    GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float 0 nullPtr)
    GL.vertexAttribArray   (GL.AttribLocation 0) $= GL.Enabled
    GL.bindVertexArrayObject     $= Nothing
    GL.bindBuffer GL.ArrayBuffer $= Nothing
    return vao


main :: IO ()
main = do
    _ <- initGLUT

    perf <- newIORef nullPerf
    seed <- fromIntegral . toNanoSecs <$> getTime Monotonic

    let bodies     = fst $ randomSystem numStars (mkStdGen seed)
        vertices   = V.fromList $ concatMap vertexFromBody (systemBodies bodies)
        velocities = V.replicate (4*numStars) 0

    {- setup SSBOs -}
    ssboPos <- makeSSBO vertices
    ssboVel <- makeSSBO velocities

    {- setup physics program -}
    physicsProg <- makePhysicsProgram

    {- setup renderer program -}
    rendererProg <- makeRenderProgram

    {- setup VAOs for rendering -}
    vao <- makeVaoForBuffer ssboPos

    --sys  <- newIORef $ System [ b1, b2 ]
    --let (randSys, _) = randomSystem 300 (mkStdGen 2)
    --sys <- newIORef $ randSys

    GLUT.displayCallback $= display rendererProg vao
    GLUT.reshapeCallback $= Just reshape
    GLUT.idleCallback    $= Just (idle perf physicsProg ssboPos ssboVel)
    GLUT.mainLoop
 
  where
    initGLUT :: IO GLUT.Window
    initGLUT = do
        GLUT.initialDisplayMode $= [GLUT.DoubleBuffered]
        (progName, _) <- GLUT.getArgsAndInitialize
        GLUT.createWindow progName

    display :: GL.Program -> GL.VertexArrayObject -> GLUT.DisplayCallback
    display pgrm vao = do
        GLUT.clear [ GLUT.ColorBuffer, GLUT.DepthBuffer ]
        GL.currentProgram        $= Just pgrm
        GL.bindVertexArrayObject $= Just vao
        GL.drawArrays GL.Points 0 numStars
        GL.bindVertexArrayObject $= Nothing
        GLUT.swapBuffers

    idle :: IORef Perf -> GL.Program -> GL.BufferObject -> GL.BufferObject -> GLUT.IdleCallback
    idle perfRef pgrm ssboPos ssboVel = do
        updateAndPrintFPS perfRef
        GL.currentProgram $= Just pgrm
        GL.bindBufferBase GL.IndexedShaderStorageBuffer 0 $= Just ssboPos
        GL.bindBufferBase GL.IndexedShaderStorageBuffer 1 $= Just ssboVel
        GLRaw.glDispatchCompute (numStars `div` groupSize) 1 1
        GLRaw.glMemoryBarrier GLRaw.GL_SHADER_STORAGE_BARRIER_BIT
        --bindBufferAndApply ssboPos (numStars * 4) printVectorGLfloat
        GLUT.postRedisplay Nothing

    reshape :: GLUT.ReshapeCallback
    reshape size = do
        GLUT.viewport $= (GLUT.Position 0 0, size)
        GLUT.postRedisplay Nothing

    printVectorGLfloat :: Vector GLfloat -> IO ()
    printVectorGLfloat = print

    updateAndPrintFPS :: IORef Perf -> IO ()
    updateAndPrintFPS perfRef = do
        perf  <- readIORef perfRef
        perf' <- refreshPerf perf
        writeIORef perfRef perf'
        let fps = (1000000000 :: Double) / fromIntegral (toNanoSecs $ perfDiff perf')
        printf "%.2f FPS\n" fps
