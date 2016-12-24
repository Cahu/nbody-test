module Main where

import Lib

import Data.IORef
import System.Random
import Control.Monad
import Control.Concurrent

import Linear

import Foreign.Ptr      (nullPtr)
import Foreign.Storable (sizeOf)

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import Graphics.Rendering.OpenGL (GLfloat, ($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT          as GLUT

import qualified Data.ByteString as BS


newtype Swapper a = Swapper { getPair :: (a, a) }

data ObjectBundle = ObjectBundle
    { vao :: GL.VertexArrayObject
    , vbo :: GL.BufferObject
    }

swap :: Swapper a -> Swapper a
swap (Swapper (a, b)) = Swapper (b, a)


mkShader :: GL.ShaderType -> FilePath -> IO GL.Shader
mkShader shaderType sourcePath = do
    s <- GL.createShader shaderType
    c <- BS.readFile sourcePath
    GL.shaderSourceBS s $= c
    GL.compileShader s
    status <- GL.compileStatus s
    unless status $ do
        info <- GL.shaderInfoLog s
        error info
    return s


checkProgram :: GL.Program -> IO ()
checkProgram program = do
    status <- GL.linkStatus program
    unless status $ do
        info <- GL.programInfoLog program
        error info


makePhysicsProgram :: IO GL.Program
makePhysicsProgram = do
    shader  <- mkShader GL.VertexShader "shaders/physics.glsl"
    program <- GL.createProgram
    GL.attachShader program shader
    GL.setTransformFeedbackVaryings program ["oValue"] GL.InterleavedAttribs
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


verticesArraySize :: (V.Storable a, Num b) => Vector a -> b
verticesArraySize v 
    | V.null v  = 0
    | otherwise = fromIntegral $ V.length v * sizeOf (V.head v)


makeVBO :: Vector GLfloat -> IO GL.BufferObject
makeVBO vertices = do
    vbo <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    V.unsafeWith vertices $ \v -> do
        let size = verticesArraySize vertices
        GL.bufferData GL.ArrayBuffer $= (size, v, GL.StreamDraw)
    GL.bindBuffer GL.ArrayBuffer $= Nothing
    return vbo


makeEmptyVBO :: (Integral a ) => a -> IO GL.BufferObject
makeEmptyVBO size = do
    vbo <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    GL.bufferData GL.ArrayBuffer $= (fromIntegral size, nullPtr, GL.StreamDraw)
    GL.bindBuffer GL.ArrayBuffer $= Nothing
    return vbo


makeVaoForBuffer :: GL.BufferObject -> IO GL.VertexArrayObject
makeVaoForBuffer vbo = do
    vao <- GL.genObjectName
    GL.bindVertexArrayObject     $= Just vao
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)
    GL.vertexAttribArray   (GL.AttribLocation 0) $= GL.Enabled
    GL.bindVertexArrayObject     $= Nothing
    GL.bindBuffer GL.ArrayBuffer $= Nothing
    return vao


main :: IO ()
main = do
    win <- init

    {- setup VBOs -}
    vboA <- makeVBO      vertices
    vboB <- makeEmptyVBO (verticesArraySize vertices)

    {- setup physics program -}
    physicsProg <- makePhysicsProgram

    {- setup renderer program -}
    rendererProg <- makeRenderProgram

    {- setup VAOs for rendering -}
    vaoA <- makeVaoForBuffer vboA
    vaoB <- makeVaoForBuffer vboB

    swapper <- newIORef $ Swapper (ObjectBundle vaoA vboA, ObjectBundle vaoB vboB)

    --sys  <- newIORef $ System [ b1, b2 ]
    --let (randSys, _) = randomSystem 300 (mkStdGen 2)
    --sys <- newIORef $ randSys

    GLUT.displayCallback $= display rendererProg swapper
    GLUT.reshapeCallback $= Just reshape
    GLUT.idleCallback    $= Just (idle physicsProg swapper)
    GLUT.mainLoop
 
  where
    vertices = V.fromList
        [ ( 0.0), ( 0.5), 0
        , ( 0.5), (-0.5), 0
        , (-0.5), (-0.5), 0
        ]

    b1 = Body 1 1000  (V3   1000  0 0) (V3 0 0 0)
    b2 = Body 2 1000  (V3 (-1000) 0 0) (V3 0 0 0)

    init :: IO GLUT.Window
    init = do
        GLUT.initialDisplayMode $= [GLUT.DoubleBuffered]
        (progName, _) <- GLUT.getArgsAndInitialize
        GLUT.createWindow progName

    debug :: System -> IO ()
    debug sys = mapM_ (print) (systemBodies sys) >> putStrLn "---"

    display :: GL.Program -> IORef (Swapper ObjectBundle) -> GLUT.DisplayCallback
    display pgrm swapperRef = do
        (bundle, _) <- getPair <$> readIORef swapperRef
        GLUT.clear [ GLUT.ColorBuffer, GLUT.DepthBuffer ]
        GL.currentProgram        $= Just pgrm
        GL.bindVertexArrayObject $= Just (vao bundle)
        GL.drawArrays GL.Triangles 0 3
        GL.bindVertexArrayObject $= Nothing
        GLUT.swapBuffers

    idle :: GL.Program -> IORef (Swapper ObjectBundle) -> GLUT.IdleCallback
    idle pgrm swapperRef = do
        (bundleA, bundleB) <- getPair <$> readIORef swapperRef
        GL.currentProgram    $= Just pgrm
        GL.bindVertexArrayObject                              $= Just (vao bundleA)
        GL.bindBufferBase GL.IndexedTransformFeedbackBuffer 0 $= Just (vbo bundleB)
        GL.rasterizerDiscard $= GL.Enabled
        GL.beginTransformFeedback GL.Points
        GL.drawArrays GL.Points 0 3
        GL.endTransformFeedback
        GL.rasterizerDiscard $= GL.Disabled
        GL.bindBufferBase GL.IndexedTransformFeedbackBuffer 0 $= Nothing
        GL.bindVertexArrayObject                              $= Nothing
        modifyIORef swapperRef swap
        GLUT.postRedisplay Nothing

    reshape :: GLUT.ReshapeCallback
    reshape size = do
        GLUT.viewport $= (GLUT.Position 0 0, size)
        GLUT.postRedisplay Nothing
