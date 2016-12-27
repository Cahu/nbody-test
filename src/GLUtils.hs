module GLUtils
( mkShader
, checkProgram
, bindBufferAndApply
)

where

import Control.Monad

import Foreign.ForeignPtr

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import qualified Data.ByteString as BS

import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL


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

bindBufferAndApply :: (V.Storable a) => GL.BufferObject -> Int -> (Vector a -> IO b) -> IO b
bindBufferAndApply buff size f = do
    GL.bindBuffer GL.ShaderStorageBuffer $= Just buff
    result <- GL.withMappedBuffer GL.ShaderStorageBuffer GL.ReadOnly
        (newForeignPtr_ >=> \ptr' -> f (V.unsafeFromForeignPtr0 ptr' size))
        (error . show)
    GL.bindBuffer GL.ShaderStorageBuffer $= Nothing
    return result

