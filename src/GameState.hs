module GameState where

import Graphics.Rendering.OpenGL

-- Global game state
data State = State CameraPosition

-- Camera Position
data CameraPosition = CameraPosition GLfloat