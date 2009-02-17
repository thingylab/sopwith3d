module Rendering(
					renderObj,
					ObjLine(ObjV, ObjVn, ObjF, OtherStuff)) where

import Graphics.UI.GLUT
import Control.Monad

data ObjFace = FaceTriangle [Vertex3 GLfloat]
	| FaceQuad [Vertex3 GLfloat]
	deriving Show

data ObjLine = ObjV (Vertex3 GLfloat)
	| ObjVn (Normal3 GLfloat)
	| ObjF [Int] [Int]					-- list of vertices, list of normals
	| OtherStuff						-- ignored lines
	deriving Show

{-
	All these lookups in renderFace are very inefficient.
	TODO: transform the ObjLine object first in a friendlier list
	that can easily be mapM_-ed.
-}
renderObj (vs,ns, fs) =
	mapM_ (renderFace vs ns) fs

renderFace vs ns f@(ObjF vi ni) =
	case length vi of
		3 -> renderPrimitive Triangles $ do
			mapM_ vertex vertices
			mapM_ normal normals
		4 -> renderPrimitive Quads $ do
			mapM_ vertex vertices
			mapM_ normal normals
		where
			vertices = map (findVertices vs) vi
			normals = map (findNormals ns) ni

findVertices vs vn =
	v
	where
		ObjV v = vs !! (vn - 1)

findNormals ns nn =
	n
	where
		ObjVn n = ns !! (nn - 1)

{-renderFileObject (FileObject fo) =
	mapM_ renderFace fo

renderFace (FaceTriangle vs) =
	renderPrimitive Triangles $ mapM_ vertex vs
renderFace (FaceQuad vs) =
	renderPrimitive Quads $ mapM_ vertex vs-}