module OBJParser(loadOBJ) where

{-
	Pseudo-parser for the OBJ file format.
	The actual specification is here: 
	http://people.sc.fsu.edu/~burkardt/txt/obj_format.txt
	
	
	
	This implementation has the following limitations:
	- It only considers the vertices, normals and faces. Everything else is ignored.
	
	TODO: Move all the parsing to Parsec, while we're at it...
-}

import Graphics.UI.GLUT
import Control.Monad

import Rendering

import Text.ParserCombinators.Parsec	-- this requires "-package parsec"

{-
	Load a .OBJ file.
	The result is a triple: ([vertices], [normals], [faces]) such as:
	(
		[ObjV (Vertex3 1.0 (-1.0) (-1.0)), ...],
		[ObjVn (Normal3 0.0 (-1.0) 0.0), ...],
		[ObjF [1,2,3,4] [1,1,1,1], ...]
	)
-}
loadOBJ :: String -> IO ([ObjLine], [ObjLine], [ObjLine])
loadOBJ file = do
	ls <- getLines file
	return $ foldr parseObjLine ([], [], []) ls

parseObjLine l (v, vn, f) = 
	case parseSingleObjLine l of
		vert@(ObjV _) 	-> (vert:v, vn, f)
		norm@(ObjVn _) 	-> (v, norm:vn, f)
		face@(ObjF _ _) -> (v, vn, face:f)
		OtherStuff		-> (v, vn, f)

parseSingleObjLine line = 
	case l of
		"v":xs 	-> makeVertex xs
		"vn":xs	-> makeNormal xs
		"f":xs	-> makeFace xs
		_ 		-> OtherStuff
	where l = words line

makeVertex [x, y, z] = ObjV (Vertex3 (read x) (read y) (read z))
makeNormal [x, y, z] = ObjVn (Normal3 (read x) (read y) (read z))
makeFace xs = 
	foldr 
		(\(y, z) (ObjF ws xs) -> (ObjF (y:ws) (z:xs)))
		(ObjF [] []) 
		cs
	where cs = map splitToken xs

{-
	Parsing of the `face' part (actually just the "vertex//normal" part).
-}
tokenParser :: Parser (Int, Int)
tokenParser = do
	n1 <- many1 digit
	string "//"
	n2 <- many1 digit
	return (read n1, read n2)

splitToken :: String -> (Int, Int)
splitToken input
        = case (parse tokenParser "" input) of
            Left err -> (0, 0)
            Right x  -> x

getLines = liftM lines . readFile