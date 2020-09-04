{-
   Name: Sergio Delgado
   G#:   00512529
-}

module Project3
  ( width
  , height
  , energyAt
  , energies
  , nextGroups
  , buildNodeFromPacket
  , buildRowFromPackets
  , packetsToNodes
  , findVerticalPath
  , findHorizontalPath
  , removeVerticalPath
  , removeHorizontalPath
  , gridToFile
  , fileToGrid
  , Grid()
  , Node(..)
  , RGB(..)
  )
where

-- optional: trace :: String -> a -> a, prints your string when
-- a's evaluation is forced.
import Debug.Trace  

-- for writing files
import System.IO

import Debug.Trace
import System.IO
import Data.List
import Control.Monad
import Data.Maybe
import Data.Tuple
import Data.Ord
--import Data.Arrow


------------------------------------------------------------------------
-- The following is needed for consistency between all our solutions.
------------------------------------------------------------------------

data RGB = RGB Int Int Int deriving (Show, Eq)
type Grid a = [[a]]

type Coord   = (Int,Int)
type Picture = Grid RGB
type Path    = [Coord]

type NodeEnergy = Int
type PathCost   = Int

type Packet = (Coord, RGB, NodeEnergy)  -- used much later
data Node = Node
	        Coord             -- this location
	        RGB               -- color info
	        NodeEnergy        -- energy at this spot
	        PathCost          -- cost of cheapest path from here to bottom.
	        Node              -- ref to next node on cheapest path. Use No's when when we're at the bottom.
	        (Node,Node,Node)  -- Three candidates we may connect to.
	        | No   -- sometimes there is no next node and we use No as a placeholder.
	        deriving (Show, Eq)
------------------------------------------------------------------------



-- ---  Helper Functions to retrieve RGB components ---
getR :: RGB -> Int
getR (RGB r g b) = r 

getG :: RGB -> Int
getG (RGB r g b) = g 

getB :: RGB -> Int
getB (RGB r g b) = b 
------------------------------------------------------


getCoord :: Node -> Coord
getCoord (Node c _ _ _ _ _) = c

getPC :: Node -> Int
getPC No = 0
getPC (Node _ _ _ pc _ _) = pc

getEn :: Node -> Int
getEn No = 0
getEn (Node _ _ e _ _ _) = e

getNext :: Node -> Node
getNext No = No
getNext (Node _ _ _ _ n _) = n


getCoordRow :: Coord -> Int
getCoordRow (r,c) = r

getCoordCol :: Coord -> Int
getCoordCol (r,c) = c


height :: Grid a -> Int
height [[]] = 0
height a = length a

width :: Grid a -> Int
width [[]] = 0
width a = length (head a)



--  Routine to get one element from the grid RGB -------
getElem :: Int -> Int -> [a]-> a
getElem col idx (e:es) = if (idx == col)
					     then e
					     else getElem col (idx+1) es 


getRow :: Int -> Int -> Grid a -> [a]
getRow row idx (r:rs) = if (idx == row)
					    then r
					    else getRow row (idx+1) rs 


getElemCoord ::  Int -> Int -> Grid a -> a
getElemCoord row col grid = (getElem col 0 (getRow row 0 grid))
-----------------------------------------------------------


----- Helper functions for function energyAt ----------------------------------
getLeft :: Grid RGB -> Coord -> RGB
getLeft  g (r,c)  | c == 0    = getElemCoord r ((width g)-1) g
				  | otherwise = getElemCoord r (c - 1) g

getUpper :: Grid RGB -> Coord -> RGB
getUpper g (r,c)  | r == 0    = getElemCoord ((height g)-1) c g
		  	      | otherwise = getElemCoord (r - 1) c g

getRight :: Grid RGB -> Coord-> RGB
getRight  g (r,c) = getElemCoord r (mod (c+1) (width g)) g

getLower :: Grid RGB -> Coord -> RGB
getLower  g (r,c) = getElemCoord (mod (r+1) (height g)) c g

getEnergy :: RGB -> RGB -> Int
getEnergy (RGB r1 g1 b1) (RGB r2 g2 b2) = ((r1-r2)^2) + ((g1-g2)^2) + ((b1-b2)^2)
----------------------------------------------------------------------------------


energyAt :: Grid RGB -> Coord -> NodeEnergy
energyAt g co = ( (getEnergy (getLeft g co) (getRight g co))
				 + (getEnergy (getUpper g co) (getLower g co)) )     



energies :: Grid RGB -> Grid NodeEnergy
energies g = [[ (energyAt g (i,j))
				| j <- [0..((width g)-1)]]
				| i <- [0..((height g)-1)]] 



nextGroups :: [a] -> a -> [(a,a,a)]
nextGroups (x:[]) ev = [(ev, x, ev)]
nextGroups l ev = [ if j==0 
					then  (ev, (l!!j), (l!!(j+1))) 
					else (if j==((length l)-1)
        				  then  ( (l!!(j-1)) , (l!!j) , ev )
						  else  ( (l!!(j-1)) , (l!!j) , (l!!(j+1)) )
						 )
				    | j <- [0..((length l)-1)]]



getCheapest :: (Node,Node,Node) -> Node
getCheapest (l,m,r) | (m==No) = No
					| (l==No) = (if (getPC r)<(getPC m)
								 then r
								 else m)
					| (r==No) = (if (getPC m)<(getPC l)
								 then m
								 else l)
					| otherwise = if ((getPC l)<=(getPC m)) && ((getPC l)<=(getPC r)) 
								  then l
								  else (if (getPC m)<=(getPC r)
								  	    	  then m
								  	    	  else r
								  	   )

buildNodeFromPacket::Packet -> (Node,Node,Node) -> Node
buildNodeFromPacket (c, r, e) t = (Node c r e (e+(getPC (getCheapest t))) (getCheapest t) t)

buildRowFromPackets::[Packet] -> [(Node,Node,Node)] -> [Node]
buildRowFromPackets ps ts = [(buildNodeFromPacket (ps!!i) (ts!!i))
								| i <- [0..((length ps)-1)] ]

makeNosRow:: Int -> [Node]
makeNosRow l = [ No | i <- [0..(l-1)]]

packetsToNodes:: Grid Packet -> Grid Node
packetsToNodes [] = []
packetsToNodes (rowP:rowPs) = if (rowPs == [])
							  then (let nextRow = makeNosRow (length rowP)
							  		in	( (buildRowFromPackets rowP (nextGroups nextRow No)) : [] )
							  	   )
							  else (let restOfNodeGrid = packetsToNodes rowPs
							  	    in ( (buildRowFromPackets rowP (nextGroups (head restOfNodeGrid) No) ) : restOfNodeGrid)
							  	   ) 



rgbsToPackets:: Grid RGB -> Grid Packet
rgbsToPackets rgbs = let engs = energies rgbs
				     in [[ ( (r,c) , ((rgbs!!r)!!c) , ((engs!!r)!!c) ) 
			            	| c <- [0..((width rgbs)-1)]]
							| r <- [0..((height rgbs)-1)]]  
							  	   


findMinPathHead:: [Node] -> Node -> Node
findMinPathHead [] minNode = minNode 
findMinPathHead (n:ns) minNode =  (if (getPC n) < (getPC minNode)
						 	       then findMinPathHead ns n  
							       else findMinPathHead ns minNode
							   	  )		

getNodeFromRow :: [Node] -> Node -> Node
getNodeFromRow [] _ = No
getNodeFromRow _ No = No
getNodeFromRow (n:ns) x = (if (getCoord n) == (getCoord x)
						    then n
						    else getNodeFromRow ns x )

buildPath:: Grid Node -> Node -> Path
buildPath (r:rs) curNode = if (rs == [])
							then (getCoord curNode) : []
							else (getCoord curNode) : buildPath rs (getNodeFromRow (head rs) (getNext curNode)) 

findVerticalPath :: Grid RGB -> Path 
findVerticalPath rgbs = let nodes = packetsToNodes (rgbsToPackets rgbs)
						 in (buildPath nodes (findMinPathHead (head nodes) (head (head nodes))) ) 



flipCoords :: [Coord] -> [Coord]
flipCoords cs = [ ( (getCoordCol (cs!!i)) , (getCoordRow (cs!!i)) ) | i <- [0..((length cs)-1)]]

findHorizontalPath :: Grid RGB -> Path
findHorizontalPath rgbs = let nodes =  (packetsToNodes (rgbsToPackets (transpose rgbs)))
						   in (flipCoords (buildPath nodes (findMinPathHead (head nodes) (head (head nodes))) )  )


	
removeRGB :: [RGB] -> Int -> [RGB]
removeRGB rgbs d = [ if (i+1)>d  
					 then (rgbs!!(i+1))
					 else (rgbs!!i)
					 | i <- [0..((length rgbs)-2)] ] 

removeVerticalPath :: Grid RGB -> Path -> Grid RGB
removeVerticalPath (row:rows) (c:cs) = if (rows == [])
							    	   then (removeRGB row (getCoordCol c)) : []
							    	   else (removeRGB row (getCoordCol c)) : (removeVerticalPath rows cs)


invertCoords :: [Coord] -> [Coord]
invertCoords cs = [ ( (getCoordCol (cs!!i)) , (getCoordRow (cs!!i)) )
					| i <- [0..((length cs)-1)]]

removeHorizontalPath :: Grid RGB -> Path -> Grid RGB
removeHorizontalPath g p = transpose (removeVerticalPath (transpose g) (invertCoords p))



rgbRowToList :: [RGB] -> String
rgbRowToList [] = "" 
rgbRowToList (r:rs) = (show (getR r))++" "++(show (getG r))++" "++(show (getB r))++"\n"++(rgbRowToList rs)

rgbGridToList :: Grid RGB -> String
rgbGridToList [] = ""
rgbGridToList (r:rs) = (rgbRowToList r)++(rgbGridToList rs)

gridToFile :: Grid RGB -> FilePath -> IO ()
gridToFile g filePath = writeFile filePath ("P3\n"++(show (width g))++"\n"++(show (height g))++"\n"++"255\n"++(rgbGridToList g))



buildRGBGrid :: Int -> Int -> [String] -> Grid RGB
buildRGBGrid cols rows ws = [ [ RGB (read (ws!!((r*cols*3)+(c*3)))::Int)  (read (ws!!((r*cols*3)+((c*3)+1)))::Int)	 (read (ws!!((r*cols*3)+((c*3)+2)))::Int) 
								| c <- [0..(cols-1)]]  
								| r <- [0..(rows-1)]]


ppmToGrid :: [String] -> Grid RGB
ppmToGrid (w:ws) = let cols = read (head ws)::Int
					in ( let rows = read (head (tail ws))::Int
						  in buildRGBGrid cols rows (tail (tail (tail ws)))	
						)


fileToGrid :: FilePath -> IO (Grid RGB)
fileToGrid filename = do
  contents <- readFile filename
  let g = ppmToGrid (words contents)
  return g

