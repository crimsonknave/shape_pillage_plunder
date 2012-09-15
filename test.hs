setup :: [(Int, Int, String, [Char])]
setup = [(0,0,"Start", "NESW"), (0,1, "Straight Hallway", "NS"), (1,0,"Curved Hallway","WS"),((-1),0,"Slide","EW"),(0,(-1),"Threeway Hallway", "NSW")]

direction :: Char -> String
direction 'N' = "North"
direction 'S' = "South"
direction 'E' = "East"
direction 'W' = "West"
direction x = "Unknown"

xCoord :: (Int, Int, String, [Char]) -> Int
xCoord (i, _, _, _) = i

yCoord :: (Int, Int, String, [Char])        -> Int
yCoord (_, i, _, _) = i

findName :: (Int, Int, String, [Char]) -> String
findName (_, _, i, _) = i

exits :: (Int, Int, String, [Char]) -> [Char]
exits (_, _, _, i) = i

directions :: String -> String
directions (x:y:[]) = direction x ++ " and " ++ direction y
directions (x:y:xs) = direction x ++ ", " ++ directions (y:xs)

directionMap :: Char -> (Int, Int)
directionMap 'N' = (0, 1)
directionMap 'E' = (1, 0)
directionMap 'S' = (0, (-1))
directionMap 'W' = ((-1), 0)
directionMap x = (0, 0)

findRoom :: Int -> Int -> (Int, Int, String, [Char])
findRoom x y = if null rooms then (x, y, "Cave In !", "") else head rooms
    where
    rooms = [ room | room <- setup, xCoord room == x, yCoord room == y]

findNeighbor :: (Int, Int, Char) -> (Int, Int, String, [Char])
--findNeighbor (x,y,dir) = head [ room | room <- setup, xCoord room == x + (fst (directionMap dir)), yCoord room == y + (snd (directionMap dir))]
findNeighbor (x,y,dir) = findRoom (x + (fst (directionMap dir))) (y + (snd (directionMap dir)))



describeRoom :: (Int, Int, String, [Char]) -> IO ()
describeRoom (x,y,name, exits) = do
                                 putStrLn (name ++ " has exits to the " ++ directions exits)
                                 putStrLn ("It is located at (" ++ show x ++ "," ++ show y ++ ")")
                                 putStrLn (unlines [ "It's neighbor to the " ++ direction i ++ " is a " ++ findName (findNeighbor (x,y,i)) | i <- exits])
