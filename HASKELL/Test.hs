
module Query where
import Data.List


type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry] 

type ColSeparator = Char
type LnSeparator = Char

splitBy :: Char -> String -> [String]
splitBy c  = foldr op [[]] 
              where op x (y:ys)
                      | x /= c = (x:y):ys
                      | otherwise = []:(y:ys)

--imi ia tail-ul 
tailBy :: LnSeparator -> String -> [String]
tailBy lin sir = tail (splitBy lin sir)

--imi ia sirul separat dupa linii si mi-l separa dupa coloane
foldrBy :: ColSeparator -> [String] -> [[String]]
foldrBy col [] = []
foldrBy col (c:sir) = (splitBy col c) : (foldrBy col sir) 

-- TODO 1
read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table col lin [] = Table [] [[]]
read_table col lin sir = (Table (splitBy col (head (splitBy lin sir))) (reverse $ tail $ reverse $ foldrBy col (tailBy lin sir)))

-----------------------------------------------------------------------------------
--transpusa matrice
tr :: [[String]] -> [[String]]
tr ([]:_) = []
tr m = (map head m):(tr (map tail m)) 

--the longest word in a string
longestL :: [String] -> Int
longestL xss = length $ snd $ maximum $ [(length xs, xs) | xs <- xss]

--the longest word in each string
longestM :: [[String]] -> [Int]
longestM [] = []
longestM (x:xs) = (longestL x) : (longestM xs)

--am vectorul cu lungimile maxime ale coloanelor
longestTail :: [[String]] -> [Int]
longestTail x = longestM (tr x)
------------------------------------------------------------------------------------

--lista cu lungimi head uri
longestHead :: [String] -> [Int]
longestHead [] = []
longestHead (x:xs) = length x : (longestHead xs) 

--lista cu valori maxime dintre doua liste
longest :: [Int] -> [Int] -> [Int]
longest [] [] = []
longest (x:xs) (y:ys) = if (x<y) then y : (longest xs ys) else x : (longest xs ys) 

--functia finala
functie :: [String] -> [[String]] -> [Int]
functie x y = longest (longestHead x) (longestTail y)

------------------------------------------------------------------------------------
--afisam numarul de spatii care trebuie dupa un cuvant
lengthS :: String -> [String] -> String
lengthS s [] = []
lengthS s (l:ls) = l ++ (lengthS s ls)

--afisam cuvantul, numarul de spatii si |
afisS :: String -> Int -> String
afisS a i = a ++ (lengthS a (replicate (i - length(a)) " ")) ++ "|"  

--afisam randul, [Int] este vectorul cu lungimi maxime
afisR :: [String] -> [Int] ->String
afisR a [] = []
afisR (a:as) (x:xs) = (afisS a x) ++ (afisR as xs)  

--afisam tot
afisM :: [[String]] -> [Int] -> String
afisM [] l = []
afisM (x:xs) l = "\n|" ++ (afisR x l) ++ (afisM xs l)

--afisam [[String]]
afisT :: [[String]] -> [Int] -> IO ()
afisT x y = putStr((afisM x y))

show_p :: Table -> String
show_p (Table t1 t2) =  (afisM t2 $ (longest (longestHead t1) (longestTail t2)) ) 

instance Show Table where
	show  = show_p 

 						
-- read :: ColSeparator -> LnSeparator -> String -> Table
-- read col lin [] = Table [] [[]]
-- read_table col lin sir = Table (splitBy col) (head (splitBy lin)) [] 

--elimina element cu indexul i
dropK k l = take k l ++ drop (k+1) l

--primeste ca parametru un [String] si imi afla un vector cu indicii care nu sunt
functieL :: [String] -> [String] -> [Int]
functieL colors select = map (\(Just x) -> x) $ map (`elemIndex` colors) $ filter (`notElem` select) colors

functieS :: [[String]] -> [[String]]

