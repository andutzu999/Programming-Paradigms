module Query where

import Data.List
import UserInfo
import Rating
import Movie

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
user_info = read_table '|' '\n' user_info_str
rating = read_table ' ' '\n' rating_str
movie = read_table '|' '\n' movie_str

--show--
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

--afisam [[String]]
afisM :: [[String]] -> [Int] -> String
afisM [] l = []
afisM (x:xs) l = "\n|" ++ (afisR x l) ++ (afisM xs l)

--afisam linia cu "----"
afisLin :: [Int] -> String
afisLin [] = "-"
afisLin (x:xs) = (lengthS [] (replicate x "-")) ++ "-" ++ (afisLin xs)

listAntet :: [String] -> [[String]] -> String
listAntet t1 t2 = (afisLin (longest (longestHead t1) (longestTail t2)))

listPrima :: [String] -> [[String]] -> String
listPrima t1 t2 = (afisR t1 (longest (longestHead t1) (longestTail t2)))

show_p :: Table -> String
show_p (Table t1 t2) = (listAntet t1 t2) ++ "\n" ++ "|" ++ (listPrima t1 t2) ++ "\n" ++
 (listAntet t1 t2) ++ (afisM t2 $ (longest (longestHead t1) (longestTail t2))) ++
 "\n" ++ (listAntet t1 t2) ++ "\n" 

instance Show Table where
	show  = show_p 

--select & select_limit
-------------------------------------------------------------------------------------------

--primeste ca parametru un [String] si imi afla un vector cu indicii care sunt
functieL :: [String] -> [String] -> [Int]
functieL s2 [] = []
functieL s2 (s:s1) = (elemIndices s s2) ++ (functieL s2 s1)

--pastrez coloanele
functieS :: [[String]] -> [Int] -> [[String]]
functieS sir [] = [] 
functieS sir (x:xs) =  (sir !! x) : (functieS sir xs)


functieF :: [String] -> Table -> Table
functieF s1 (Table s2 sir) = ( Table s1 (tr (functieS (tr sir) (functieL s2 s1) ) ) )  


select_limit :: [String] -> Integer -> Table -> Table
select_limit s1 i (Table s2 sir) = (Table s1 (take (fromIntegral i) (tr (functieS (tr sir) (functieL s2 s1)))))

------------------------------------------------------------------------------------------------------------------

--filter Lt--
------------------------------------------------------------------------------------------------------------------
--header, cuvant, obtin numarul coloanei
filterL :: [String] -> String -> Int
filterL s1 s2 = head (elemIndices s2 s1) 

--tabelul , numarul coloanei , header, cuvant , string obtinut
filterS :: [[String]] -> [String] -> String -> Int -> [[String]]
filterS [] s1 s2 nr = []
filterS (x:xs) s1 s2 nr = if (read (x !! (filterL s1 s2)) :: Int) < nr then x: (filterS xs s1 s2 nr) else filterS xs s1 s2 nr

--functia finala
filter_lt :: String -> Integer -> Table -> Table
filter_lt s1 i (Table s2 sir) = (Table s2 (filterS sir s2 s1 (fromIntegral i)))

--filter Eq
-------------------------------------------------------------------------------------------------------------------
filterEq :: [[String]] -> [String] -> String -> String -> [[String]]
filterEq [] s1 s2 str = []
filterEq (x:xs) s1 s2 str = if (x !! (filterL s1 s2)) == str then x :(filterEq xs s1 s2 str) else filterEq xs s1 s2 str

filter_eq :: String -> String -> Table -> Table
filter_eq s1 str (Table s2 sir) = (Table s2 (filterEq sir s2 s1 str)) 

--filter In--
--------------------------------------------------------------------------------------------------------------------
--verific daca cuvantul meu este in [String]
checkIfElem :: String -> [String] -> Bool
checkIfElem x db =
    if x `elem` db then True else False

filterIn :: [[String]] -> [String] -> String -> [String] -> [[String]]
filterIn [] s1 s2 str = []
filterIn (x:xs) s1 s2 str = if checkIfElem (x !! (filterL s1 s2)) str == True then x :(filterIn xs s1 s2 str) else filterIn xs s1 s2 str

filter_in :: String -> [String] -> Table -> Table
filter_in s1 str (Table s2 sir) = (Table s2 (filterIn sir s2 s1 str)) 

data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

--filter Not Lt
----------------------------------------------------------------------------------------------------------------------

filter_s :: [[String]] -> [String] -> String -> Int -> [[String]]
filter_s [] s1 s2 nr = []
filter_s (x:xs) s1 s2 nr = if (read (x !! (filterL s1 s2)) :: Int) >= nr then x: (filter_s xs s1 s2 nr) else filter_s xs s1 s2 nr

--functia finala
filter_notLT :: String -> Integer -> Table -> Table
filter_notLT s1 i (Table s2 sir) = (Table s2 (filter_s sir s2 s1 (fromIntegral i)))

--filter Not IN
------------------------------------------------------------------------------------------------------------------------
filter_i :: [[String]] -> [String] -> String -> [String] -> [[String]]
filter_i [] s1 s2 str = []
filter_i (x:xs) s1 s2 str = if checkIfElem (x !! (filterL s1 s2)) str == False then x :(filter_i xs s1 s2 str) else filter_i xs s1 s2 str

filter_notIN :: String -> [String] -> Table -> Table
filter_notIN s1 str (Table s2 sir) = (Table s2 (filter_i sir s2 s1 str)) 

--filter Not EQ
---------------------------------------------------------------------------------------------------------------------------
filter_e :: [[String]] -> [String] -> String -> String -> [[String]]
filter_e [] s1 s2 str = []
filter_e (x:xs) s1 s2 str = if (x !! (filterL s1 s2)) /= str then x :(filter_e xs s1 s2 str) else filter_e xs s1 s2 str

filter_notEQ :: String -> String -> Table -> Table
filter_notEQ s1 str (Table s2 sir) = (Table s2 (filter_e sir s2 s1 str)) 

-- TODO 3
getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter = undefined

-- TODO 4
data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table


eval :: Query -> Table
eval (Atom table) = table
eval (Select list q) = functieF list $ eval q
eval (SelectLimit list i q) = select_limit list i $ eval q
eval (Filter (Lt sir i) q) = filter_lt sir i $ eval q
eval (Filter (Eq s1 s2) q) = filter_eq s1 s2 $ eval q
eval (Filter (In s1 s2) q) = filter_in s1 s2 $ eval q
eval (Filter (Not(Lt sir i)) q) = filter_notLT sir i $ eval q
eval (Filter (Not(Eq s1 s2)) q) = filter_notEQ s1 s2 $ eval q
eval (Filter (Not(In s1 s2)) q) = filter_notIN s1 s2 $ eval q

-- TODO 5
same_zone :: String -> Query
same_zone = undefined

male_within_age :: Integer -> Integer -> Query
male_within_age = undefined

mixed :: [String] -> [String] -> Int -> Query
mixed = undefined