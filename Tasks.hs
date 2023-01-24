{-
    PP Project 2021

    This is where you will write the implementation for the given tasks.
    You can add other modules aswell.
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Tasks where

import Data.List
import Dataset
import Text.Printf
import Data.Maybe

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
    TASK SET 1
-}

-- Task 1

myHead::[t] -> t
myHead (x:_) = x

--concats stundent's name to his/her final grade
build_row_ex1 :: Row -> Row
build_row_ex1 list = myHead(list) : printf "%.2f" ( final_grade list):[]  

--computes all the questions from the oral exam
grades_aux ::Row -> Float
grades_aux list = ( (foldr (+) 0 $ map convert $ tail (init list)) / 4 )

--aux function for converting String to Float
--fills every "" with 0.0
convert::String -> Float
convert a 
    | a =="" = 0.0 
    | otherwise = read a::Float 

--sum oral exam with written exam
final_grade :: Row -> Float
final_grade list = (read $ last list) + (grades_aux list)


--maps grades for Tabel of studens
compute_exam_grades :: Table -> Table
compute_exam_grades list = ["Nume","Punctaj Exam"] : ((map build_row_ex1) . (tail) ) list 

-- Task 2

helper :: Table -> [Float]
helper list = ( (map final_grade) . (tail) ) list

-- Number of students who have passed the exam:
get_passed_students_num :: Table -> Int
get_passed_students_num list =  length $ filter(>2.5) $ helper list


passed_students_F :: Table -> Float
passed_students_F list = fromIntegral (get_passed_students_num list)

--get total grades
total :: Table -> Float
total list = fromIntegral ( length $ helper list )

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage list = ( ( passed_students_F  list) / (total list / 100) ) / 100 

-- Average exam grade
get_exam_avg :: Table -> Float
get_exam_avg list = (foldr (+) 0 $ helper list) / (total list) 

-- Number of students who gained at least 1.5p from homework:
hw_points :: Row -> Row
hw_points (a:b:c:d:e:_) = [c,d,e] 

--final hw grade
hwk_grades :: Row -> Float
hwk_grades list = foldr (+) 0  $ map convert (hw_points list) 

get_passed_hw_num :: Table -> Int
get_passed_hw_num list = length $ filter (>=1.50) $ ( (map hwk_grades) . (tail) ) list

-- Task 3
--conver question points to float
auxQ1 :: Row -> Float
auxQ1(a:b:_) = convert b

--return list of question points
qPoints1:: Table -> [Float]
qPoints1 list = ( (map auxQ1) . (tail) ) list 

--computes average points for question
avgQ1 :: Table -> Float
avgQ1 list = (foldr (+) 0 $ qPoints1 list) / (fromIntegral $ length $ qPoints1 list)

--repeat the proces for questions Q2, Q3, Q4, Q5, Q6

auxQ2 :: Row -> Float
auxQ2(a:b:c:_) = convert c

qPoints2:: Table -> [Float]
qPoints2 list = ( (map auxQ2) . (tail) ) list

avgQ2 :: Table -> Float
avgQ2 list = (foldr (+) 0 $ qPoints2 list) / (fromIntegral $ length $ qPoints2 list)


auxQ3 :: Row -> Float
auxQ3(a:b:c:d:_) = convert d

qPoints3:: Table -> [Float]
qPoints3 list = ( (map auxQ3) . (tail) ) list

avgQ3 :: Table -> Float
avgQ3 list = (foldr (+) 0 $ qPoints3 list) / (fromIntegral $ length $ qPoints3 list)

auxQ4 :: Row -> Float
auxQ4(a:b:c:d:e:_) = convert e

qPoints4:: Table -> [Float]
qPoints4 list = ( (map auxQ4) . (tail) ) list

avgQ4 :: Table -> Float
avgQ4 list = (foldr (+) 0 $ qPoints4 list) / (fromIntegral $ length $ qPoints4 list)

auxQ5 :: Row -> Float
auxQ5(a:b:c:d:e:f:_) = convert f

qPoints5:: Table -> [Float]
qPoints5 list = ( (map auxQ5) . (tail) ) list

avgQ5 :: Table -> Float
avgQ5 list = (foldr (+) 0 $ qPoints5 list) / (fromIntegral $ length $ qPoints5 list)

auxQ6 :: Row -> Float
auxQ6(a:b:c:d:e:f:g:_) = convert g

qPoints6:: Table -> [Float]
qPoints6 list = ( (map auxQ6) . (tail) ) list

avgQ6 :: Table -> Float
avgQ6 list = (foldr (+) 0 $ qPoints6 list) / (fromIntegral $ length $ qPoints6 list)

--return list with every average question
qAvgPoints :: Table -> [Float]
qAvgPoints list = avgQ1 list : avgQ2 list : avgQ3 list : 
                avgQ4 list : avgQ5 list : avgQ6 list:[]

--convert list to String
toString :: [Float] -> [String]
toString list = map (printf "%.2f") list

--computes every average question
get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs list = ["Q1","Q2","Q3","Q4","Q5","Q6"] : toString (qAvgPoints list) : []

-- Task 4
--uses function qPoints to computes all question points
--filters out questions equals with 0 then equals with 1 then equals with 2
--reterns the number for each filtred list
--concats the results
q1Grades :: Table -> Row
q1Grades list = "Q1" : (map show $ (length $  filter (==0) $ qPoints1 list) :
                     (length $  filter (==1) $ qPoints1 list) :
                     (length $  filter (==2) $ qPoints1 list) :[]) 

--repeat the procces with every question

q2Grades :: Table -> Row
q2Grades list = "Q2" : (map show $ (length $  filter (==0) $ qPoints2 list) :
                     (length $  filter (==1) $ qPoints2 list) :
                     (length $  filter (==2) $ qPoints2 list) :[]) 

q3Grades :: Table -> Row
q3Grades list = "Q3" : (map show $ (length $  filter (==0) $ qPoints3 list) :
                     (length $  filter (==1) $ qPoints3 list) :
                     (length $  filter (==2) $ qPoints3 list) :[]) 

q4Grades :: Table -> Row
q4Grades list = "Q4" : (map show $ (length $  filter (==0) $ qPoints4 list) :
                     (length $  filter (==1) $ qPoints4 list) :
                     (length $  filter (==2) $ qPoints4 list) :[])

q5Grades :: Table -> Row
q5Grades list = "Q5" : (map show $ (length $  filter (==0) $ qPoints5 list) :
                     (length $  filter (==1) $ qPoints5 list) :
                     (length $  filter (==2) $ qPoints5 list) :[])  

q6Grades :: Table -> Row
q6Grades list = "Q6" : (map show $ (length $  filter (==0) $ qPoints6 list) :
                     (length $  filter (==1) $ qPoints6 list) :
                     (length $  filter (==2) $ qPoints6 list) :[]) 

--concats the result for every qGrades function
get_exam_summary :: Table -> Table
get_exam_summary list = ["Q","0","1","2"] : q1Grades list : q2Grades list : q3Grades list :
                    q4Grades list : q5Grades list : q6Grades list :[]

-- Task 5
-- compares students by grades
-- if they have grades equals compares by names
compareElements :: [String] -> [String] -> Ordering
compareElements list1@(x:y:_) list2@(a:b:_) 
    | compare (read y::Float) (read b::Float) == EQ = compare x a 
    | otherwise = compare (read y::Float) (read b::Float)

--sorting stundest using compareElements 
sortStudents :: Table -> Table
sortStudents (xs) = sortBy compareElements xs 

get_ranking :: Table -> Table
get_ranking list = ["Nume","Punctaj Exam"] : sortStudents ( tail (compute_exam_grades list) ) 

-- Task 6
toFloat :: [String] -> Float
toFloat list = read (last list)::Float

-- if the oral exam = 0 returns written exam
-- if written exam = 0 returns oral exam
-- otherwise return grades difference
difference :: Row -> Value
difference list
    | grades_aux list == 0 =  printf "%.2f" ( read (last list)::Float)
    | convert (last list) == 0 = printf "%.2f" (grades_aux list)
    | otherwise =  printf "%.2f" ( grades_aux list - convert (last list) )

--return list of student's name, oral grade, written grade, exams difference
build_row_ex6 :: Row -> Row
build_row_ex6 list = myHead(list) : printf "%.2f" (grades_aux list) 
                    : printf "%.2f" (toFloat list) : difference list:[]

--computes input table to task table format
build_table_ex6 :: Table -> Table
build_table_ex6 list = map build_row_ex6 (tail list)

-- compares students by exams difference
-- if they exams difference equals compares by names
compare_difference:: [String] -> [String] -> Ordering
compare_difference list1@(x:xs) list2@(y:ys) 
    | compare (read (last list1)::Float) (read (last list2)::Float) == EQ = compare x y 
    | otherwise = compare (read (last list1)::Float) (read (last list2)::Float)

--sorting stundest using compare_difference
sortByDifference :: Table -> Table
sortByDifference (xs) = sortBy compare_difference xs 

get_exam_diff_table :: Table -> Table
get_exam_diff_table list = ["Nume","Punctaj interviu","Punctaj scris","Diferenta"] : 
            sortByDifference (build_table_ex6 list)



{-
    TASK SET 2
-}

--Converts String to Table
read_csv :: CSV -> Table
read_csv list = map (charSep ',') $ charSep '\n' list

--splitOn implementation from Data.List.Split
--hw_checker doesn't have library Data.List.Split
charSep :: Char -> String -> [String]
charSep sep = foldr op []
            where op x []
                    | x == sep = [] : [""]
                    | otherwise = [[x]]
                  op x (y:ys)
                    | x == sep = "":(y:ys)
                    | otherwise = (x:y):ys

--Converts Table to String
write_csv :: Table -> CSV
write_csv list =  intercalate "\n" (map (intercalate ",") list )


--Task 1

--get the index of given element
get_index :: String -> [String] -> Int
get_index name list = fromJust $ elemIndex name list

--matrix transpose
trans:: [[a]]->[[a]]
trans ([]:_) = []
trans x = (map head x) : trans (map tail x)

-- gets index of given String
-- transpose the table to conver columns to lists
-- take function returns table with rows from given index below
-- drop function removes all the rows from given index below
-- the final result is a table with only one list
-- myHead retunrns only the list
as_list :: String -> Table -> [String]
as_list name list = myHead $ drop h  $ take ( h + 1) $ trans $ tail list 
                    where
                        h = get_index name (myHead list)

--Task 2
compareEleme :: String -> Table -> [String] -> [String] -> Ordering
compareEleme nume t l1 l2 
 | compare (l1 !! ( get_index nume (myHead t) ) ) (l2 !! ( get_index nume (myHead t) ) ) == EQ = compare (l1 !! 0) (l2 !! 0)
 | otherwise = compare (l1 !! ( get_index nume (myHead t) ) ) (l2 !! ( get_index nume (myHead t) ) )
                                 
auxy :: String -> Table -> Table
auxy nume xs =   sortBy (compareEleme nume xs ) xs  

tsort :: String -> Table -> Table
tsort nume t =  head t : init (auxy nume t)  
--Task 3

--map implementation
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> (f x) : acc) []

vmap :: (Value -> Value) -> Table -> Table
vmap list = myMap (myMap list)

--Task 4

rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f name tabel = name : (map f (tail tabel)) 

--takes a hw_grades Table Row
--removes Name and Lab points
--calculates grades points
get_grades :: Row -> Float
get_grades list = foldr (+) 0 $ map convert $ (tail (tail list)) 

-- concats student's name with his/her total grade
get_hw_grade_total :: Row -> Row
get_hw_grade_total list = (head list) : printf "%.2f" (get_grades list) : []

--Task 5
vunion :: Table -> Table -> Table
vunion t1 t2 
            | myHead t1 == myHead t2 = t1 ++ (tail t2)
            | otherwise = t1

--Task6

checkLenghRow :: Row -> Row -> Bool
checkLenghRow r1 r2
                | length r1 == length r2 = True
                | otherwise = False 


insSpace :: Row -> Row
insSpace list = list ++ [""]

--until acts kinda like a do while 

emptyRow:: Row -> Row -> Row
emptyRow l1 l2 = until (checkLenghRow l2) insSpace l1 

--builds an empty Row 
-- empty Row length = Table Row length  
insRow :: Table -> Table
insRow list = list ++ [emptyRow [] (myHead list)]

--check if two Tables have the same number of Rows
checkLengh :: Table -> Table -> Bool
checkLengh t1 t2
                | length t1 == length t2 = True
                | otherwise = False 

-- adds emppty rows to the smaler table
expandTable :: Table -> Table -> Table 
expandTable t1 t2 = until (checkLengh t2) insRow t1

-- if tables are equal transpose both, concats the result, transpose the result
-- if one table is smaller fill it with empty rows then repeat the steps from 
-- previous coment
hunion :: Table -> Table -> Table
hunion t1 t2 
            | length t1 < length t2 = trans $ (trans (expandTable t1 t2)) ++ (trans t2)
            | length t1 == length t2 = trans $ (trans t1) ++ (trans t2)
            | length t1 > length t2 = trans $ (trans t1 ) ++ (trans (expandTable t2 t1))

--Task 7

compareHeader :: String -> Table -> Table -> Ordering
compareHeader nume t1  t2 = compare ( (head t1) !! h ) ( (head t2) !! h )
                            where h = get_index nume (head t1)

keySet :: String -> Table -> Row
keySet nume t = tail $ (trans t) !! h
                where h = get_index nume ( head t) 

get_key_index :: String -> String -> Table -> Table -> (Int, Int)
get_key_index key keyValue t1 t2  
    | elemIndex keyValue (keySet key t2) == Nothing = (get_index  keyValue ( keySet key t1 ), -1)
    | elemIndex keyValue (keySet key t1) == Nothing = ( -1, get_index  keyValue ( keySet key t2 ) )
    | otherwise = (get_index  keyValue ( keySet key t1 ), get_index  keyValue ( keySet key t2 ))

index_list :: String -> [String] -> Table -> Table -> [(Int,Int)]
index_list key [] t1 t2 = []
index_list key (x:xs) t1 t2 = get_key_index key x t1 t2 : index_list key xs t1 t2 



mergeRows :: String -> [(Int,Int)] -> Table -> Table -> Table
mergeRows key [] t1 t2 = []
mergeRows key list@(x:xs) t1 t2 
    | (fst x) < 0 =  ( tail (removeRow key t2) !! (snd x) )  : mergeRows key xs t1 t2 
    | (snd x) < 0 =  ( (tail t1) !!  (fst x)) : mergeRows key xs t1 t2 
    | otherwise = ( ((tail t1) !!  (fst x)) ++ (  tail (removeRow key t2) !! (snd x) ) ) : mergeRows key xs t1 t2 

buildHeader :: String -> Table -> Table -> Row
buildHeader name t1 t2 = (head t1) ++ (head (removeRow name t2))
                                 

removeRow :: String -> Table -> Table
removeRow nume t = trans $ take h ( trans t) ++ drop (h+1) (trans t)
                    where h = get_index nume (head t) 


aux_join :: String -> Table -> Table -> Table
aux_join nume t1 t2 
    | compareHeader nume t1 t2 == EQ = buildHeader nume t1 t2 : mergeRows nume (index_list nume (keySet nume t1) t1 t2) t1 t2 
    | otherwise = []


additonal_space:: Row -> Row -> Row
additonal_space l1 l2 = until (checkLenghRow l1) insSpace l2 

tjoin :: String -> Table -> Table -> Table
tjoin key t1 t2 = map (additonal_space (head (aux_join key t1 t2) ) ) (aux_join key t1 t2)

--Task 8


cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian f name t1 t2 = name : [ f x y | x <- (tail t1), y <- (tail t2)]

--Task 9
--return recursively transpose table for task 9
aux_p :: [String] -> Table -> Table
aux_p [] t1 = []
aux_p list@(x:xs) t1 =  as_list x t1 : aux_p xs t1 

--tranpose input table and concates collumn names
projection :: [String] -> Table -> Table
projection [] t1 = []
projection list t1 =  list : trans (aux_p list t1)


{-
    TASK SET 3
-}





data QResult = CSV CSV | Table Table | List [String]

instance Show QResult where
    show (CSV a) = show a
    show (Table t) = write_csv t 
    show (List b) = show b 

data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    |Graph EdgeOp Query


-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value
 



class Eval a where
    eval :: a -> QResult
 
instance Eval Query where
    eval (FromCSV str) = Table $ read_csv str
    eval (ToCSV query) = CSV $ write_csv (extract $ eval query ) 
    eval (AsList colname query) = List  $ as_list colname (extract $ eval query) 
    eval (Sort colname query) = Table $ tsort colname (extract $ eval query) 
    eval (ValueMap op query) = Table $ vmap op (extract $ eval query) 
    eval (RowMap op colnames query) = Table $ rmap op colnames (extract $ eval query)  
    eval (VUnion query1 query2) = Table $ vunion (extract $ eval query1)  (extract $ eval query2)
    eval (HUnion query1 query2) = Table $ hunion (extract $ eval query1)  (extract $ eval query2)
    eval (TableJoin colname query1 query2) = Table $ tjoin colname (extract $ eval query1)  (extract $ eval query2)
    eval (Cartesian op colnames query1 query2) = Table $ cartesian op colnames (extract $ eval query1)  (extract $ eval query2)
    eval (Projection colnames query) = Table $ projection colnames (extract $ eval query)  
    eval (Filter conditon query) = Table $ (head $ extract $ eval query) : (transpose $ projection (function (head $ extract $ eval query) (tail $ extract $ eval query) conditon) (transpose $ extract $ eval query))

   
      
    
extract :: QResult -> Table
extract (Table a) = a 


data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp
    function :: [String] -> Table -> (FilterCondition a) -> [String]

type FilterOp = Row -> Bool


isMemberF ::Eq t => t -> [t] -> Bool
isMemberF n [] = False
isMemberF n (x:xs)
    | n == x = True
    | otherwise = isMemberF n xs

instance FEval Float where
    feval str (Eq colname ref) =  \x->( read  ( x !! (get_index colname str) ) :: Float ) == ref  
    feval str (Gt colname ref) =  \x->( read  ( x !! (get_index colname str) ) :: Float ) > ref  
    feval str (Lt colname ref) =  \x->( read  ( x !! (get_index colname str) ) :: Float ) < ref  
    feval str (In colname list) = \x-> isMemberF ( convert ( x !! (get_index colname str) ) ) list
    feval str (FNot cond) = \x-> not (feval str cond x)  
    feval str (FieldEq colname1 colname2) = \x->( read  ( x !! (get_index colname1 str) ) :: Float ) == ( read  ( x !! (get_index colname2 str) ) :: Float )  
    function str [x] cond = if feval str cond x == True then ( (x !! 0) : [] ) else []
    function str t@(x:xs) cond = if feval str cond x == True  then ( (x !! 0) : (function str xs cond) )  else function str xs cond

instance FEval String where
    feval str (Eq colname ref) =  \x-> ( x !! (get_index colname str) )  == ref  
    feval str (Gt colname ref) =  \x-> ( x !! (get_index colname str) )  > ref  
    feval str (Lt colname ref) =  \x-> ( x !! (get_index colname str) ) < ref  
    feval str (In colname list) = \x-> isMemberF ( x !! (get_index colname str) ) list
    feval str (FNot cond) = \x-> not (feval str cond x) 
    feval str (FieldEq colname1 colname2) = \x->( x !! (get_index colname1 str)  ) == ( x !! (get_index colname2 str) )
    function str [x] cond = if feval str cond x == True then ( (x !! 0) : [] ) else []
    function str t@(x:xs) cond = if feval str cond x == True  then ( (x !! 0) : (function str xs cond) )  else function str xs cond 




similarities_query :: Query
similarities_query = undefined 