module Mushroom where
import Data.Ratio ((%))
import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

--
-- --                                      Pre-defined functions.
-- 
-- These functions are used to convert between counts, rationals, and string percentages. The counts
-- are similar to those used in the summaries for Project 1. You may not need these functions at all,
-- depending on your implementation choices.

ratioOfCount :: (Int, Int) -> Rational
ratioOfCount (a,b) = (fromIntegral a) % (fromIntegral $ a+b)

percentOfRatio :: Rational -> String
percentOfRatio r = (show $ truncate $ 100 * r) ++ "%"

percentOfCount :: (Int, Int) -> String
percentOfCount c = percentOfRatio $ ratioOfCount c 

outOf :: Int -> Int -> Rational
outOf a b =  (fromIntegral a) % (fromIntegral b)

-- All undefined values and functions should be completed. Your code will compile 
-- even if some functions are left undefined.

--
-- --                                       Milestone
--

-- Mushroom edibility is either Nom or NoNom. We should only eat mushrooms we are pretty certain are
-- Noms. Please do not actually eat any mushrooms based on advice from this project.
data Edible = Nom | NoNom deriving (Show, Eq, Ord)

--Define an algebraic data type for all the different possible attributes.
--You may decompose this by having sub-types (like Operators and Tokens for the class project),
--but every possible attribute should be representable as an Attribute.
--You may not use Strings or Integers in this type.  You should derive Eq and Show (at least to
--start). You may also derive Ord, but do not rely on the ordering.

data Color = Brown | Green | Purple | White | Yellow deriving (Eq, Show, Ord)
data CShape = Bell | Conical | Knobbed deriving (Eq, Show, Ord)
data SShape = Bulbous | Club | Missing deriving (Eq, Show, Ord)
data Size = Broad | Narrow deriving (Eq, Show, Ord)
data Smell = Almond | Anise | Foul | None | Musty deriving (Eq, Show, Ord)
data Attribute = StalkColorAtt Color | SapColorAtt Color | SporeColorAtt Color | CapShapeAtt CShape | StalkShapeAtt SShape | GillSizeAtt Size | OdorAtt Smell deriving (Eq, Show, Ord)

-- Make a list of all possible attributes. There should be 28 different attributes.
allAttributes :: [Attribute]
allAttributes = [StalkColorAtt Brown, StalkColorAtt Green, StalkColorAtt Purple, StalkColorAtt White, StalkColorAtt Yellow, SapColorAtt Brown, SapColorAtt Green, SapColorAtt Purple, SapColorAtt White, SapColorAtt Yellow, SporeColorAtt Brown, SporeColorAtt Green, SporeColorAtt Purple, SporeColorAtt White, SporeColorAtt Yellow, CapShapeAtt Bell, CapShapeAtt Conical, CapShapeAtt Knobbed, StalkShapeAtt Bulbous, StalkShapeAtt Club, StalkShapeAtt Missing, GillSizeAtt Broad, GillSizeAtt Narrow, OdorAtt Almond, OdorAtt Anise, OdorAtt Foul, OdorAtt None, OdorAtt Musty]
--A mushroom is a list of attributes.
type Mushroom = [Attribute]

--An observation is a mushroom that is known to be nomable, or not nomable.  Thus, it is a tuple of
--a mushroom and an edibility.
type Observation = (Mushroom, Edible)

-- readObservation takes a single line of the input file, in the format described on the
-- project page, and return the corresponding observation.  You may find the splitOn function
-- helpful. splitOn takes a string delimiter, a string to be split, and splits the string based on
-- the delimiter.  For instance (words str) is the same as (splitOn " " str)
-- I suggest you make helper functions for reading different columns.
-- The sequence function may be helpful.
editbility :: String -> Edible  
editbility "edible" = Nom 
editbility "poison" = NoNom 
editbility str = error $ "Not included edibility " ++ str

stalkColor :: String -> Attribute 
stalkColor "brown" = StalkColorAtt Brown
stalkColor "green" = StalkColorAtt Green
stalkColor "purple" = StalkColorAtt Purple
stalkColor "white" = StalkColorAtt White
stalkColor "yellow" = StalkColorAtt Yellow
stalkColor str = error $ "Not included stalkColor" ++ str

sapColor :: String -> Attribute 
sapColor "brown" = SapColorAtt Brown
sapColor "green" = SapColorAtt Green
sapColor "purple" = SapColorAtt Purple
sapColor "white" = SapColorAtt White
sapColor "yellow" = SapColorAtt Yellow
sapColor str = error $ "Not included sapColor" ++ str

sporeColor :: String -> Attribute 
sporeColor "brown" = SporeColorAtt Brown
sporeColor "green" = SporeColorAtt Green
sporeColor "purple" = SporeColorAtt Purple
sporeColor "white" = SporeColorAtt White
sporeColor "yellow" = SporeColorAtt Yellow
sporeColor str = error $ "Not included sporeColor" ++ str

capShape :: String -> Attribute 
capShape "bell" = CapShapeAtt Bell
capShape "conical" = CapShapeAtt Conical
capShape "knobbed" = CapShapeAtt Knobbed
capShape str = error $ "Not included capShape" ++ str

stalkShape :: String -> Attribute 
stalkShape "bulbous" = StalkShapeAtt Bulbous 
stalkShape "club" = StalkShapeAtt Club
stalkShape "missing" = StalkShapeAtt Missing  
stalkShape str = error $ "Not included stalkShape" ++ str

gillSize :: String -> Attribute 
gillSize "broad" = GillSizeAtt Broad
gillSize "narrow" = GillSizeAtt Narrow
gillSize str = error $ "Not included gillSize" ++ str

odorSmell :: String -> Attribute 
odorSmell "almond" = OdorAtt Almond  
odorSmell "anise" = OdorAtt Anise
odorSmell "foul" = OdorAtt Foul
odorSmell "none" = OdorAtt None
odorSmell "musty" = OdorAtt Musty 
odorSmell str = error $ "Not included odorSmell" ++ str 

--stringSplitter :: String -> [String]
--stringSplitter lst = splitOn lst 

readObservation :: String -> Maybe Observation
readObservation input = Just ([y z | (y,z) <- x], editbility s )
    where (s:splitter) = splitOn "," input 
          (x) = zip [stalkColor, sapColor, sporeColor, capShape, stalkShape, gillSize, odorSmell] splitter

-- readObservationFile takes the entire contents of an entire file and return the list of
-- observations. Note the first line has header information and is not a valid observation. 
-- The lines function may be helpful. 
-- The sequence function may be helpful.
readObservationFile :: String ->  Maybe [Observation]
readObservationFile file = sequence $ [readObservation x | x <- tail lin]
    where lin = lines file 

--                                     Core Project

--numCorrect computes how much information about edibility can be gained by checking a specific
--attribute. It takes a single attribute and a list of observations, and answers the question: 
--"If all we had to go on was this attribute, how many mushrooms would we label correctly?"
--1) Split the observations into the those with the attribute, and those without the attribute. 
--2) One of these sets will have a higher percentage of edible mushrooms. Call that set A, and the
--   other set B. Note that A may be the set with the attribute, or the set without the attribute.
--3) If mushrooms in set A are assumed to be edible, and those in the other set B are assumed to be
--   inedible, return the number of correct guesses.
--4) Important: if either set is empty, no information is gained by looking at the attribute. Return 0.
--
--You may find the built-in partition function useful.
--
-- This was done in the in-class activity! Refer to that for assistance.

hasAttribute :: Observation -> Attribute -> Bool --Checks if the observation has the wanted attribute
hasAttribute (attribute, editbility) attr = 
    if attr `elem` attribute then True else False 

attributeSplit :: Attribute -> [Observation] -> ([Observation], [Observation]) --Partition into a list with the attribute and a list without the attribute
attributeSplit attr lst = partition (`hasAttribute` attr) lst 

edibleCounter :: [Observation] -> Int --Checks how many mushrooms are edible
edibleCounter [ ] = 0
edibleCounter ((attributes, editbility):observations) = 
    if editbility == Nom
    then 1 + edibleCounter observations
    else edibleCounter observations 

poisonCounter :: [Observation] -> Int --Checks how many mushrooms are poisonous
poisonCounter [ ] = 0
poisonCounter ((attributes, editbility):observations) = 
    if editbility == NoNom
    then 1 + poisonCounter observations
    else poisonCounter observations

ediblePercent :: [Observation] -> Rational  --numEdible out of length 
ediblePercent lst = outOf (edibleCounter lst) (length lst) 

poisonPercent :: [Observation] -> Rational  --numPoison out of length 
poisonPercent lst = outOf (poisonCounter lst) (length lst) 

bstPercent :: [(Rational, [Observation])] -> [Observation]
bstPercent lst = snd(maximum lst)

wrstPercent :: [(Rational, [Observation])] -> [Observation]
wrstPercent lst = snd(minimum lst)

numCorrect :: Attribute -> [Observation] -> Int
numCorrect attr lst = if ((fst (attributeSplit attr lst)) == [ ] || ((snd (attributeSplit attr lst)) == [ ])) then 0 
else
    let setA = fst (attributeSplit attr lst) --Yes
        setB = snd (attributeSplit attr lst) --No
        setAEdibles = (ediblePercent setA, setA)
        setBEdibles = (ediblePercent setB, setB)
        setEdible = bstPercent [setAEdibles, setBEdibles] 
        setPoison = wrstPercent [setAEdibles, setBEdibles]
    in (edibleCounter setEdible) + (poisonCounter setPoison)


-- A decision tree is a binary tree that stores the likelihood of a mushroom being edible based on
-- its attributes.  Decision nodes are labeled with an attribute and have two children, with the
-- left child applying to mushrooms with that attribute, and the right child applying to mushrooms
-- without that attribute.  End nodes are leaves, and  should store enough information to compute
-- the percent chance of a mushroom being edible.  Do not store lists of observations or mushrooms.
-- Doubles are likely not precise enough, but Rationals or tuples (or even triples) of Integers will
-- be sufficient.

-- Define an algebraic data type for decision trees.
data DTree = EndNode Rational | Node Attribute (DTree) (DTree) deriving Show

-- Given a list of attributes and a list of observations, build a decision tree.
--  * If all the observations have the same edibility, you can safely make an end node: there is no
--    need to further analyze a mushroom.  
--  * If all the observations have the same attributes, you must make an end node : there is no way
--    to futher analyze a mushroom.
--  * Otherwise, go through the list of attributes and find the one that gives you the most
--    information about edibility, as measured by the number of correct guesses that can be obtained if
--    this was the only attribute used.  Then create a decision node using that attribute as a pivot.
--  * For efficiency, you can delete the pivot from the list of attributes, but it doesn't really
--    matter.
--  * You should create helper functions for this problem. 
--bestAttribute::[Attribute] -> [Observation] -> Attribute
--bestAttribute = undefined 

isThisCorrect :: Attribute -> [Observation] -> (Int, Attribute)
isThisCorrect attr obsLst = (numCorrect attr obsLst, attr)

attributeList :: [Attribute] -> [Observation] -> [(Int, Attribute)]
attributeList attrLst obsLst = map (`isThisCorrect` obsLst) attrLst 

doTheyHaveSameEditbility :: [Observation] -> Bool
doTheyHaveSameEditbility obsLst = if ((edibleCounter obsLst) == (length obsLst) || (edibleCounter obsLst) == 0) then True else False 

attributePull :: [Observation] -> [[Attribute]]
attributePull obsLst = [attributeLst | (attributeLst, editbility) <- obsLst]

doTheyHaveSameAttribute :: [Observation] -> Bool
doTheyHaveSameAttribute obsLst = all (== head (attributePull obsLst)) (attributePull obsLst)

whichIsTheBstAttribute :: [Attribute] -> [Observation] -> Attribute
whichIsTheBstAttribute attrLst obsLst = snd (maximum (attributeList attrLst obsLst))

buildTree :: [Attribute] -> [Observation] -> DTree
buildTree attrLst obsLst 
    | ((doTheyHaveSameEditbility obsLst) == True) = (EndNode ((edibleCounter obsLst) `outOf` (length obsLst)))
    | ((doTheyHaveSameAttribute obsLst) == True) = (EndNode ((edibleCounter obsLst) `outOf` (length obsLst))) 
    | otherwise = Node bestAttribute (buildTree attrLst (fst (attributeSplit bestAttribute obsLst))) (buildTree attrLst (snd (attributeSplit bestAttribute obsLst)))
        where bestAttribute = whichIsTheBstAttribute attrLst obsLst 


-- rateMushroom takes a mushroom, a decision tree, and a safety limit, and returns a string
-- describing if we can eat the mushroom.  Follow the decision tree for this mushroom, and check if
-- the corresponding end node estimates the chance of edibility to be higher than the safety limit.
-- If it is greater than the safety limit, return the string "Eat the mushroom" 
-- If it is less than or equal to the safety limit, return the string "Do not eat the mushroom"
-- For full credit, append the estimate to the string similar to the below:
--   "Eat the mushroom: estimate a 95% chance of being edible."
--   "Do not eat the mushroom: estimate a 40% chance of being poisonous."
-- The ``precentOfRatio`` and ``percentOfCount`` functions may be helful.
rateMushroom :: Mushroom -> DTree -> Rational -> String
rateMushroom shrooms (EndNode rational) isItSafe = if rational > isItSafe then "Eat the mushroom" else "Do not eat the mushroom" 
rateMushroom shrooms (Node attribute yes no) isItSafe = if attribute `elem` shrooms then rateMushroom shrooms yes isItSafe else rateMushroom shrooms no isItSafe

-- buildGuide takes a decision tree, a safety limit, and return an itemized guide. 
-- Each line is numbered separately and should have one of two forms.
--  "n: Eat the mushroom." / "n: Do not eat the mushroom."
--  "n: If the mushroom has (attribute) go to step x, otherwise go to step y."
-- For this implementation, every node in the tree will have a separate line.
-- You will need helper functions.
nodeCount :: DTree -> Int
nodeCount (EndNode rational) = 1
nodeCount (Node attr yes no) = 1 + (nodeCount yes) + (nodeCount no) 

displayTree :: Int -> DTree -> Rational -> [String]
displayTree nodes (EndNode rational) isItSafe = if rational > isItSafe then [(show nodes) ++ ": Eat the mushroom"] else [(show nodes) ++ ": Do not eat the mushroom"]
displayTree nodes (Node attr yes no) isItSafe =  
    let right = displayTree ((nodeCount no) +nodes + 1) no isItSafe
        left = displayTree (1 + nodes) yes isItSafe
        currentNode = [(show nodes) ++ ": If the mushroom has " ++ (show attr) ++ ", then go to step " ++ (show (1 + nodes)) ++ ", otherwise go to step " ++ (show ((nodeCount no) + nodes + 1))]
    in currentNode ++ left ++ right 

buildGuide :: DTree -> Rational -> [String]
buildGuide tree rational = displayTree 1 tree rational 

--
--                                     Full Credit
--

-- For the first full credit, improve on the derived Show instance for attributes.  Make a custom
-- instance of Show that returns a proper English clause describing the attribute. For instance, "a
-- club stalk", "narrow gills", or "an almond odor." 


-- For the second full credit portion, you will eliminate redundancies in the guide. This will be
-- done using common subexpression elimination. We will keep an index mapping strings to integer
-- locations. Since indexes are useful for other types, we will write generic functions.
type Index a = [(a, Int)]

-- makeEntry adds an element to the index. It returns the location of the element in 
-- the index and, if necessary, an updated index.
-- If the element is already in the index, you should not add it again. 
-- If it does not occur in the index, find the next largest location and associate that element with
-- that location.
-- Index locations should start at 1.
makeEntry :: Eq a => a -> Index a -> (Int, Index a)
makeEntry = undefined


-- For instance: makeEntry 'x' [('a',1),('x',2),('b',3)] = (2, [('a',1),('x',2),('b',3)])
-- For instance: makeEntry 'y' [('a',1),('x',2),('b',3)] = (4, [('a',1),('x',2),('b',3),('y',4)])
-- The order of the entries in the index does not matter, and will quite likely be reversed.

-- Once makeEntry is working, make a version of buildGuide (buildGuideCSE) that passes around an index of
-- strings.  When you want to add a string to the index, use makeEntry to avoid creating duplicates.
-- As a natural consequence of this, you will return an "upside-down" guide: the entry point will
-- have the largest location.

buildGuideCSE :: DTree -> Rational -> [String]
buildGuideCSE = undefined

-- For extra credit, change indexes from association lists to association binary search trees.
