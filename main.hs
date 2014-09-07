--
-- JSON Parser
--
-- Hunt Graham
--

import Data.List.Split
import Data.Maybe
import Data.Char
import Text.Read
import Data.List


data JSONnode = JSONnode String JSONdata 
		--deriving (show)

data JSONdata =   JSONarray [JSONdata] 
		| JSONstring String
		| JSONobject [JSONnode]
		| JSONbool Bool
		| JSONnum Float
		--deriving (show)

-- Show instances to display JSONnodes --
instance Show JSONnode where
	show (JSONnode key jData) = key ++ ": " ++ (showJData jData)

instance Show JSONdata where
	show jd = showJData jd

showJData (JSONstring s) = show s
showJData (JSONbool s) = show s
showJData (JSONnum s) = show s
showJData (JSONobject s) = concat $ map show s
showJData (JSONarray s) = concat $ map show s

showJNode :: JSONnode -> String
showJNode (JSONnode key jData) = key ++ ": " ++ (show jData)

-- Pipe operator for easy chaining
x |> f = f x

-- Main entry point --
main = do
	contents <- getContents
	let new = filterReturns contents
	putStr $ show new
	--putStr $ map toUpper contents

test = parseJSON prepedData


-- Old Parser Func --
parseJSON :: String -> JSONnode
parseJSON jsonString
 	| isObject str  = createObjectNode key str
	| isBool str 	= createBoolNode key str
	| isNum str 	= createNumNode key str
	-- | isString str  = JSONnode key (JSONstring str)
	-- | otherwise     = JSONnode key (JSONnum 69)
	| otherwise     = JSONnode key (JSONstring str)
	where 
	      splits = splitNode3 jsonString
	      key    = fst splits
	      str    = snd splits


newObjNode :: String -> JSONnode
newObjNode obj = JSONnode "key" (JSONobject [])

newStringNode :: String -> JSONnode
newStringNode str = 
	let key = "key"
	in JSONnode key (JSONstring str)



-- New Parser Func --
jsonParse :: String -> Maybe JSONnode
jsonParse jStr
	| isObject' = Just (newObjNode jStr)
	-- | isArray
	-- | isBool        = createBoolNode key str
	-- | isNum 	= createNumNode key str
	-- | isString str  = JSONnode key (JSONstring str)
	| otherwise     = Nothing
	-- | otherwise     = JSONnode key (JSONstring str)
	where 
		isObject' = isObject jStr
		--isArray'  = isObject jsonString
		--isNum'    = isObject jsonString
		--isString' = isObject jsonString
		--isObject' = isObject jsonString


-- JSON type checkers --
startsWith :: String -> Char -> Bool
startsWith str char = head str == char

isNum :: String -> Bool
isNum str = 
	let res = readMaybe str :: Maybe Float
	in if res == Nothing
	      then False
	      else True

isObject :: String -> Bool
isObject str = head str == '{'

isString :: String -> Bool
isString str = head str == '"'

isArray :: String -> Bool
isArray str = head str == '['

isBool :: String -> Bool
isBool str = str == "False" || str == "True"

-- JSON creator funcs --
createBoolNode :: String -> String -> JSONnode
createBoolNode key str = 
	if str == "False" then JSONnode key (JSONbool False) else JSONnode key (JSONbool True)

createObjectNode :: String -> String -> JSONnode
createObjectNode key str =
	let strippedStr = stripObject str
	    props = splitOn "," strippedStr
	    nodes = map parseJSON props
	in JSONnode key (JSONobject nodes)

createNumNode :: String -> String -> JSONnode
createNumNode key str =
	let num = read str
	in JSONnode key (JSONnum num)

-- Prep Functions --
filterReturns :: String -> String
filterReturns = filter (\x -> (x /= '\n')) 

filterQuotes :: String -> String
filterQuotes = filter (\x -> (x /= '\"')) 

prepString :: String -> String -- Composed. Yes!
prepString = filterReturns . filterQuotes 

-- Strip Functions --
stripObject :: String -> String -- TD: Add an error context.
stripObject str = 
	tail $ init str

stripArray :: String -> String -- TD: Add an error context.
stripArray str = 
	tail $ init str

-- Splitter functions --
splitNode3 :: String -> (String, String)
splitNode3 str = 
	let index = findIndex (\x -> x == ':') str
	in if index == Nothing
	      then ("", "")
	else 
		let front = slice 0 (fromJust index - 1) str
		    back = slice (fromJust index + 1) (length str - 1) str
		in (front, back)

slice :: Int -> Int -> String -> String
slice from to xs = take (to - from + 1) (drop from xs)

-- Sample Data --
x = JSONstring "poop"
d = JSONnum 56
e = JSONbool True
n = JSONnode "expression" x
c = JSONnode "expression" x
y = JSONobject [n,c]
b = JSONnode "exp" y
k = JSONnode "key" (JSONarray [x,d,e])

--sampData = "{\"name\":\"True\"}"
--sampData = "\"name\":\"150\""
--sampData = "\"name\":{key:150,secKey:True}"
sampData = "\"name\":{key:150}"

dt = "name:key"

prepedData = prepString sampData

sampleData = "{\"menu\": {\"id\": \"file\",\"value\": \"File\",\"popup\":\"menuitem\": [{\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},{\"value\": \"Open\", \"onclick\": \"OpenDoc()\"},{\"value\": \"Close\", \"onclick\": \"CloseDoc()\"}]}}}"











