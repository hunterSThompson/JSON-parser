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
import Data.Text (strip, pack, unpack)


data JSONnode = JSONnode String JSONdata | Fail
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
showJNode Fail = "Failed!"

-- Pipe operator for easy chaining
x |> f = f x

-- Main entry point --
main = do
	contents <- getContents
	let new = filterReturns contents
	putStr $ show new
	--putStr $ map toUpper contents


--newObjNode :: String -> JSONnode
newObjNode :: String -> String
newObjNode str = 
	let 
		prepedStr = str |> stripWhiteSpace |> stripObject
		(key, jStr) = splitNode prepedStr
		splits = (splitOn "," jStr) |> (map stripWhiteSpace) 
		--key = "key"
	--in JSONnode key (JSONobject [])
	in jStr

-- For parsing a "key-less" object.  Only used for 'outermost' node
initFunc :: String -> JSONdata
initFunc jStr =
	let prepedStr = jStr |> stripWhiteSpace |> stripObject |> stripWhiteSpace
	    nodes = (splitOn "," jStr) |> (map stripWhiteSpace) |> (map jsonParse)
	in JSONobject nodes

newStringNode :: String -> JSONnode
newStringNode str = 
	let -- preped = prepString str (strip the {}/[] and extra spaces)
		key = "key"
	in JSONnode key (JSONstring str)

newArrayNode :: String -> JSONnode
newArrayNode obj =
	let key = "key"
	in JSONnode key (JSONobject [])

removeQuotes :: String -> String
removeQuotes str = 
	let first = if head str == '\"' then tail str else str
	in 
		if last first == '\"' then init first else first
	


-- New Parser Func --
jsonParse :: String -> JSONnode
jsonParse jStr
	-- | isObject jStr = newObjNode jStr
	| isArray'  = newArrayNode jStr
	-- | isBool        = createBoolNode key str
	-- | isNum 	= createNumNode key str
	-- | isString str  = JSONnode key (JSONstring str)
	| otherwise = JSONnode "FAIL" (JSONbool False)
	-- | otherwise     = JSONnode key (JSONstring str)
	where 
		isObject' = isObject jStr
		isArray'  = isArray jStr
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
stripWhiteSpace :: String -> String
stripWhiteSpace str =
	str |> pack |> strip |> unpack

stripObject :: String -> String -- TD: Add an error context.
stripObject str = 
	tail $ init str

stripArray :: String -> String -- TD: Add an error context.
stripArray str = 
	tail $ init str

-- Splitter functions --
splitNode :: String -> (String, String) --Gross. will refactor later
splitNode str = 
	let index = findIndex (\x -> x == ':') str
	in if index == Nothing
		then ("", "")
	else
		let (key, jStr) = splitAt (fromJust index) str
		in let
			newStr = tail jStr
			in (key |> stripWhiteSpace, newStr |> stripWhiteSpace)


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











