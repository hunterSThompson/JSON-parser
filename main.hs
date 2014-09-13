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


-- Types --
data JSONnode = JSONnode String JSONdata | Fail

data JSONdata =   JSONarray [JSONdata] 
		| JSONstring String
		| JSONobject [JSONnode]
		| JSONbool Bool
		| JSONnum Float

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
	

-- New Parser Func --
jsonParse :: String -> JSONnode
jsonParse str
	-- | isObject jStr = newObjNode jStr
	| isArray'       = newArrayNode jStr
	-- | isBool jStr    = createBoolNode key str
	| otherwise      = newBoolNode jStr
	-- | isNum 	= createNumNode key str
	-- | isString str  = JSONnode key (JSONstring str)
	-- | otherwise = JSONnode "FAIL" (JSONbool False)
	-- | otherwise     = JSONnode key (JSONstring str)
	where 
		jStr = str |> trim
		isObject' = isObject jStr
		isArray'  = isArray jStr
		--isNum'    = isObject jsonString
		--isString' = isObject jsonString
		--isObject' = isObject jsonString
		

-- Main Parse Funcion --
{- For parsing a "key-less" object.  Only used for 'outermost' node -}
parse :: String -> JSONdata
parse jStr =
	let prepedStr = jStr |> trim |> stripObject |> trim 
	    nodes = (splitOn "," prepedStr) |> (map trim) |> (map jsonParse)
	in JSONobject nodes

-- JSON type handlers --
newObjNode :: String -> JSONnode
newObjNode str = 
	let 
		str1 = str |> trim{-Cut out white space -}
		(key, str2) = splitNode str1 {-Split the node into key & string -}
		str3 = str2 |> stripObject {-Strip {} off object string -}
		splits = (splitOn "," str3) |> (map trim) {-Split properties -}
		objs = map (\x -> jsonParse x) splits {-Parse each object string -}
	in JSONnode key (JSONobject objs)

newStringNode :: String -> JSONnode
newStringNode str = 
	let 
		key = "key"
	in JSONnode key (JSONstring str)

newArrayNode :: String -> JSONnode
newArrayNode obj =
	let key = "key"
	in JSONnode key (JSONobject [])

stringToBool :: String -> JSONdata
stringToBool str = if str == "False" then
		JSONbool False
		else JSONbool True

newBoolNode :: String -> JSONnode
newBoolNode str =
	let (key, str1) = splitNode str
	    node = stringToBool str1
	in JSONnode key node


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


-- Prep Functions --
filterReturns :: String -> String
filterReturns = filter (\x -> (x /= '\n'))

filterQuotes :: String -> String
filterQuotes = filter (\x -> (x /= '\"'))

prepString :: String -> String -- Composed. Yes!
prepString = filterReturns . filterQuotes 


-- Strip Functions --
trim :: String -> String
trim str =
	str |> pack |> strip |> unpack

stripObject :: String -> String -- TD: Add an error context.
stripObject str = 
	tail $ init str

stripArray :: String -> String -- TD: Add an error context.
stripArray str = 
	tail $ init str

removeQuotes :: String -> String
removeQuotes str = 
	let first = if head str == '\"' then tail str else str
	in 
		if last first == '\"' then init first else first

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
			in (key |> trim, newStr |> trim)


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











