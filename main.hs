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

-- Show instances to display JSONnodes j--
instance Show JSONnode where
	show (JSONnode key jData) = "\"" ++ key ++ "\" : " ++ (show jData)
	show Fail = "failed!!"

instance Show JSONdata where
	show jd = showJData jd

showJData (JSONstring s) = show s
showJData (JSONbool s) = show s
showJData (JSONnum s) = show s
showJData (JSONobject s) = concat $ map (\x -> "\t" ++ show x ++ "\n") s
showJData (JSONarray s) = concat $ map show s


-- Pipe operator for easy chaining
x |> f = f x


-- Main entry point --
main = do
	contents <- getContents
	--let new = filterReturns contents
	--let parsed = jsonParse contents
	--let noQuotes = removeQuotes contents
	let noQuotes = parse contents
	putStrLn "Result:"
	putStr $ show noQuotes
	

-- New Parser Func --
jsonParse :: String -> JSONnode
jsonParse str 
	| isObject' 	= newObjNode key jStr
	| isArray'      = newArrayNode key jStr
	| isBool'   	= newBoolNode key jStr
	| isNum'	= newNumNode key jStr
	| otherwise     = newStringNode key jStr
	where 
		(key, jStr) = splitNode str
		isBool' = jStr == "True" || jStr == "False"
		isNum' = isNum jStr
		isArray' = head jStr == '['
		isObject' = head jStr == '{'
		

-- Main Parse Funcion --
{- For parsing a "key-less" object.  Only used for 'outermost' node -}
parse :: String -> JSONdata
parse jStr =
	let prepedStr = jStr |> trim |> stripObject |> trim 
	    nodes = (splitOn "," prepedStr) |> map (\x -> x |> trim |> jsonParse)
	in JSONobject nodes

-- JSON type handlers --
newObjNode :: String -> String -> JSONnode
newObjNode key str = 
	let 
		str3 = str |> stripObject {- Strip {} off object string -}
		splits = (splitOn "," str3) |> (map trim) {- Split properties -}
		objs = map (\x -> jsonParse x) splits {- Parse each object string -}
	in JSONnode key (JSONobject objs)

newStringNode :: String -> String -> JSONnode
newStringNode key str = JSONnode key (JSONstring str)

newArrayNode :: String -> String -> JSONnode
newArrayNode key obj =
	-- split by ',' then map jsonParse to result.
	-- if lists in JSON are homogenous, check all first and throw error
	JSONnode key (JSONobject [])

newBoolNode :: String -> String -> JSONnode
newBoolNode key str =
	let node = if str == "False" then JSONbool False else JSONbool True
	in JSONnode key node

newNumNode :: String -> String -> JSONnode
newNumNode key str =
	let num = read str :: Float
	in JSONnode key (JSONnum num)


-- JSON type checkers --
startsWith :: String -> Char -> Bool
startsWith str char = head str == char

isNum :: String -> Bool
isNum str = 
	let res = readMaybe str :: Maybe Float
	in if res == Nothing
	      then False
	      else True

-- Prep Functions --
filterReturns :: String -> String
filterReturns = filter (\x -> (x /= '\n'))

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
	let 
		trm = trim str
		first = if head trm == '\"' then tail trm else trm
	in 
		if last first == '\"' then init first else first

-- Splitter functions --
splitNode :: String -> (String, String) --Gross. will refactor later
splitNode str =
	let index = findIndex (\x -> x == ':') str
	in cut str index

cut :: String -> Maybe Int -> (String, String)
cut _ Nothing = ("", "")
cut str (Just index) = 
	let (key, jStr) = splitAt index str
	in (key |> removeQuotes, jStr |> tail |> trim)



-- Sample Data --
x = JSONstring "poop"
d = JSONnum 56
e = JSONbool True
n = JSONnode "expression" x
c = JSONnode "expression" x
y = JSONobject [n,c]
b = JSONnode "exp" y
k = JSONnode "key" (JSONarray [x,d,e])
p = JSONnode "key1" y

--sampData = "{\"name\":\"True\"}"
--sampData = "\"name\":\"150\""
--sampData = "\"name\":{key:150,secKey:True}"
sampData = "\"name\":{key:150}"

dt = "name:key"

prepedData = prepString sampData

sampleData = "{\"menu\": {\"id\": \"file\",\"value\": \"File\",\"popup\":\"menuitem\": [{\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},{\"value\": \"Open\", \"onclick\": \"OpenDoc()\"},{\"value\": \"Close\", \"onclick\": \"CloseDoc()\"}]}}}"
