import Data.Char (isDigit, isLower, isUpper, isAlpha, isPunctuation)
import System.Random (randomRIO)



--primary's functions--

passwordAnalyzer :: String -> String
passwordAnalyzer password  
    |scoring password < 6 = "Debil : contraseña con los siguientes defectos: " 
    |scoring  password < 8 ="Media : contraseña aceptable.\n" ++ "Sugerencias: \n" ++ unlines (suggestion password)
    |otherwise  = "Fuerte : contraseña segura.\n " ++ "Sugerencias: \n" ++ unlines (suggestion password)


passwordBuilder :: Int -> [Condition] -> IO String
passwordBuilder passwordLength conditions = sequence (replicate passwordLength (wordWith conditions))

--Imperative way-- 
wordWith ::  [Condition] ->  IO Char
wordWith listOfconditions = do 
    randomChar <- generateChar pool
    if satisfiesCondition randomChar listOfconditions 
        then return randomChar
    else wordWith listOfconditions

--auxiliar's function-- 
scoring :: String -> Float 
scoring password =  (0.4 * lenghtScoring password) + (0.3 * commonWordsScoring password) + (0.3 * characterVarietyScoring password)

suggestion ::  String -> [String]
suggestion password = [ msg | (condition, msg) <- conditionsWithMessages, not (condition password) ] --(condition, msg) <- iteraccion sobre listas 

--Choose a random element of pool 
generateChar :: String -> IO Char
generateChar listOfChars = (listOfChars !!) <$> randomRIO (0, length listOfChars - 1)  

--type of scoring--   
lenghtScoring :: String -> Float 
lenghtScoring password 
    | length password < 6 = 2
    | length password < 8 = 4
    | length password < 12 = 7
    |otherwise = 10                              

commonWordsScoring ::  String -> Float
commonWordsScoring password  
    |elem password commonWords  = 0
    |otherwise = 10 

characterVarietyScoring :: String -> Float
characterVarietyScoring password   
    |countConditions password == 4 = 10
    |countConditions password == 3 = 7
    |countConditions password == 2 = 5
    |countConditions password == 1 = 3
    |otherwise = 0 

--Conditions--
type Condition = String -> Bool

digitCondition :: Condition
digitCondition  = any isDigit  

mayusCondition :: Condition
mayusCondition  = any isUpper

lowerCondition :: Condition
lowerCondition  =  any isLower 

punctuationCondition :: Condition
punctuationCondition  =  any isPunctuation 

conditions :: [Condition]
conditions = [digitCondition , mayusCondition, lowerCondition, punctuationCondition]

conditionsWithMessages :: [(Condition, String)]
conditionsWithMessages =
    [ (digitCondition, "Agregar al menos un número")
    , (mayusCondition, "Agregar al menos una letra mayúscula")
    , (lowerCondition, "Agregar al menos una letra minúscula")
    , (punctuationCondition, "Agregar al menos un carácter especial")
    ]

countConditions :: String ->  Int 
countConditions password = length (filter (\condition -> condition password) conditions)

satisfiesCondition :: Char -> [Condition] -> Bool
satisfiesCondition word listOfconditions = any (\condition -> condition [word]) listOfconditions

--Dictionary --
commonWords :: [String]
commonWords = ["hola"]

-- CharSet--

digits, uppercase, lowercase, punctuation :: String
digits = ['0'..'9']
uppercase = ['A'..'Z']
lowercase = ['a'..'z']
punctuation = "!@#$%^&*()-_=+[]{};:,.<>?/|"

pool:: String
pool = digits ++ uppercase ++ lowercase ++ punctuation



