import Data.Char (isDigit, isLower, isUpper, isAlpha, isPunctuation)
import System.Random (randomRIO)
import System.IO
import Data.Time (getCurrentTime)


--primary's functions--

passwordAnalyzer :: String -> String
passwordAnalyzer password  
    |scoring password < 6 = "Debil : contraseña con los siguientes defectos: " 
    |scoring  password < 8 ="Media : contraseña aceptable.\n" ++ "Sugerencias: \n" ++ unlines (suggestion password)
    |otherwise  = "Fuerte : contraseña segura.\n " ++ "Sugerencias: \n" ++ unlines (suggestion password)


passwordBuilder :: Int -> [Condition] -> IO String
passwordBuilder passwordLength conditions = sequence (replicate passwordLength (wordWith conditions))

passwordEncryptor:: String -> Int -> String
passwordEncryptor password shift = asciiToPassword (map (+ shift) (passwordToAscii password))

passwordDecryptor :: String -> Int -> String
passwordDecryptor password shift = asciiToPassword (map (- shift) (passwordToAscii password))

writeLog :: FilePath -> String -> IO ()
writeLog logFile message = do
    currentTime <- getCurrentTime                     --getting the time 
    let logMessage = show currentTime ++ "-" message  --This is the structure of the logs 
    appendFile logFile (logMessage ++ "\n")           --Adding the message to the logFile

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

--Transalate char-> ASCII 
asciiConverter :: Char -> Int
asciiConverter letter = ord letter

passwordToAscii :: String -> [Int]
passwordToAscii word =  map (\letter -> asciiConverter letter ) word

--Simplificated way--
asciiToPassword ::[Int] -> String
asciiToPassword  = map (asciiConverterReverse . asciiCycle) 

asciiConverterReverse :: Int -> Char
asciiConverterReverse number = chr number

asciiCycle :: Int -> Int
asciiCycle number 
    | number > 126 = 33 + ((number - 33) `mod` 94)
    | number < 33  = 126 - ((33 - number) `mod` 94)
    | otherwise    = number

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



