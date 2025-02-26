import Data.Char (isDigit, isLower, isUpper, isAlpha, isPunctuation)



passwordAnalyzer :: String -> String
passwordAnalyzer password  
    |scoring password < 6 = "Debil : contraseña con los siguientes defectos: " 
    |scoring  password < 8 ="Media : contraseña aceptable.\n" ++ "Sugerencias: \n" ++ unlines (suggestion password)
    |otherwise  = "Fuerte : contraseña segura.\n " ++ "Sugerencias: \n" ++ unlines (suggestion password)

scoring :: String -> Float 
scoring password =  (0.4 * lenghtScoring password) + (0.3 * commonWordsScoring password) + (0.3 * characterVarietyScoring password)

suggestion ::  String -> [String]
suggestion password = [ msg | (condition, msg) <- conditionsWithMessages, not (condition password) ] --(condition, msg) <- iteraccion sobre listas 


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
digitCondition :: String -> Bool
digitCondition  = any isDigit  

mayusCondition ::  String -> Bool
mayusCondition  = any isUpper

lowerCondition :: String -> Bool
lowerCondition  =  any isLower 

punctuationCondition :: String -> Bool
punctuationCondition  =  any isPunctuation 

conditions :: [String -> Bool]
conditions = [digitCondition , mayusCondition, lowerCondition, punctuationCondition]

conditionsWithMessages :: [(String -> Bool, String)]
conditionsWithMessages =
    [ (digitCondition, "Agregar al menos un número")
    , (mayusCondition, "Agregar al menos una letra mayúscula")
    , (lowerCondition, "Agregar al menos una letra minúscula")
    , (punctuationCondition, "Agregar al menos un carácter especial")
    ]

countConditions :: String ->  Int 
countConditions password = length (filter (\condition -> condition password) conditions)

--Dictionary --
commonWords :: [String]
commonWords = ["hola"]

