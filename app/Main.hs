{--
-- EPITECH PROJECT, 2024
-- B-FUN: WALFRAM
-- File description:
-- main file
--}


module Main (main) where
import System.Environment
import System.Exit
import Data.Maybe (isNothing)
import Text.Read
import System.IO

data Conf = Conf {
    rule :: Int,
    start :: Int,
    linez :: Int,
    window :: Int,
    move :: Int
} | Error {
    error :: String
} deriving (Show)



usage :: IO ()
usage = hPutStrLn stderr "USAGE:"
 >> hPutStrLn stderr "\t./wolfram --rule x1 --lines x2 --window x3 --start x4 --move x5\n"
 >> hPutStrLn stderr "DESCRIPTION"
 >> hPutStrLn stderr "\t--rule x1:\tthe ruleset to use, no default value, mandatory! (1 <= x1 <= 256)"
 >> hPutStrLn stderr "\t--lines x2:\tthe number of lines to display (if none, program never stops)"
 >> hPutStrLn stderr "\t--window x3:\tthe number of cells to display on each line (80 by default)"
 >> hPutStrLn stderr "\t--start x4:\tthe generation number at wich to start the display (0 by default)"
 >> hPutStrLn stderr "\t--move x5:\ta translation to apply on the window (negative == left, positive == righ)"


argsParser :: Conf -> [String] -> Conf
argsParser (Conf _ x2 x3 x4 x5) ("--rule":r:xs) 
    | isNothing (readMaybe r :: Maybe Int) = Error "Must be Int"
    | (read r :: Int) < 0 = Error "Rule must be Int > 0"
    | (read r :: Int) > 255 = Error "Rule must be Int < 256"
    | otherwise = argsParser (Conf (read r) x2 x3 x4 x5) xs

argsParser (Conf x1 _ x3 x4 x5) ("--start":s:xs)
    | isNothing (readMaybe s :: Maybe Int) = Error "Must be Int"
    | (read s :: Int) < 0 = Error "Start must be Int > 0"
    | otherwise = argsParser (Conf x1 (read s) x3 x4 x5) xs

argsParser (Conf x1 x2 _ x4 x5) ("--lines":l:xs)
    | isNothing (readMaybe l :: Maybe Int) = Error "must be Int"
    | (read l :: Int) < 0 = Error "Lines must be Int > 0"
    | otherwise = argsParser (Conf x1 x2 (read l) x4 x5 ) xs

argsParser (Conf x1 x2 x3 _ x5) ("--window":w:xs)
    | isNothing (readMaybe w :: Maybe Int) = Error "Must be Int"
    | (read w :: Int) < 0 = Error "Window must be Int > 0"
    | otherwise = argsParser (Conf x1 x2 x3 (read w) x5) xs

argsParser (Conf x1 x2 x3 x4 _) ("--move":m:xs)
    | isNothing (readMaybe m :: Maybe Int) = Error "Move must be Int"
    | otherwise = argsParser (Conf x1 x2 x3 x4 (read m)) xs

argsParser conf [] = conf
argsParser _ (x:_) = Error ( x ++ ": unkown argument")


argsChecker :: Conf -> IO ()
argsChecker (Error e) = hPutStrLn stderr e
 >> usage
 >> exitWith (ExitFailure 84)

argsChecker (Conf (-1) _ _ _ _) = usage 
 >> exitWith (ExitFailure 84)

argsChecker _ = return ()


decimalToBinary :: Int -> [Int]
decimalToBinary n = padTo8Bits $ reverse $ go n
  where
    go 0 = []
    go x = let (q, r) = x `divMod` 2
           in r : go q

    padTo8Bits bits = replicate (8 - length bits) 0 ++ bits

createFirstLine :: Conf -> [Char]
createFirstLine (Conf _ _ _ windowSize _) =
    replicate (250 * windowSize) ' '
    ++ replicate (windowSize `div` 2) ' '
    ++ "*"
    ++ replicate (windowSize `div` 2 - 1) ' '  -- Retirer un espace avant "*"
    ++ replicate (250 * windowSize) ' '

generateNewLine :: [Int] -> [Char] -> [Char]
generateNewLine binRuleNB ('*':'*':'*':x)
    | binRuleNB!!0 == 1 = ('*':generateNewLine binRuleNB ('*':'*':x)) 
    | otherwise = (' ':generateNewLine binRuleNB ('*':'*':x))

generateNewLine binRuleNB ('*':'*':' ':x) 
    | binRuleNB!!1 == 1 = ('*':generateNewLine binRuleNB ('*':' ':x)) 
    | otherwise = (' ':generateNewLine binRuleNB ('*':' ':x))

generateNewLine binRuleNB ('*':' ':'*':x) 
    | binRuleNB!!2 == 1 = ('*':generateNewLine binRuleNB (' ':'*':x)) 
    | otherwise = (' ':generateNewLine binRuleNB (' ':'*':x))

generateNewLine binRuleNB ('*':' ':' ':x) 
    | binRuleNB!!3 == 1 = ('*':generateNewLine binRuleNB (' ':' ':x)) 
    | otherwise = (' ':generateNewLine binRuleNB (' ':' ':x))

generateNewLine binRuleNB (' ':'*':'*':x) 
    | binRuleNB!!4 == 1 = ('*':generateNewLine binRuleNB ('*':'*':x)) 
    | otherwise = (' ':generateNewLine binRuleNB ('*':'*':x))

generateNewLine binRuleNB (' ':'*':' ':x) 
    | binRuleNB!!5 == 1 = ('*':generateNewLine binRuleNB ('*':' ':x)) 
    | otherwise = (' ':generateNewLine binRuleNB ('*':' ':x))

generateNewLine binRuleNB (' ':' ':'*':x) 
    | binRuleNB!!6 == 1 = ('*':generateNewLine binRuleNB (' ':'*':x)) 
    | otherwise = (' ':generateNewLine binRuleNB (' ':'*':x))

generateNewLine binRuleNB (' ':' ':' ':x) 
    | binRuleNB!!7 == 1 = ('*':generateNewLine binRuleNB (' ':' ':x)) 
    | otherwise = (' ':generateNewLine binRuleNB (' ':' ':x))

generateNewLine _ _ = [' '] 


pimpMyLine :: Int -> Int -> String -> String
pimpMyLine win mov str =
    take win $ drop (midIndex - (win `div` 2)) str
  where
    midIndex = (length str `div` 2) - mov


printMyLine :: Conf -> [Char] -> IO ()
printMyLine conf@(Conf _ start _ _ _ ) str
    | start <= 1 = putStrLn str
    | otherwise = return()

generateWolfram :: Conf -> [Int] -> [Char] -> IO ()
generateWolfram (Conf _ _ 1 _ _) _ _ = return ()
generateWolfram (Conf ruleNb start linez window move) binRuleNB prevLine
    | start > 0 = do
        let newLine = generateNewLine binRuleNB prevLine
        let pimpedLine = pimpMyLine window move newLine
        printMyLine (Conf ruleNb start linez window move) pimpedLine
        generateWolfram (Conf ruleNb (start - 1) linez window move) binRuleNB $ ' ':newLine
    | otherwise = do
        let newLine = generateNewLine binRuleNB prevLine
        let pimpedLine = pimpMyLine window move newLine
        printMyLine (Conf ruleNb start linez window move) pimpedLine
        generateWolfram (Conf ruleNb (start - 1) (linez - 1) window move) binRuleNB $ ' ':newLine


printWolfram :: Conf -> IO ()
printWolfram conf@(Conf ruleNb start linez win mov) = do
    let binRuleNB = decimalToBinary ruleNb
    if start > 0    
        then generateWolfram conf binRuleNB (createFirstLine conf)
        else do
            let firstLine = createFirstLine conf
            putStrLn (pimpMyLine win mov firstLine)
            generateWolfram conf binRuleNB firstLine

main :: IO ()
main = do
    args <- getArgs
    let default_conf = Conf (-1) 0 (-1) 80 0
    let parse = argsParser default_conf args
    argsChecker parse
    printWolfram parse
