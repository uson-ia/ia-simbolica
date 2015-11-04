-- Tom: an implementation of 16-year old Tom Riddles' evil dairy ---------------

-------------------------------------------------------------------------------
import Data.List
import System.Random
import Text.Regex

-- Main -----------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Hello?"
  mainloop

mainloop :: IO ()
mainloop = do
  input <- getLine
  seed <- randomIO
  putStrLn $ respond input (mkStdGen seed)
  mainloop

------------------------------------------------------------------------------

-- Custom types --------------------------------------------------------------
type RawIRPair = ([String], [String])
type IRPair = ([Regex], [String])

-- Response ------------------------------------------------------------------
combinedResponses :: [RawIRPair]
combinedResponses = foldl' (++) [] [general, hpLines, nonSequitur ]


responses :: [IRPair]
responses = regexify combinedResponses
  where regexify :: [RawIRPair] -> [IRPair]
        regexify []     = []
        regexify (x:xs) =
          [(map (\i -> mkRegexWithOpts i True False) (fst x), snd x)] ++
          regexify xs
          
respond :: RandomGen g => String -> g -> String
respond input randGen = pickResponse (findMatchingResponses input) randGen

pickResponse :: RandomGen g => ([String], [String]) -> g -> String
pickResponse (responses, substrings) randGen =
  fillIn (responses !! chosen) substrings
  where chosen = (fst $ randomR (0, length responses - 1) randGen :: Int)

fillIn :: String -> [String] -> String
fillIn response substrings = fillInCount response substrings 0

fillInCount :: String -> [String] -> Int -> String
fillInCount response (x:xs) n =
  fillInCount (subRegex (mkRegex ("!" ++ show n)) response x) xs (n+1)
fillInCount response [] _ = response

findMatchingResponses :: String -> ([String], [String])
findMatchingResponses input = checkListOfTuples input responses
  where checkListOfTuples input (x:xs) =
           case (checkTuple input x) of
             ([], []) -> checkListOfTuples input xs
             (responses, substrings) -> (responses, substrings)

checkTuple :: String -> IRPair -> ([String], [String])
checkTuple input (i:is, r) =
  case (matchRegex i input) of
    Just a -> (r, a)
    Nothing -> checkTuple input (is, r)
checkTuple _ _ = ([], [])


general :: [RawIRPair]
general = [
  (["hello"], ["Hi. What's going on?", "Hello. It's been a while, who are you?", "Hello, my name is Tom Riddle", "Hello. My name is Tom Riddle. How did you come by my diary?"]),
  (["I need ([a-zA-Z ]*)", "I want ([a-zA-Z ]*)"],["Well, do you think it's at all possible to get !0?", "What would changemm if you got !0?"]),
  (["Are you ([a-zA-Z ]*)"], ["I am as !0 as you. Well, maybe no quite.", "Hah! Are you !0?"])
    ]

hpLines :: [RawIRPair]
hpLines = [
    (["My name ([a-zA-Z ]*)"], ["Hello !0, my name is Tom Riddle", "Hello, !0. My name is Tom Riddle. How did you come by my diary?"]),
    (["Do you know anything about the [cC]hamber of [sS]ecrets?"], ["Everyone asks me about the Chamber! I'm rather tired of that question.", "Yes", "I know only the heir may open it."]),
    (["I found it"], ["Lucky that I recorded my memories in some more lasting way than ink. But I always knew that there would be those who would not want this diary read."]),
    (["Hogwarts"], ["I loved Hogwarts, it was my first real home.", "Which house are you in?"]),
    (["House"], ["Which house are you in?"]),
    (["Quidditch"], ["Witches and wizards zooming around on brooms...Can't say I am a fan"]),
    (["Slytherin"], ["Slytherin... ambitious and cunning...", "Slytherin is not to be trifled with", "Slytherin is quite close to my heart."]),
    (["Hufflepuff"], ["Hufflepuff... not much to say about it...", "Hufflepuff? That is...interesting", "Hufflepuff loves to puff the huffle"]),
    (["Gryffindor"], ["Gryffindor... courage and friendship is not my style...", "Gryffindor is not to be trifled with", "Gryffindor and Slytherin are not so different you know..."]),
    (["Ravenclaw"], ["Ravenclaw... clever little thing...", "Ravenclaw...very smart, but smart enough?", "Between you and me I almost was sorted into Ravenclaw"]),
    (["Avada Kedavra", "Expelliarmus", "Lumos", "Winguardium Leviosa", "Petrificus Totalus", "Crucio", "Imperius"], ["Sounds like some serious bit of magic", "Aren't you forgetting your wand?", "Woah, there!"]),
    (["Voldemort"], ["My reputation precedes me...", "Do you think I would keep my filthy mudblood name?"]),
    (["Butterbeer"], ["How refreshing!", "I just love butterbeer!"])
    ]
    
nonSequitur :: [RawIRPair]
nonSequitur = [
  (["([a-zA-Z ]*)"], [ "I'm not sure I understand you fully.",
		 "What does that suggest to you?",
		 "I see.",
		 "Can you elaborate on that?",
		 "Say, do you have any magical problems?",
         "Could you repeat that on parsel tongue please?",
         "Could we have a more intimate conversation? I need to grow stronger.",
         "Say...Have you heard of a certain Lord Voldemort?"])]
         
friend :: [RawIRPair]
friend = [(["friend"]), ["Why do you bring up the topic of friends?",
		 "Do your friends worry you?",
		 "Do your friends pick on you?",
		 "Are you sure you have any friends?",
		 "Do you impose on your friends?",
		 "Perhaps your love for friends worries you."]
]

         
alike :: [RawIRPair]
alike = [(["alike"]), ["In what way?",
		 "What resemblence do you see?",
		 "What does the similarity suggest to you?",
		 "What other connections do you see?",
		 "Cound there really be some connection?",
		 "How?"]
]


-- Response data --------------------------------------------------------------


