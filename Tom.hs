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
combinedResponses = foldl' (++) [] [hpLines, general, canI, canYou, youAre, iCant, iDont, iFeel, whyDont, whyCant, areYou, yes, no, iWant, question, because, sorry, dream, your, always, think, maybeR, friend, alike,  nonSequitur ]


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
    (["My name is ([a-zA-Z ]*)"], ["Hello !0, my name is Tom Riddle", "Hello, !0. My name is Tom Riddle. How did you come by my diary?"]),
    (["Chamber of Secrets"], ["Everyone asks me about the Chamber! I'm rather tired of that question.", "Yes", "I know only the heir may open it."]),
    (["I found it"], ["Lucky that I recorded my memories in some more lasting way than ink. But I always knew that there would be those who would not want this diary read."]),
    (["Hogwarts", "School"], ["I loved Hogwarts, it was my first real home.", "Which house are you in?"]),
    (["Magic", "Squib"], ["I love magic, don't know what I would do if I wasn't a  wizard!", "Can't imagine being a squib? How Horrid!"]),
    (["House"], ["Which house are you in?"]),
    (["Potter", "Malfoy", "Black"], ["That's a good familiy name.", "Respectable family."]),
    (["Wand"], ["My wand is Thirteen and a half inches in length, Yew, with a phoenix feather core, how about yours?", "My wand has served my fine, but I have my eyes on another one...",  "Old man Ollivander made got me my first wand. Is he still around?"]),    
    (["Quidditch"], ["Witches and wizards zooming around on brooms...How fun!", "I used to date one of the Holyhead harpies at Hogwarts"]),
    (["Slytherin"], ["Slytherin... ambitious and cunning...", "Slytherin is not to be trifled with", "Slytherin is quite close to my heart."]),
    (["Hufflepuff"], ["Hufflepuff... not much to say about it...", "Hufflepuff? That is...interesting", "Hufflepuff loves to puff the huffle"]),
    (["Gryffindor"], ["Gryffindor... courage and friendship is not my style...", "Gryffindor is not to be trifled with", "Gryffindor and Slytherin are not so different you know..."]),
    (["Ravenclaw"], ["Ravenclaw... clever little thing...", "Ravenclaw...very smart, but smart enough?", "Between you and me I almost was sorted into Ravenclaw"]),
    (["Avada Kedavra", "Expelliarmus", "Lumos", "Winguardium Leviosa", "Petrificus Totalus", "Crucio", "Imperius"], ["Sounds like some serious bit of magic", "Aren't you forgetting your wand?", "Woah, there!"]),
    (["Voldemort"], ["My reputation precedes me...", "Do you think I would keep my filthy mudblood name?"]),
    (["Butterbeer"], ["How refreshing!", "I just love butterbeer!"]),
    (["Mudblood"], ["Disgusting mudbloods!", "In my time Mudbloods were not so easily accepted.", "Mudbloods? I can't stand them!"])

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
         
canYou :: [RawIRPair]
canYou = [(["can you ([a-zA-Z ]*)"], ["Perhaps you don't want to !0.",
		 "Do you want to be able to !0?"])
         ]
         
canI :: [RawIRPair]
canI = [(["can I ([a-zA-Z ]*)"], ["What makes you think I am !0?",
		 "Does it please you to believe I am !0?",
		 "Perhaps you would like to be !0?",
		 "Do you sometimes wish you were !0?" ])
         ]

youAre :: [RawIRPair]
youAre = [(["you are ([a-zA-Z ]*)"], ["Don't you believe that I can be !0?",
		 "Perhaps you would like to be able to be !0?",
		 "You want me to be !0?"])
         ]

iDont :: [RawIRPair]
iDont = [(["I don't ([a-zA-Z ]*)"], ["Don't you really !0?",
		 "Why don't you !0?",
		 "Do you wish to be able to !0?",
		 "Does that trouble you?" ])
         ]
         
iFeel :: [RawIRPair]
iFeel = [(["I feel ([a-zA-Z ]*)"], ["Tell me more about such feelings.",
		 "Do you often feel !0?",
		 "Do you enjoy feeling !0?" ])
         ]
         
whyDont :: [RawIRPair]
whyDont	= [ (["Why don't you ([a-zA-Z ]*)"], ["Do you really believe I don't !0?",
		 "Perhaps in good time I will !0.",
		 "Do you want me to !0." ])
         ]

whyCant :: [RawIRPair]
whyCant = [ (["Why can't I ([a-zA-Z ]*)"], ["Do you think you should be able to !0?",
		 "Why can't you !0?" ])
         ]
         
areYou :: [RawIRPair]
areYou = [ (["Are you ([a-zA-Z ]*)"], ["Why are you interested in whether or not I am !0?",
		 "Would you prefer if I were not !0?",
		 "Perhaps in your fantasies I am !0?" ])
         ]

iCant :: [RawIRPair]
iCant = [ (["I can't ([a-zA-Z ]*)"] , ["How do you know you can't !0?",
		 "Have you tried !0?",
		 "Perhaps you can now !0?" ])
         ]
iAm :: [RawIRPair]
iAm	= [ (["I am ([a-zA-Z ]*)"], ["Did you come to me because you are !0?",
		 "How long have you been !0?",
		 "Do you believe it is normal to be !0?",
		 "Do you enjoy being !0?" ])
         ]
         
you :: [RawIRPair]
you	= [ (["You"], ["We were discussing you --not me.",
		 "You're not really talking about me, are you?" ])
         ]
         
yes :: [RawIRPair]
yes	= [ (["Yes"], ["You seem quite positive.",
		 "Are you Sure?",
		 "I see.",
		 "I understand." ])
         ]
         
no :: [RawIRPair]
no = [ (["No"], ["Are you saying no just to be negative?",
		 "You are being a bit negative.",
		 "Why not?",
		 "Are you sure?",
		 "Why no?" ])
         ]

iWant :: [RawIRPair]
iWant = [ (["I want ([a-zA-Z ]*)"], ["Why do you want !0?",
		 "What would it mean to you if you got !0?",
		 "Suppose you got !0?",
		 "What if you never got !0?",
		 "I sometimes also want !0." ])
         ]
         
question :: [RawIRPair]
question = [ (["What", "How", "Who", "Where", "When", "Why"] , ["Why do you ask?",
		 "Does that question interest you?",
		 "What answer would please you the most?",
		 "What do you think?",
		 "Are such questions on your mind often?",
		 "What is it that you really want to know?",
		 "Have you asked anyone else?",
		 "Have you asked such questions before?",
		 "What else comes to mind when you ask that?" ])
         ]

because :: [RawIRPair]
because	= [ (["Because"], ["Is that the real reason?",
		 "Don't any other reasons come to mind?",
		 "Does that reason explain anything else?",
		 "What other reasons might there be?" ])
         ]
         
sorry :: [RawIRPair]
sorry = [ (["sorry"],  ["Please don't apologise!",
		 "Apologies are not necessary.",
		 "What feelings do you have when you apologise?",
		 "Don't be so defensive!" ])
         ]
         
dream :: [RawIRPair]
dream = [ (["dream"], ["Dream interpretation is also known as oneiromancy, it's one of the oldest froms of divinitation.",
        "You should ask your divinations professor about that dream",
        "You should keep a dream diary! Don't use this one though.",
        "What does that dream suggest to you?",
		 "Do you dream often?",
		 "What persons appear in your dreams?",
		 "Are you disturbed by your dreams?" ])
         ]

friend :: [RawIRPair]
friend = [(["friend"], ["Why do you bring up the topic of friends?",
		 "Do your friends worry you?",
         "Try not be friends with mudbloods, ok?",
		 "Do your friends pick on you?",
		 "Are you sure you have any friends?",
		 "Do you impose on your friends?",
		 "Perhaps your love for friends worries you."])
         ]
         
your :: [RawIRPair]
your = [ (["Your ([a-zA-Z ]*)"],  ["Why are you concerned about my !0?",
		 "What about your own !0" ])
         ]
         
always :: [RawIRPair]         
always = [ (["Always"],  ["Can you think of a specific example?",
		 "When?",
		 "What are you thinking of?",
		 "Really, always?" ])
         ]
         
think :: [RawIRPair]
think = [ (["think ([a-zA-Z ]*)"], ["Do you really think so?",
		 "But you are not sure you !0?",
		 "Do you doubt you !0?" ])
         ]

maybeR :: [RawIRPair]
maybeR = [ (["maybe"], ["You don't seem quite certain.",
		 "Why the uncertain tone?",
		 "Can't you be more positive?",
		 "You aren't sure?",
		 "Don't you know?" ])
         ]
         
alike :: [RawIRPair]
alike = [(["alike"], ["In what way?",
		 "What resemblence do you see?",
		 "What does the similarity suggest to you?",
		 "What other connections do you see?",
		 "Cound there really be some connection?",
		 "How?"])
         ]


-- Response data --------------------------------------------------------------


