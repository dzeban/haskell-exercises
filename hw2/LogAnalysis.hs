{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- LogMessage builder
createLogMessage :: MessageType -> String -> [String] -> LogMessage
createLogMessage mt t l = LogMessage mt (read t) (unwords l)

-- Get severity from LogMessage
severity :: LogMessage -> Int
severity (LogMessage (Error int) _ _) = int
severity _ = -1

-- Get timestamp from LogMessage
timestamp :: LogMessage -> TimeStamp
timestamp (LogMessage _ ts _) = ts
timestamp _ = error "Invalid message"

-- Get log string from LogMessage
logString :: LogMessage -> String
logString (LogMessage _ _ m) = m
logString _ = error "Invalid message"

-- Convert list of log messages to list of strings
toStrings :: [LogMessage] -> [String]
toStrings (m:ms) = logString m : toStrings ms
toStrings [] = []

-- Check if LogMessage is an Error message
isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _ = False


-- Given a log string construct LogMessage
parseMessage :: String -> LogMessage
parseMessage s = case words s of 
    ("E":errType:timeStamp:message) -> createLogMessage (Error (read errType)) timeStamp message
    ("W":timeStamp:message)         -> createLogMessage Warning timeStamp message
    ("I":timeStamp:message)         -> createLogMessage Info timeStamp message
    list -> Unknown (unwords list)

-- Parse log from string
parse :: String -> [LogMessage]
parse s = case lines s of
    (m:ms) -> (parseMessage m) : (parse $ unlines $ ms)
    []     -> []

-- Insert into LogMessage BST
insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node left val right)
    | timestamp msg <= timestamp val = Node (insert msg left) val right
    | timestamp msg >  timestamp val = Node left val (insert msg right)
-- Insert of unknown message must return tree unchanged
insert (Unknown _) tree = tree
-- XXX: This makes GHC -Wall option happy.
insert _ _ = error "Something went wrong"

-- Build BST from list of LogMessages
build :: [LogMessage] -> MessageTree
build (msg:msgs) = insert msg (build msgs)
build [] = Leaf

-- Tree in-order traversal
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)

-- LogAnalysis function that returns Error messages with severity > 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong l = toStrings $ inOrder $ build $ filter (\m -> severity m > 50 && isError m) l
