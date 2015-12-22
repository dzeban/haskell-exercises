--------------------------------------------------------------------------------
-- | Todo - simple and strange todo manager
--
-- Todo allows you to work with your todos. You can add a todo, and it will be
-- assigned a unique 3-letters id, which you can later use later to delete todo.
-- You can also ask todo to show your todos.
--
-- All todos are stored in a file "todos.txt".
--
-- Copyright (c) 2015 Alex Dzyoba <alex@dzyoba.com>
-- License: MIT
--------------------------------------------------------------------------------
import System.IO
import System.Directory
import System.Environment
import System.Random
import Control.Monad

todoFile = "todos.txt"
tempFile = "temp_todos.txt"
todoIdLen = 3

main = do
    args <- getArgs
    when (some args) $ do
        let command = head args
            params  = tail args

        status <- case command of
                  "add"  -> addTodo params
                  "del"  -> delTodo params
                  "show" -> showTodo
                  []     -> usage
                  _      -> usage
        return ()

-- | Add a new todo
addTodo :: [String] -> IO ()
addTodo strings =
    when (some strings) $ do
        -- We don't care about reseeding random generator,
        -- because next time we'll recreate it
        g <- newStdGen
        let todoId = take todoIdLen (randomRs ('a', 'z') g)
        appendFile todoFile (makeTodo todoId strings)

-- | Delete a todo
--
-- This will create temp file, copying all lines except the one
-- to delete into it, and then replace todo file with temp file
delTodo :: [String] -> IO ()
delTodo strings =
    when (some strings) $ do
        let todoId = head strings
        handle <- openFile todoFile ReadMode
        content <- hGetContents handle

        -- Create new todos unfiltering task to remove
        let newContent = unlines (removeTodoById todoId (lines content))

        -- Write new content to temp file
        temphandle <- openFile tempFile WriteMode
        hPutStr temphandle newContent

        hClose handle
        hClose temphandle

        -- Replace todo with temp file
        removeFile todoFile
        renameFile tempFile todoFile

-- | Remove a todo by id from a list
removeTodoById :: String -> [String] -> [String]
removeTodoById todoId content = filter (not . matchTodo todoId) content

-- | Predicate to match todo with id extracting
matchTodo :: String -> String -> Bool
matchTodo idToMatch todo = idToMatch == todoId
                           where todoId = fst $ splitAt todoIdLen todo

-- | Construct todo string prepending id and delimiter
makeTodo :: String -> [String] -> String
makeTodo todoId todo = prefix ++ delim ++ message
        where prefix = todoId
              delim  = ": "
              message = unwords todo ++ "\n"

-- | List todos
showTodo :: IO ()
showTodo = do
    content <- readFile todoFile
    putStr content

-- | Show usage
usage :: IO ()
usage = print "add <todo> | del <index> | show"

-- | Little helper function, invert of "null" to use with "when"
some :: [a] -> Bool
some = not . null
