{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import           Log


parseMessage :: String -> LogMessage
parseMessage s =
   case words s of
     ("I":timestamp:msg) -> LogMessage Info (read timestamp) (unwords msg)
     ("W":timestamp:msg) -> LogMessage Warning (read timestamp) (unwords msg)
     ("E":lvl:timestamp:msg) -> LogMessage (Error (read lvl)) (read timestamp) (unwords msg)
     unknown -> Unknown (unwords unknown)

parse :: String -> [LogMessage]
parse s = fmap parseMessage (lines s)


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert msg Leaf      = Node Leaf msg Leaf
insert msg@(LogMessage _ timestamp _) (Node left node@(LogMessage _ nodeTimestamp _) right)
  | timestamp < nodeTimestamp = Node (insert msg left) node right
  | otherwise = Node left node (insert msg right)
insert _ t = t                  -- otherwise a warning shows up

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left node right) = inOrder left ++
                                 [node] ++
                                 inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs =
  let sortedMsgs = inOrder (build msgs)
  in (fmap getMsg) . (filter isErrorMsg) $ sortedMsgs
  where isErrorMsg (LogMessage (Error n) _ _) = n >= 50
        isErrorMsg _                          = False
        getMsg (LogMessage _ _ msg) = msg
        getMsg (Unknown msg)        = msg
