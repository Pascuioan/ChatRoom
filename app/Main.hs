{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Monad (void)
import qualified Graphics.Vty as V
import Graphics.Vty.Config
import Graphics.Vty.CrossPlatform

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import Data.Char
import Data.Text.Zipper
import GHC.Utils.Misc
import GHC.Word
import Text.Wrap (defaultWrapSettings, preserveIndentation)
import System.IO
import Control.Concurrent
import Network.Socket
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, SomeException)

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import Brick.BChan
import Brick.Types(ViewportType(Vertical))
import Brick.AttrMap (attrMap)
import Brick.Widgets.Core (vLimit, vBox, viewport, str, strWrapWith)

-- Brick functions and data

data Name = CHAT | INPUT
  deriving (Ord, Show, Eq)

data State =
    State { _editor :: E.Editor String Name,
            _chat :: String,
            _username :: String,
            _handle :: Handle}
makeLenses ''State

data MessageRevEvent = MRE String

vp :: M.ViewportScroll Name
vp = M.viewportScroll CHAT

buildState :: String -> Handle -> State
buildState username handle = 
  State {
    _chat = "",
    _editor = E.editor INPUT Nothing "",
    _username = username,
    _handle = handle
  }

drawUi :: State -> [T.Widget Name]
drawUi st = [ui]
  where
    ui = B.border $ vBox [chat, B.hBorder, input]
    chat = viewport CHAT Vertical (strWrapWith settings $ _chat st)
    settings = defaultWrapSettings { preserveIndentation = True }
    input = vLimit 2 $ E.renderEditor (str . unlines) True (st^.editor)
        
appEvent :: T.BrickEvent Name MessageRevEvent -> T.EventM Name State ()
appEvent (T.VtyEvent (V.EvKey V.KEnter [])) = do
  e <- use editor
  let currentInput = E.getEditContents e !! 0
  name <- use username
  clientHandle <- use handle
  if strip currentInput /= "" then
    liftIO $ hPutStrLn clientHandle (name ++ ": " ++ currentInput)
  else
    return ()
  clearEditor
  M.vScrollToEnd vp
appEvent (T.AppEvent (MRE msg)) = do
  addChatLine msg
  M.vScrollToEnd vp
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = do
  h <- use handle
  name <- use username
  liftIO $ hPutStrLn h (name ++ " disconnected.")
  M.halt
appEvent (T.VtyEvent (V.EvKey V.KDown  [V.MCtrl])) = M.vScrollBy vp 1
appEvent (T.VtyEvent (V.EvKey V.KUp  [V.MCtrl])) = M.vScrollBy vp (-1)
appEvent ev = do
  zoom editor $ E.handleEditorEvent ev

clearEditor :: T.EventM Name State ()
clearEditor = do 
  editor %= (E.applyEdit clearZipper)

addChatLine :: String -> T.EventM Name State ()
addChatLine s = do
  if strip s /= "" then
    chat %= (++ s ++ "\n")
  else
    return ()

app :: M.App State MessageRevEvent Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return ()
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = const $ M.showCursorNamed INPUT
          }

-- User input functions

strip :: [Char] -> [Char]
strip = filter (not . isSpace)

stripLeadingSpaces :: String -> String
stripLeadingSpaces [] = []
stripLeadingSpaces (h : t)
  |isSpace h = stripLeadingSpaces t
  |otherwise = h:t

stripEndingSpaces :: String -> String
stripEndingSpaces = reverse . stripLeadingSpaces . reverse

stringToInt :: String -> Int
stringToInt s = sum [x * (round (10**(fromIntegral y - 1)) :: Int) | (x, y) <- zip (fmap digitToInt s) (reverse [1..(length s)])]

tuplify :: [a] -> (a,a,a,a)
tuplify [a,b,c,d] = (a,b,c,d)
tuplify _ = error "This error will never be thrown; it will forever live as a shadow of what it could have become. \
                  \Perhaps the non-exhastive patterns gods intended for the error's greatest possesion to be the hope\
                  \it holds that one day it shall make itself useful by helping the programmer write better code."

intTupleToWord8Tuple :: (Int, Int, Int, Int) -> (Word8, Word8, Word8, Word8)
intTupleToWord8Tuple (a, b, c, d) = (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d)

allInRange :: [Int] -> Bool
allInRange l = foldr (&&) True $ fmap (\x -> (x >= 0) && (x <= 255)) l

parseIp :: String -> Maybe (Int, Int, Int, Int)
parseIp s
  |(length l == 4) && allInRange l = Just $ tuplify l
  |otherwise = Nothing
  where l = map stringToInt $ filter (\x -> x /= "") $ split '.' $ filter (\x -> isDigit x || x == '.') s

validatePort :: String -> Maybe Int
validatePort s
  |(&&) (s /= "") $ foldr (&&) True $ fmap isDigit s = Just $ stringToInt s
  |otherwise = Nothing

getName :: IO String
getName = do
  putStr "Hello! What is your name?\n>>"
  hFlush stdout
  input <- getLine
  if strip input == "" then
    getName
  else
    return $ stripLeadingSpaces $ stripEndingSpaces input

getIp :: IO (Maybe (Int, Int, Int, Int))
getIp = do
  putStr "IP (c to cancel):\n>>"
  hFlush stdout
  input <- fmap strip getLine
  if input == "c" then
    return Nothing
  else do
    let maybeIp = parseIp input
    case maybeIp of
      Nothing -> getIp
      Just ip -> return $ Just ip

getPort :: IO Int
getPort = do
  putStr "Port:\n>>"
  hFlush stdout
  input <- fmap strip getLine
  let maybePort = validatePort input
  case maybePort of
    Nothing -> getPort
    Just port -> return port

getOption :: IO String
getOption = do
  putStr "Would you like to (h)ost a server or (j)oin one?\n>>"
  hFlush stdout
  input <- getLine
  if (strip input == "h") || (strip input == "j") then
    return $ strip input
  else
    getOption

-- Client code

client :: String -> IO ()
client name = do
  (Just ip) <- getIp
  port <- getPort
  sock <- socket AF_INET Stream 0
  connect sock (SockAddrInet (fromIntegral port) $ tupleToHostAddress $ intTupleToWord8Tuple ip)
  handle <- socketToHandle sock ReadWriteMode
  let initialState = buildState name handle
  -- create a thread for comunicating with the server
  eventChannel <- Brick.BChan.newBChan 5
  Brick.BChan.writeBChan eventChannel $ MRE $ "Use ':con' to see current connections."
  _ <- forkIO $ clientConnection eventChannel handle
  -- start the client interface
  let buildVty = Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
  initialVty <- buildVty
  hPutStrLn handle $ name ++ " joined."
  void $ M.customMain initialVty buildVty (Just eventChannel) app initialState

clientConnection :: Brick.BChan.BChan MessageRevEvent -> Handle -> IO ()
clientConnection channel handle = do
  result <- catch (fmap Just $ hGetLine handle) (\(_ :: SomeException) -> return Nothing)
  case result of
    Just msg -> do
      Brick.BChan.writeBChan channel $ MRE msg
      clientConnection channel handle
    Nothing -> do
      Brick.BChan.writeBChan channel $ MRE "Server closed."

-- Server code

server :: String -> IO ()
server name = do
  -- create listening socket
  sock <- socket AF_INET Stream 0
  bind sock (SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1))
  listen sock 5
  -- start accepting connections in a new thread
  channel <- newChan -- message channel shared by the messageSender and each client thread
  handlesMV <- newMVar [] -- share variable to hold the different handles for each of the clients
  _ <- forkIO $ accepter sock channel handlesMV
  -- start the message listener
  _ <- forkIO $ messageListener channel handlesMV
  -- create the client socket
  (SockAddrInet port _) <- getSocketName sock
  clientSock <- socket AF_INET Stream 0
  connect clientSock (SockAddrInet port $ tupleToHostAddress (127, 0, 0, 1))
  handle <- socketToHandle clientSock ReadWriteMode
  let initialState = buildState name handle
  -- start client connection manager thread
  eventChannel <- Brick.BChan.newBChan 5
  Brick.BChan.writeBChan eventChannel $ MRE $ "Server opened on port " ++ show port ++ "."
  Brick.BChan.writeBChan eventChannel $ MRE $ "Use ':con' to see current connections."
  hPutStrLn handle $ name ++ " joined."
  _ <- forkIO $ clientConnection eventChannel handle
  -- start the client
  let buildVty = Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
  initialVty <- buildVty
  void $ M.customMain initialVty buildVty (Just eventChannel) app initialState

accepter :: Socket -> Chan String -> MVar [(Handle, String)] -> IO ()
accepter sock channel handlesMV = do
  (clientSock, _) <- accept sock
  newHandle <- socketToHandle clientSock ReadWriteMode
  hSetBuffering newHandle NoBuffering
  -- read the first message from the client (containing the name)
  result <- catch (fmap Just $ hGetLine newHandle) (\(_ :: SomeException) -> return Nothing)
  -- check the result

  case result of
    Just msg -> do
      writeChan channel msg
      let name = reverse $ drop 8 $ reverse msg -- take just the name
      -- modify the MVar
      oldHandles <- takeMVar handlesMV
      putMVar handlesMV $ ((newHandle, name) : oldHandles) 
      -- create a new thread for managing the connection with the client
      _ <- forkIO $ clientThread newHandle channel handlesMV
      -- continue accepting connections
      accepter sock channel handlesMV
    Nothing -> do -- if the client suddenly closes the connetion, just contiune accepting connections
      accepter sock channel handlesMV
  

sendToHandles :: String -> [(Handle, String)] -> IO [Maybe ()]
sendToHandles message handles = mapM sendAndCatch $ map (\(h, _) -> h) handles
  where
    sendAndCatch :: Handle -> IO (Maybe ())
    sendAndCatch handle = catch (sendMessage handle >> return (Just ())) (\(_ :: SomeException) -> return Nothing)

    sendMessage :: Handle -> IO ()
    sendMessage handle = hPutStrLn handle message

messageListener :: Chan String -> MVar [(Handle, String)] -> IO ()
messageListener channel handlesMV = do
  message <- readChan channel
  handles <- takeMVar handlesMV -- take out the handles and names
  answers <- sendToHandles message $ handles -- send message and validate handles
  let validHandles = filterHandles handles answers -- select valid handles based on answers
  putMVar handlesMV validHandles -- put back the valid handles
  -- let invalidHandles = [x | x <- handles , notElem x validHandles] -- extract the invalidated handles
  -- let noConn = length validHandles
  -- writeDisconnectNotice noConn invalidHandles -- add disconnected notice to message queue
  messageListener channel handlesMV -- start listening for messages again
    where
      filterHandles :: [(Handle, String)] -> [Maybe ()] -> [(Handle, String)]
      filterHandles handles res = map (\(x, _) -> x) $ filter (\(_, y) -> y /= Nothing) $ zip handles res
      writeDisconnectNotice :: Int -> [(Handle, String)] -> IO [()]
      writeDisconnectNotice noConn l = mapM (\(x, y) -> hClose x >> (writeChan channel $ y ++ "'s connection has been removed from the memory (" ++ show noConn ++ " remaining).")) l

clientThread :: Handle -> Chan String -> MVar [(Handle, String)] -> IO ()
clientThread handle channel handlesMV = do
  result <- catch (fmap Just $ hGetLine handle) (\(_ :: SomeException) -> return Nothing)
  case result of
    Just msg -> do
      if isCon msg then
        do 
          conStr <- formatCon handlesMV
          _ <- catch (fmap Just $ hPutStrLn handle conStr) (\(_ :: SomeException) -> return Nothing)
          clientThread handle channel handlesMV
      else do
        writeChan channel msg
        clientThread handle channel handlesMV
    Nothing -> do
      return ()
  where 
    isCon :: String -> Bool
    isCon (a:b:c:d:s)
      |a:b:c:d:"" == ":con" = True
      |otherwise = isCon $ b:c:d:s
    isCon _ = False
    formatCon :: MVar [(Handle, String)] -> IO String
    formatCon mv = do
      handles <- readMVar mv
      let names = map (\(_, s) -> s) handles
      return $ "Currently connected: " ++ (foldl (\x y -> x ++ ", " ++ y) (names !! 0) $ drop 1 names) ++ "."

-- Main

main :: IO ()
main = do
  name <- getName
  input <- getOption
  if input == "h" then
    server name
  else
    client name