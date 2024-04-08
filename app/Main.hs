{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Monad (void, filterM)
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
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import Brick.BChan
import Brick.Types(Widget, ViewportType(Horizontal, Vertical, Both))
import Brick.AttrMap (attrMap)
import Brick.Widgets.Core (hLimit, vLimit, hBox, vBox, viewport, str, fill, strWrapWith, strWrap)

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
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
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

strip = filter (not . isSpace)

stripLeadingSpaces :: String -> String
stripLeadingSpaces [] = []
stripLeadingSpaces (h : t)
  |isSpace h = stripLeadingSpaces t
  |otherwise = h:t

stripEndingSpaces :: String -> String
stripEndingSpaces = reverse . stripLeadingSpaces . reverse

stringToInt :: String -> Int
stringToInt s = sum [x * (round (10**(fromIntegral y - 1))) | (x, y) <- zip (fmap digitToInt s) (reverse [1..(length s)])]

tuplify :: [a] -> (a,a,a,a)
tuplify [a,b,c,d] = (a,b,c,d)

intTupleToWord8Tuple :: (Int, Int, Int, Int) -> (Word8, Word8, Word8, Word8)
intTupleToWord8Tuple (a, b, c, d) = (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d)

allInRange :: [Int] -> Bool
allInRange l = foldr (&&) True $ fmap (\x -> (x >= 0) && (x <= 255)) l

parseIp :: String -> Maybe (Int, Int, Int, Int)
parseIp s
  |(length l == 4) && allInRange l = Just $ tuplify $ l
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
  forkIO $ clientConnection eventChannel handle
  -- start the client interface
  let buildVty = Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
  initialVty <- buildVty
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
  forkIO $ accepter sock channel handlesMV
  -- start the message listener
  forkIO $ messageListener channel handlesMV
  -- create the client socket
  (SockAddrInet port _) <- getSocketName sock
  clientSock <- socket AF_INET Stream 0
  connect clientSock (SockAddrInet port $ tupleToHostAddress (127, 0, 0, 1))
  handle <- socketToHandle clientSock ReadWriteMode
  let initialState = buildState name handle
  -- start client connection manager thread
  eventChannel <- Brick.BChan.newBChan 5
  Brick.BChan.writeBChan eventChannel $ MRE $ "Server opened on port " ++ show port ++ ".\n"
  forkIO $ clientConnection eventChannel handle
  -- start the client
  let buildVty = Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
  initialVty <- buildVty
  void $ M.customMain initialVty buildVty (Just eventChannel) app initialState

handleCleanUp :: [Handle] -> IO [Handle]
handleCleanUp ls = filterM hIsOpen ls

accepter :: Socket -> Chan String -> MVar [Handle] -> IO ()
accepter sock channel handlesMV = do
  (clientSock, clientSockAddr) <- accept sock
  newHandle <- socketToHandle clientSock ReadWriteMode
  hSetBuffering newHandle NoBuffering
  oldHandles <- takeMVar handlesMV
  putMVar handlesMV $ (newHandle : oldHandles)
  forkIO $ clientThread newHandle channel
  accepter sock channel handlesMV

sendToHandles :: String -> [Handle] -> IO ()
sendToHandles message [] = do
  return ()
sendToHandles message (h:t) = do
  restult <- catch (fmap Just $ hPutStrLn h message) (\(_ :: SomeException) -> return Nothing)
  sendToHandles message t

messageListener :: Chan String -> MVar [Handle] -> IO ()
messageListener channel handlesMV = do
  message <- readChan channel
  handles <- readMVar handlesMV
  sendToHandles message $ handles
  messageListener channel handlesMV

clientThread :: Handle -> Chan String -> IO ()
clientThread handle channel = do
  result <- catch (fmap Just $ hGetLine handle) (\(_ :: SomeException) -> return Nothing)
  case result of
    Just msg -> do
      writeChan channel msg
      clientThread handle channel
    Nothing -> do
      return ()

-- Main

main :: IO ()
main = do
  -- void $ M.defaultMain app initialState
  name <- getName
  input <- getOption
  if input == "h" then
    server name
  else
    client name