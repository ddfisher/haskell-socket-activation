{-# LANGUAGE ForeignFunctionInterface #-}
-- | This is a module for systemd socket activation.  See
-- <http://0pointer.de/blog/projects/socket-activation.html> and
-- <http://www.freedesktop.org/software/systemd/man/systemd.socket.html>
module Network.Socket.Activation
    (
    -- * Getting all activated sockets
      getActivatedSockets

    -- * Getting a fixed numbers of sockets
    -- $fixed
    , getActivatedSockets1
    , getActivatedSockets2

    ) where

import Control.Applicative
import Control.Monad
import Control.Monad
import Network.Socket
import System.Posix.Process
import System.Posix.Env
import System.Posix.Types
import Foreign.C.Types(CInt(..))

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

fdStart :: CInt
fdStart = 3

-- | Return a list of activated sockets, if the program was started with
-- socket activation.  The sockets are in the same order as in
-- the associated @.socket@ file.  The sockets will have their family, type,
-- and status set appropriately.  Returns @Nothing@ in systems without
-- socket activation (or when the program was not socket activated).
--
-- If your program requires being started with some small fixed number of
-- sockets, you may want to use 'getActivatedSockets1' or
-- 'getActivatedSockets2'.
--
-- === Example
--
-- This program prints whether it was started with socket activation
-- and, if it was, how many activated sockets it has been given.
--
-- > import Network.Socket.Activation (getActivatedSockets)
-- >
-- > main :: IO ()
-- > main = do
-- >
-- >     socketsMaybe <- getActivatedSockets
-- >
-- >     case socketsMaybe of
-- >         Nothing ->
-- >             putStrLn "This program is not socket activated."
-- >         Just sockets ->
-- >             putStrLn ("This program was activated with " ++
-- >                       show (length sockets) ++ " socket(s).")
getActivatedSockets :: IO (Maybe [Socket])
getActivatedSockets = runMaybeT $ do
    listenPid <- read <$> MaybeT (getEnv "LISTEN_PID")
    listenFDs <- read <$> MaybeT (getEnv "LISTEN_FDS")
    myPid     <- lift getProcessID
    guard $ listenPid == myPid
    mapM makeSocket [fdStart .. fdStart + listenFDs - 1]
  where makeSocket :: CInt -> MaybeT IO Socket
        makeSocket fd = do
          fam  <- socketFamily fd
          typ  <- socketType fd
          stat <- socketStatus fd
          lift $ mkSocket fd fam typ defaultProtocol stat

-- $fixed
-- Often a program using socket activation will require being activated
-- some fixed number of sockets.  Here we provide some actions to make it
-- convenient to get the number of activated sockets your program expects
-- or otherwise fail with an informative error message.

-- | Return an activated socket, if the program was started with exactly
-- one activated socket.  The socket will have its family, type, and
-- status set appropriately.  Returns @Left@ containing an error message
-- if the program was not started with exactly one activated socket.
--
-- The more general version of this for any number of sockets is
-- 'getActivatedSockets'.  If you need two sockets instead of one, use
-- 'getActivatedSockets2'.
--
-- === Example
--
-- This example illustrates a common case where we have a program that
-- requires being started with one activated socket. In the first line
-- of @main@, we attempt to obtain the socket. In the case where the
-- program was not started with exactly one activated socket, we use
-- the @die@ function to print an error message and quit the program.
--
-- > import Network.Socket.Activation (getActivatedSockets1)
-- > import System.Exit (die)
-- >
-- > main :: IO ()
-- > main = do
-- >     socket <- getActivatedSockets1 >>= either die pure
-- >     ...
getActivatedSockets1 :: IO (Either String Socket)
getActivatedSockets1 = do
    socketsMaybe <- getActivatedSockets
    return $ case socketsMaybe of
        Nothing -> Left "This program is not socket activated."
        Just xs -> case xs of
            []  -> Left "This program is socket activated, but \
                        \there are no sockets. Exactly one is required."
            [x] -> Right x
            _   -> Left $ "There are too many activated sockets; \
                          \expected 1, got " ++ show (length xs)

-- | Return two activated sockets, if the program was started with exactly
-- two activated sockets.  The sockets are in the same order as in
-- the associated @.socket@ file.  The sockets will have their family, type,
-- and status set appropriately.  Returns @Left@ containing an error message
-- if the program was not started with exactly two activated sockets.
--
-- The more general version of this for any number of sockets is
-- 'getActivatedSockets'.  If you need one socket instead of two, use
-- 'getActivatedSockets1'.
--
-- === Example
--
-- This example illustrates a common case where we have a program that
-- requires being started with two activated sockets. In the first line
-- of @main@, we attempt to obtain the sockets. In the case where the
-- program was not started with exactly two activated sockets, we use
-- the @die@ function to print an error message and quit the program.
--
-- > import Network.Socket.Activation (getActivatedSockets2)
-- > import System.Exit (die)
-- >
-- > main :: IO ()
-- > main = do
-- >     (socket1, socket2) <- getActivatedSockets2 >>= either die pure
-- >     ...
getActivatedSockets2 :: IO (Either String (Socket, Socket))
getActivatedSockets2 = do
    socketsMaybe <- getActivatedSockets
    return $ case socketsMaybe of
        Nothing -> Left "This program is not socket activated."
        Just xs -> case xs of
            []     -> Left "This program is socket activated, but \
                           \there are no sockets. Exactly two are required."
            [_]    -> Left "This program was started with only one activated \
                           \socket. Exactly two are required."
            [x, y] -> Right (x, y)
            _      -> Left $ "There are too many activated sockets; \
                             \expected 2, got " ++ show (length xs)

socketFamily :: CInt -> MaybeT IO Family
socketFamily fd = do
    familyInt <- lift $ c_socket_family fd
    guard $ familyInt >= 0
    return $ unpackFamily familyInt

socketType :: CInt -> MaybeT IO SocketType
socketType fd = do
    typeInt <- lift $ c_socket_type fd
    case typeInt of
        0 -> return NoSocketType
        1 -> return Stream
        2 -> return Datagram
        3 -> return Raw
        4 -> return RDM
        5 -> return SeqPacket
        _ -> mzero

socketStatus :: CInt -> MaybeT IO SocketStatus
socketStatus fd = do
    listeningInt <- lift $ c_socket_listening fd
    case listeningInt of
      0 -> return Bound
      x | x > 0 -> return Listening
      _ -> mzero

foreign import ccall unsafe "socket_family"
  c_socket_family :: CInt -> IO CInt

foreign import ccall unsafe "socket_type"
  c_socket_type :: CInt -> IO CInt

foreign import ccall unsafe "socket_listening"
  c_socket_listening :: CInt -> IO CInt
