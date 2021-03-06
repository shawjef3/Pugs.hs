{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
    POSIX calls and emulations.

>   And now all those lands lie under the wave.
>   And I walk in Ambarona, in Tauremorna, in Aldalome.
>   In my own land, in the country of Fangorn,
>   Where the roots are long,
>   And the years lie thicker than the leaves
>   In Tauremornalome.
-}

module Pugs.Compat.Posix (
    _PUGS_HAVE_POSIX,
    createLink,
    createSymbolicLink,
    readSymbolicLink,
    rename,
    removeLink,
    setFileMode,
    getEnvironment,
    getArg0,
    statFileSize,
    statFileMTime,
    statFileCTime,
    statFileATime,
    getProcessID,
    getRealUserID,
    getEffectiveUserID,
    getRealGroupID,
    getEffectiveGroupID,
    getProcessTimes,
    setEnv,
    getEnv,
    unsetEnv,
    signalProcess,
    ProcessTimes(..),
    clocksPerSecond,
    DirStream,
    openDirStream,
    readDirStream,
    rewindDirStream,
    closeDirStream,
    executeFile',        -- the prime signifies we changed signature.
    getCurrentDirectory,
    setCurrentDirectory,
    doesFileExist,
    doesDirectoryExist,
    doesExist,
    pugsTimeSpec
) where

import Foreign
import System.Cmd
import System.Posix.Types
import Data.Time
import System.IO.Error

#ifdef PUGS_HAVE_POSIX
import System.Posix.Files
import System.Posix.Process
import System.Posix.Env hiding (getEnvironment)
import System.Posix.Directory
import System.Posix.User
import System.Exit
import System.Environment (getEnvironment)
import Foreign.C.Types
import Foreign.C.String
import Data.Typeable
import qualified System.Posix.Signals

_PUGS_HAVE_POSIX :: Bool
_PUGS_HAVE_POSIX = True

doesExist :: FilePath -> IO Bool
doesExist = fileExist

testStatusWith :: (FileStatus -> Bool) -> FilePath -> IO Bool
testStatusWith t f = fmap t (getFileStatus f) `catchIOError` const (return False)

doesFileExist :: FilePath -> IO Bool
doesFileExist = testStatusWith isRegularFile

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist = testStatusWith isDirectory

getCurrentDirectory :: IO FilePath
getCurrentDirectory = getWorkingDirectory

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory = changeWorkingDirectory

executeFile' :: FilePath -> Bool -> [String] -> Maybe [(String, String)] -> IO ExitCode
executeFile' prog True args Nothing = rawSystem prog args
executeFile' prog search args env = do
    print (prog, search, args, env)
    executeFile prog search args env
    return $ ExitFailure 1            -- if we got here, it failed.

statFileSize :: FilePath -> IO Integer
statFileSize f = do
    s <- getFileStatus f
    return (toInteger (fileSize s))

statFileTime :: (FileStatus -> EpochTime) -> FilePath -> IO Integer
statFileTime op f = do
    s <- getFileStatus f
    return (toInteger $ fromEnum $ op s)

statFileMTime :: FilePath -> IO Integer
statFileMTime f = statFileTime modificationTime f >>= return

statFileCTime :: FilePath -> IO Integer
statFileCTime f = statFileTime statusChangeTime f >>= return

statFileATime :: FilePath -> IO Integer
statFileATime f = statFileTime accessTime f >>= return

type Signal = System.Posix.Signals.Signal
signalProcess :: Signal -> ProcessID -> IO ()
signalProcess = System.Posix.Signals.signalProcess

clocksPerSecond :: (Num a) => a
clocksPerSecond = 1000000

instance Typeable DirStream where
    typeOf _ = mkTyConApp (mkTyCon "DirStream") []

#else

import qualified Control.Exception
import Debug.Trace
import qualified System.Environment
import System.Directory (getCurrentDirectory, setCurrentDirectory, doesFileExist, doesDirectoryExist)
import System.IO
import System.Exit
import Foreign.C.String
import Foreign.Ptr
import Foreign.C.Types
import Data.Ratio
import Data.Typeable
import System.Posix.Types
import System.Posix.Internals
import Foreign.C.Error

_PUGS_HAVE_POSIX :: Bool
_PUGS_HAVE_POSIX = False

failWith :: (Monad m) => String -> m a
failWith s = fail $ "'" ++ s ++ "' not implemented on this platform."

failWithIncomplete :: (Monad m) => String -> m a
failWithIncomplete s = fail $ "'" ++ s ++ "' not fully implemented on this platform."

warnWith :: String -> IO ()
warnWith s = trace ("'" ++ s ++ "' not implemented on this platform.") $ return ()

-- This should all be moved into Compat.Win32, once we go that route

-- This is partially right - but some APIs return even "better" resolution
clocksPerSecond :: (Num a) => a
clocksPerSecond = 1000000

foreign import stdcall unsafe "SetEnvironmentVariableW" win32SetEnv :: CWString -> CWString -> IO ()

setEnv :: String -> String -> Bool -> IO ()
setEnv k v _ = withCWString k $ withCWString v . win32SetEnv

getEnv :: String -> IO (Maybe String)
getEnv k = (fmap Just (System.Environment.getEnv k)) `Control.Exception.catch` (\(e :: Control.Exception.SomeException) -> return Nothing)

unsetEnv :: String -> IO ()
unsetEnv k = withCWString k $ \ key -> withCWString "" $ \ v -> do
               win32SetEnv key v
-- #unsetEnv _ = warnWith "unsetEnv"

getEnvironment :: IO [(String, String)]
getEnvironment = System.Environment.getEnvironment

createLink :: FilePath -> FilePath -> IO ()
createLink _ _ = warnWith "link"

createSymbolicLink :: FilePath -> FilePath -> IO ()
createSymbolicLink _ _ = warnWith "symlink"

readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink _ = failWith "readlink"

rename :: FilePath -> FilePath -> IO ()
rename _ _ = warnWith "rename"

removeLink :: FilePath -> IO ()
removeLink _ = warnWith "unlink"

setFileMode :: FilePath -> FileMode -> IO ()
setFileMode _ _ = warnWith "chmod"

{-
newtype DirStream = DirStream (Ptr CDir)
    deriving (Typeable)
-}

type DirStream = ()

openDirStream :: FilePath -> IO DirStream
openDirStream name = warnWith "opendir"
--  withCString name $ \s -> do
--    dirp <- c_opendir s
--    return (DirStream dirp)

readDirStream :: DirStream -> IO FilePath
readDirStream _ = undefined

{-
readDirStream dirp =
  alloca $ \ptr_dEnt  -> loop ptr_dEnt
 where
  loop ptr_dEnt = do
    resetErrno
    r <- readdir dirp ptr_dEnt
    if (r == 0)
        then do
            dEnt <- peek ptr_dEnt
            if (dEnt == nullPtr) then return [] else do
                entry <- (d_name dEnt >>= peekCString)
                freeDirEnt dEnt
                return entry
        else do
            errno <- getErrno
            if (errno == eINTR) then loop ptr_dEnt else do
                let (Errno eo) = errno
                if (eo == end_of_dir)
                    then return []
                    else throwErrno "readDirStream"

rewindDirStream :: DirStream -> IO ()
rewindDirStream (DirStream dirp) = do
    c_rewinddir dirp
    return ()

closeDirStream :: DirStream -> IO ()
closeDirStream (DirStream dirp) = do
    c_closedir dirp
    return ()

-}

rewindDirStream :: DirStream -> IO ()
rewindDirStream _ = undefined

closeDirStream :: DirStream -> IO ()
closeDirStream _ = undefined

-- Win32 specific

type FILETIME = CULLong -- we'll keep the accuracy as long as possible

data ProcessTimes = ProcessTimes {
  elapsedTime :: Rational
  , userTime :: Rational
  , systemTime :: Rational
  , childUserTime :: Rational
  , childSystemTime :: Rational
}

foreign import stdcall unsafe "GetProcessTimes" win32GetProcessTimes ::
  Int -> Ptr FILETIME -> Ptr FILETIME -> Ptr FILETIME -> Ptr FILETIME -> IO Int
-- relies on Int == Int32 on Windows

filetimeToSeconds :: FILETIME -> Rational
filetimeToSeconds ft = ((toInteger ft) % 10)

-- See Perl5 win32/win32.c for the complete implementation
-- that works on Windows 95 as well
getProcessTimes :: IO ProcessTimes
getProcessTimes = do
                    pid <- getProcessHandle
                    alloca $ \ pDummy  -> do
                    alloca $ \ pKernel -> do
                    alloca $ \ pUser   -> do
                      poke pDummy 0
                      poke pKernel 0
                      poke pUser 0
                      win32GetProcessTimes pid pDummy pDummy pKernel pUser
                      user    <- peek pUser
                      kernel  <- peek pKernel
                      return $ ProcessTimes 0 (filetimeToSeconds user) (filetimeToSeconds kernel) 0 0

-- This is Win32 specific, dunno about other non POSIX platforms
statFileSize :: FilePath -> IO Integer
statFileSize n = Control.Exception.bracket (openFile n ReadMode) hClose hFileSize
-- statFileSize _ = failWith "-s"

-- Again, Win32 specific magic, as stolen from GHC
foreign import stdcall unsafe "GetCurrentProcessId" getProcessID :: IO Int -- relies on Int == Int32 on Windows
foreign import stdcall "GetCurrentProcess" getProcessHandle :: IO Int -- relies on Int == Int32 on Windows

-- getProcessID :: IO Int
-- getProcessID = return $ 1

type UserID = Int
type GroupID = Int

getRealUserID :: IO UserID
getRealUserID = return 1

getEffectiveUserID :: IO UserID
getEffectiveUserID = return 1

getRealGroupID :: IO GroupID
getRealGroupID = return 1

getEffectiveGroupID :: IO GroupID
getEffectiveGroupID = return 1

signalProcess :: Int -> Int -> IO ()
signalProcess _ _ = failWith "kill"

executeFile' :: FilePath -> Bool -> [String] -> Maybe [(String, String)] -> IO ExitCode
executeFile' prog True args Nothing = rawSystem prog args
executeFile' _ _ _ _ = failWithIncomplete "executeFile"

doesExist :: FilePath -> IO Bool
doesExist f = do
    rv <- doesFileExist f
    if rv then return rv else doesDirectoryExist f

statFileMTime :: FilePath -> IO Integer
statFileMTime _ = failWith "-M"

statFileCTime :: FilePath -> IO Integer
statFileCTime _ = failWith "-C"

statFileATime :: FilePath -> IO Integer
statFileATime _ = failWith "-A"

#endif

foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

getArg0 :: IO String
getArg0 = do
    alloca $ \ p_argc -> do
    alloca $ \ p_argv -> do
        getProgArgv p_argc p_argv
        argv <- peek p_argv
        peekCString =<< peekElemOff argv 0

epochY2K :: UTCTime
epochY2K = UTCTime
    { utctDay     = fromGregorian 2000 01 01
    , utctDayTime = 0
    }

{-|
Convert an internal @ClockTime@ to a Pugs-style fractional time.
Used by op0 "time", @Pugs.Run.prepareEnv@, and the file time tests.
-}
pugsTimeSpec :: UTCTime -> Rational
pugsTimeSpec clkt = toRational (diffUTCTime clkt epochY2K)
