module S17TurboSwitch where

import Network.SSH.Client.LibSSH2 (withSSH2User, execCommands)
import Network.SSH.Client.LibSSH2.Errors (ErrorCode (FILE))
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (digitToInt)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Paths_s17_turbo_switch (version)
import Data.Version (showVersion)
import Control.Monad (when)
import Options.Applicative
  ( execParser, info, header, progDesc, fullDesc, helper, Parser, option, long, short, metavar
  , value, help, auto, showDefault, infoOption, str)
import Control.Exception (catches, Handler (Handler))

data WorkMode = LowPower | Normal | Turbo deriving (Eq, Show, Enum, Read)

data CliOptions = CliOptions
  { cliHost :: String
  , cliUsername :: String
  , cliPassword :: String
  , cliWorkMode :: WorkMode
  } deriving Show

-- Name of application (should match package.yml)
appName :: String
appName = "s17-turbo-switch"

cliOptions :: Parser CliOptions
cliOptions = CliOptions
  <$> option str
      ( long "host"
     <> short 'H'
     <> metavar "HOST"
     <> help "Hostname (or IP) of S17 host"
      )
  <*> option str
      ( long "username"
     <> short 'u'
     <> metavar "USERNAME"
     <> value "root"
     <> help "username to log into SSH"
     <> showDefault
      )
  <*> option str
      ( long "password"
     <> short 'p'
     <> metavar "PASSWORD"
     <> value "admin"
     <> help "password to log into SSH"
     <> showDefault
      )
  <*> option auto
      ( long "workmode"
     <> short 'm'
     <> metavar "MODE"
     <> value Normal
     <> help "LowPower, Normal or Turbo"
     <> showDefault
      )

confFile :: String
confFile = "/config/cgminer.conf"

changeWorkMode :: WorkMode -> String -> String -> String -> IO ()
changeWorkMode wm user pass host = do
  known_hosts <- ((flip (</>)) ".ssh/known_hosts") <$> getHomeDirectory
  catches (withSSH2User known_hosts user pass host 22
    (\s -> do
        -- Get 'bitmain-work-mode' from remote
        (grep_ec, grep_output) <- execCommands s ["grep 'bitmain-work-mode' " ++ confFile]
        when (grep_ec /= 0) $ printError grep_ec grep_output

        -- Parse for value
        let v = parseWorkModeValue grep_output

        -- TODO: Check the last time work mode was changed to rate limit changes

        -- Update 'bitmain-work-mode' value on remote
        when (v /= Just i) $ do
          (sed_ec, sed_output) <- execCommands s ["sed -i '/^\"bitmain-work-mode\"/s/.*/\"bitmain-work-mode\" : \"" ++ show i ++ "\",/' " ++ confFile]
          when (sed_ec /= 0) $ printError sed_ec sed_output

          (restart_ec, restart_output) <- execCommands s ["/etc/init.d/cgminer.sh restart"]
          when (restart_ec /= 0) $ printError restart_ec restart_output

          -- TODO: Write timestamp file on miner so work mode changes can be rate limited
    ))
    [Handler $ handleSSHError known_hosts]
  where
    handleSSHError :: String -> ErrorCode -> IO ()
    handleSSHError kh FILE = putStrLn $ "libssh2 return a FILE error. Likely the " ++ kh
                             ++ " file could not be found."
    handleSSHError _ e = putStrLn $ "libssh2 error: " ++ show e
    i = fromEnum wm
    printError :: Int -> [B.ByteString] -> IO ()
    printError c m = do B.putStrLn $ B.unlines m
                        error $ "returned error code: " ++ show c

-- | Only expect the head to contain valid data
parseWorkModeValue :: [B.ByteString] -> Maybe Int
parseWorkModeValue (x:_) = (p . B.unpack) x
  where
    p ['"','b','i','t','m','a','i','n','-','w','o','r','k','-','m','o','d','e','"',' ',':',' ','"',n,'"',',','\n'] =
      Just $ digitToInt n
    p _ = Nothing
parseWorkModeValue _ = Nothing

execSwitchMode :: CliOptions -> IO ()
execSwitchMode (CliOptions host user pass wm) = changeWorkMode wm user pass host

mainExecParser :: IO ()
mainExecParser = execParser opts >>= execSwitchMode
  where
    opts = info (helper <*> versionOption <*> cliOptions)
      ( fullDesc
     <> progDesc "Switch work mode on S17 miner"
     <> header (appName ++ " - Switch work mode on S17 miner"))
    versionOption = infoOption (appName ++ " " ++ showVersion version)
      ( long "version" <> short 'v' <> help "Show version information" )
