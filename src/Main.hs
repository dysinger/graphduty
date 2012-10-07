import Control.Monad.IO.Class
import GraphDuty.Check
import GraphDuty.Types
import GraphDuty.Dispatch
import Remote
import System.Console.CmdArgs
import System.Environment

main :: IO ()
main = do
  as <- cmdArgs mode
  withArgs [] $ remoteInit (remote as) rmetadata (dispatch as)
  where
    mode = modes [ Args { config = "/etc/graphduty.json"
                                   &= help "JSON or YAML Config File"
                                   &= typFile
                        , remote = def
                                   &= help "Remote Config File"
                                   &= typFile }
                   &= help "Start Monitoring" ]
           &= verbosityArgs [explicit, name "loud", name "l"] []
           &= versionArg [explicit, name "version", name "v", summary info]
           &= summary (info ++ ", " ++ copyright)
           &= help about
           &= helpArg [explicit, name "help", name "h"]
           &= program app
    app       = "graphduty"
    version   = "0.1.0.0"
    info      = app ++ " version " ++ version
    about     = "Graphite Monitoring with Alerting to PagerDuty"
    copyright = "(C) 2012 Knewton & Tim Dysinger"
    rmetadata = [ GraphDuty.Check.__remoteCallMetaData ]
