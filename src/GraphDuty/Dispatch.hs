module GraphDuty.Dispatch where

import Control.Monad.IO.Class
import GraphDuty.Check
import GraphDuty.Config
import GraphDuty.Types
import Remote

dispatch :: Args -> String -> ProcessM ()
dispatch (Args {..}) "SLAVE" = receiveWait []
dispatch (Args {..}) _ = do
  selfNodeId   <- getSelfNode
  configResult <- liftIO (loadConfig config)
  case configResult of
    Just cfg@(Cfg {..}) -> do
      peers <- getPeers
      let nodes = selfNodeId : findPeerByRole peers "SLAVE"
      mapM_ (\(n, r) -> spawnLink n (check__closure $ Check cfg r))
        $ zip (cycle nodes) cfgRules
    Nothing -> error "Could not open & parse YAML or JSON Rule file."
