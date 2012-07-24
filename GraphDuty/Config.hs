module GraphDuty.Config where

import qualified Data.Aeson             as A
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as CB
import qualified Data.ByteString.Lazy   as LB
import           Data.Char
import qualified Data.Yaml              as Y
import           GraphDuty.Types

loadConfig :: FilePath -> IO (Maybe Cfg)
loadConfig fp =
  let fileBS = CB.pack $ map toLower fp
    in if ".j" `CB.isInfixOf` fileBS
       then return . A.decode =<< LB.readFile fp
       else if ".y" `CB.isInfixOf` fileBS
            then return . Y.decode =<< B.readFile fp
            else return Nothing
