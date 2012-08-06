module GraphDuty.Check where

import           Control.Monad.IO.Class
import qualified Data.Aeson             as A
import qualified Data.Conduit           as C
import           Data.Maybe
import           GraphDuty.Types
import qualified Network.HTTP.Conduit   as C
import           Remote

sendAlert :: Cfg -> Rule -> Metric -> ProcessM ()
sendAlert (Cfg {..}) (Rule {..}) (Metric {..}) = do
  let alert =
        A.encode
        $ PagerDutyAlert { pdAlertServiceKey  = pdCfgKey cfgPagerDuty
                         , pdAlertIncidentKey = metricTarget
                         , pdAlertEventType   = "trigger"
                         , pdAlertDescription = metricTarget
                                                ++ " is "
                                                ++ ruleOperator
                                                ++ " "
                                                ++ show ruleValue
                                                ++ " Graph: "
                                                ++ graphUrl
                                                ++ " Note: "
                                                ++ ruleNote }
      alertUrl = pdCfgUrl cfgPagerDuty ++ "/create_event.json"
      graphUrl = gCfgUrl cfgGraphite
                 ++ "?target=" ++ metricTarget
  request <- liftIO $ C.parseUrl alertUrl
  let post = request { C.method = "POST"
                     , C.requestBody = C.RequestBodyLBS alert }
  liftIO $ C.runResourceT $ C.withManager $ \m -> C.http post m >> return ()

checkMetric :: Cfg -> Rule -> Metric -> ProcessM ()
checkMetric cfg@(Cfg {..}) rule@(Rule {..}) metric@(Metric {..}) = do
  let points =
        map (\(DataPoint v _) -> fromJust v) (filter nullDp metricDatapoints)
  if length points > 0 && all (flip (op ruleOperator) ruleValue) points
    then spawnLocal (sendAlert cfg rule metric) >> return ()
    else return ()
  where
    nullDp (DataPoint Nothing _) = False
    nullDp _                     = True
    op "<"  = (<)
    op "<=" = (<=)
    op "==" = (==)
    op ">"  = (>)
    op ">=" = (>=)
    op _    = error "Rule operators can only be <, <=, ==, >, or >="

check :: Check -> ProcessM ()
check (Check {..}) = do
  say $ "Checking " ++ rulePattern checkRule
  let url = gCfgUrl (cfgGraphite checkCfg)
            ++ "/render/?target=" ++ (rulePattern checkRule)
            ++ "&from=" ++ (rulePeriod checkRule) ++ "&format=json"
  metricsResult <- liftIO $ getMetrics url
  case metricsResult of
    Just ms -> mapM_ (\m -> checkMetric checkCfg checkRule m) ms
    Nothing -> return ()
  where
    getMetrics :: String -> IO (Maybe [Metric])
    getMetrics u = return . A.decode =<< C.simpleHttp u

$( remotable [ 'check ] )
