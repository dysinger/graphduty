module GraphDuty.Types where

import Data.Aeson.TH
import Data.Binary
import Data.Data
import GraphDuty.Util
import Remote.Encoding

data Rule = Rule { rulePattern  :: String
                 , ruleOperator :: String
                 , ruleValue    :: Double
                 , rulePeriod   :: String
                 , ruleNote     :: String }
          deriving (Data, Typeable, Show, Eq)

data GraphiteCfg = GraphiteCfg { gCfgUrl :: String }
                 deriving (Data, Typeable, Show, Eq)

data PagerDutyCfg = PagerDutyCfg { pdCfgUrl :: String
                                 , pdCfgKey :: String }
               deriving (Data, Typeable, Show, Eq)

data Cfg = Cfg { cfgGraphite  :: GraphiteCfg
               , cfgPagerDuty :: PagerDutyCfg
               , cfgRules     :: [Rule] }
         deriving (Data, Typeable, Show, Eq)

data DataPoint = DataPoint (Maybe Double) Integer
         deriving (Data, Typeable, Show, Eq)

data Metric = Metric { metricTarget     :: String
                     , metricDatapoints :: [DataPoint] }
            deriving (Data, Typeable, Show, Eq)

data Check = Check { checkCfg  :: Cfg
                   , checkRule :: Rule }
           deriving (Data, Typeable, Show, Eq)

data PagerDutyAlert = PagerDutyAlert { pdAlertServiceKey  :: String
                                     , pdAlertIncidentKey :: String
                                     , pdAlertEventType   :: String
                                     , pdAlertDescription :: String }
                    deriving (Data, Typeable, Show, Eq)

data Args = Args { config :: FilePath
                 , remote :: Maybe FilePath }
          deriving (Data, Typeable, Show, Eq)

instance Binary Cfg            where put = genericPut ; get = genericGet
instance Binary Check          where put = genericPut ; get = genericGet
instance Binary DataPoint      where put = genericPut ; get = genericGet
instance Binary GraphiteCfg    where put = genericPut ; get = genericGet
instance Binary Metric         where put = genericPut ; get = genericGet
instance Binary PagerDutyAlert where put = genericPut ; get = genericGet
instance Binary PagerDutyCfg   where put = genericPut ; get = genericGet
instance Binary Rule           where put = genericPut ; get = genericGet

$( deriveJSON (camelToSnake . drop 4) ''GraphiteCfg )
$( deriveJSON (camelToSnake . drop 5) ''PagerDutyCfg )
$( deriveJSON (camelToSnake . drop 3) ''Cfg )
$( deriveJSON (camelToSnake . drop 4) ''Rule )
$( deriveJSON id                      ''DataPoint )
$( deriveJSON (camelToSnake . drop 6) ''Metric )
$( deriveJSON (camelToSnake . drop 7) ''PagerDutyAlert )
