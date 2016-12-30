{-# LANGUAGE RecordWildCards #-}

module System.Cron.Internal.DescribeTypes where

import Data.List                                (intercalate)
import Data.Maybe                               (catMaybes)
import Data.Semigroup
import System.Cron.Internal.DescribeUtils
import System.Cron.Internal.Describe.Time

data Verbosity = Verbose | NotVerbose deriving Show

data TimeFormat = Hour24 | Hour12 deriving Show

data Options = Opts {
    timeFormat :: TimeFormat
  , verbosity  :: Verbosity
  } deriving Show

type Build a = (a -> a)

data OptionBuilder = Builder {
    buildtf :: Build TimeFormat
  , buildv  :: Build Verbosity
  }

instance Semigroup OptionBuilder where
  (<>) (Builder tf1 v1) (Builder tf2 v2) = Builder (tf2 . tf1) (v2 . v1)

defaultOpts :: OptionBuilder
defaultOpts = Builder (const Hour12) (const NotVerbose)

twentyFourHourFormat :: OptionBuilder
twentyFourHourFormat = defaultOpts {buildtf = const Hour24}

twelveHourFormat :: OptionBuilder
twelveHourFormat = defaultOpts {buildtf = const Hour12}

verbose :: OptionBuilder
verbose = defaultOpts {buildv = const Verbose}

notVerbose :: OptionBuilder
notVerbose = defaultOpts {buildv = const NotVerbose}

getOpts :: OptionBuilder -> Options
getOpts (Builder tf v) = Opts (tf undefined) (v undefined)

data DescribedValue = Concrete String
                    | Every String


instance Show DescribedValue where
  show (Concrete s) = s
  show (Every s)    = s


data Time = ConcreteTime String
          | Other (Maybe DescribedValue) (Maybe DescribedValue)


instance Show Time where
  show (ConcreteTime s) = s
  show (Other md1 md2)  = intercalate ", " .
                          map show         $ catMaybes [md1, md2]


data Description = Desc {
    _time   :: Time
  , _dom    :: Maybe DescribedValue
  , _month  :: Maybe DescribedValue
  , _dow    :: Maybe DescribedValue
  }


instance Show Description where
  show Desc{..} = intercalate ", " .
                  (:) (show _time) .
                  map show         $ catMaybes [_dom, _dow, _month]
