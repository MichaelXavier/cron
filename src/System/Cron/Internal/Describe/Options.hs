module System.Cron.Internal.Describe.Options where

import Data.Semigroup
import System.Cron.Internal.Describe.Types

data Options = Opts {
    timeFormat    :: TimeFormat
  , verbosity     :: Verbosity
  }

data OptionBuilder = Builder (Options -> Options)

defaultOpts :: Options
defaultOpts = Opts Hour12 NotVerbose

twentyFourHourFormat :: OptionBuilder
twentyFourHourFormat = Builder (\o -> o {timeFormat = Hour24} )

twelveHourFormat :: OptionBuilder
twelveHourFormat = Builder (\o -> o {timeFormat = Hour12} )

verbose :: OptionBuilder
verbose = Builder (\o -> o {verbosity = Verbose})

notVerbose :: OptionBuilder
notVerbose = Builder (\o -> o {verbosity = NotVerbose})

getOpts :: OptionBuilder -> Options
getOpts (Builder f) = f defaultOpts

instance Semigroup OptionBuilder where
  (Builder f) <> (Builder a) = Builder (a . f)

instance Monoid OptionBuilder where
  mempty = Builder (const defaultOpts)
  mappend = (<>)
