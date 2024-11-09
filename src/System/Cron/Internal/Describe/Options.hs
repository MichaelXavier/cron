--------------------------------------------------------------------
-- |
-- Module      : System.Cron.Internal.Describe.Optons
-- Description : Functions for constructing the options that control how
--               cron schedules are described
-- Copyright   : (c) Joseph Canero 2016
-- License     : MIT
--
-- Maintainer: Joseph Canero <jmc41493@gmail.com>
-- Portability: portable
--------------------------------------------------------------------
module System.Cron.Internal.Describe.Options where

-------------------------------------------------------------------------------
import Data.Default
import Data.Semigroup as Semigroup
-------------------------------------------------------------------------------
import System.Cron.Internal.Describe.Types
-------------------------------------------------------------------------------


-- | Type that holds onto information for constructing options for
-- 'System.Cron.Describe.describe'.
data OptionBuilder = Builder (Options -> Options)


-- | Return a builder that creates the default options for
-- 'System.Cron.Describe.describe'. The default options are:
-- 'System.Cron.Describe.notVerbose' and 'System.Cron.Describe.twelveHourFormat'.
defaultOpts :: OptionBuilder
defaultOpts = Builder (const def)


-- | Return a builder that sets the options to use a 24-hour time format.
-- This changes how hours are described. Using the 24-hour format,
-- all hours are returned as their left-padded numeric value (01:00, 22:00, etc)
twentyFourHourFormat :: OptionBuilder
twentyFourHourFormat = Builder (\o -> o {timeFormat = Hour24} )


-- | Return a builder that sets the options to use a 12-hour time format.
-- This changes how hours are described. Using the 12-hour format,
-- all hours are returned as their left-padded numeric value with their period
-- (01:00 AM, 10:00 PM, etc)
twelveHourFormat :: OptionBuilder
twelveHourFormat = Builder (\o -> o {timeFormat = Hour12} )


-- | Return a builder that sets the options to be verbose. A verbose description
-- doesn't eliminate unnecessary information. The only caveat being that month
-- information is only ever displayed if it isn't "*".
verbose :: OptionBuilder
verbose = Builder (\o -> o {verbosity = Verbose})


-- | Return a builder that sets the options to not be verbose. All information
-- about the described cron schedule is returned. The only caveat being that
-- month information is only ever displayed if it isn't "*".
notVerbose :: OptionBuilder
notVerbose = Builder (\o -> o {verbosity = NotVerbose})


-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------


getOpts :: OptionBuilder -> Options
getOpts (Builder f) = f def


data Options = Opts {
    timeFormat    :: TimeFormat
  , verbosity     :: Verbosity
  }

instance Default Options where
  def = Opts {timeFormat = Hour12, verbosity = NotVerbose}

instance Semigroup.Semigroup OptionBuilder where
  (Builder f) <> (Builder a) = Builder (a . f)

instance Monoid OptionBuilder where
  mempty = Builder (const def)
  mappend = (<>)
