cron
====
[![Build Status](https://secure.travis-ci.org/MichaelXavier/cron.png)](http://travis-ci.org/MichaelXavier/cron)

Cron data structure and Attoparsec parser for Haskell. The idea is to embed it
in larger systems which want to roll their own scheduled tasks in a format that
people are used to.

`System.Cron` is where all the interesting datatypes live. You will also find
`scheduleMatches`, which you can use to compare a time against a `CronSchedule`
to see if an action needs to be performed.  System.Cron.Parser is where you will
find the parsers `cronSchedule`, `crontabEntry` and `cronTab`. To parse
individual schedules up to full crontab files.


To do anything, you'll need to install cabal-dev with cabal.

To build, run:

    make

To run tests, run:

    make test

If you have inotify-tools, run this to run tests continuously.

    make autotest

To generate docs:

    make docs

#### Scheduler
Cron offers a scheduling monad which can be found in `System.Cron.Schedule`. This monad transform allows you to declare a set of jobs (of the type `IO ()`) that will be executed at intervals defined by cron strings.

```haskell
main :: IO ()
main = do
    ...
    tids <- execSchedule $ do
        addJob job1 "* * * * *"
        addJob job2 "0 * * * *"
    print tids
    ...

job1 :: IO ()
job1 = putStrLn "Job 1"

job2 :: IO ()
job2 = putStrLn "Job 2"
```

## Contributors

* [Simon Hengel](https://github.com/sol)
* [Alberto Valverde](https://github.com/albertov)
* [Andrew Rademacher](https://github.com/AndrewRademacher)
* [Peter Simons](https://github.com/peti)
