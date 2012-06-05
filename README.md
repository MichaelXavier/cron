cron
====

Cron data structure and Attoparsec parser for Haskell. The idea is to embed it
in larger systems which want to roll their own scheduled tasks in a format that
people are used to.

`System.Cron` is where all the interesting datatypes live. You will also find
`scheduleMatches`, which you can use to compare a time against a cronSchedule to
see if an action needs to be performed.  System.Cron.Parser is where you will
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
