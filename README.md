# Hive

A Haskell implementation of the Hive game.

https://boardgamegeek.com/boardgame/2655/hive

## How to play

```
$ stack build
$ stack exec play-hive
```

This will kickstart a REPL where you can type commands. Available commands:

* `show [TARGET]`: Display the given target. If no target is given, show all. Available targets: `board`, `player`
* `help`: Display all of these commands
