# advent-of-code

This repository should serve as a stark reminder for future me when I say "but
I'm so busy! I don't have time for <this thing that will greatly improve my
life>!"

## Twist

TL;DR: Scala Native and scala-cli

As a challenge, I'm trying to learn [Scala
Native](https://scala-native.readthedocs.io/en/latest/) and write most of the
solution code in the most unsafe, C-like way, without relying on
reimplementation of stdlib and not using javalib.

Additionally, each solution is a _almost_ self-contained [scala-cli
script](https://scala-cli.virtuslab.org/)

## Run

Example:

```bash
$ scala-cli 2020/day3.scala _lib.scala -- input_day3.txt
```

Inputs are not provided here, because they're unique to each participant.

I'm doing 2020 as a practice run up to 2021 (which starts in 5 days as I'm
writing this document), so I don't intend to finish it.
