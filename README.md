# Euler Haskell

> Solutions to Project Euler problems in Haskell.

## Motivation

Project Euler problems are largely _math_ problems. Coding skills only come into play near the end of a given problem.

My goal is to learn **interesting mathematics** primarily. Although I'm also learning Haskell, I don't plan on using particularly advanced language features in this repository.

The [Advent of Code](https://github.com/neillrobson/advent-haskell) repository is a more appropriate location to mess around with novel Haskell language features and libraries.

> [!NOTE]
> Since this repository is public on GitHub, I will only publish solutions to the first 100 problems, in accordance with the [official policy](https://projecteuler.net/about#publish).
>
> Although I might share some general-purpose utilities in this pubic repo (e.g. generating primes quickly), I'll do my best to avoid problem-specific algorithms.

## Current Goal

I've noodled with [Problem 465, Polar Polygons](https://projecteuler.net/problem=465) for over a decade now. I'm going through selected related problems, to build up mathematical intuition and tools for one day conquering 465.

## Template Haskell enumeration

From Claude:

> You're slightly overestimating it. The imports themselves are not automated — look at 2025/AOC2025.hs:23-34, where every AOC2025.DayNN module is still imported by hand. What Discover.hs automates is the registry that those imports feed into.
>
> Here's the actual mechanism:
>
> 1. Enumerate every possible spec (Discover.hs:122-125). challengeSpecUniverse is just the Cartesian product of all Day × Part values — a static enumeration, no discovery.
> 2. Construct the expected function name (Discover.hs:161-162). specName turns CS day=1 part=a into the string "day01a".
> 3. Ask the compiler if that name is in scope (Discover.hs:128-130). TH.lookupValueName is the key Template Haskell primitive: at splice time it checks whether any identifier with that string is currently bound, returning Just Name or Nothing. The names get into scope only because
> of the manual import AOC2025.DayNN as AOC lines.
> 4. Build a list expression for the hits (Discover.hs:107-110, 132-142). traverse … catMaybes drops the Nothings, so only days/parts that actually exist make it into the generated [(Day, (Part, SomeSolution))] literal that the $$(solutionList) splice expands to.
> 5. Pick the right SomeSolution constructor (Discover.hs:145-151, 185-192). TH.reify inspects the solver's type and chooses MkSomeSolNF if its result has NFData, otherwise MkSomeSolWH.
>
> So the splice replaces what would otherwise be a long, hand-written list like [(day01, (Part1, MkSomeSolNF day01a)), (day01, (Part2, MkSomeSolNF day01b)), …] with a generated equivalent — and gracefully skips days you haven't written yet. The imports are load-bearing: comment one
> out and the corresponding day silently drops from the map (no error), because lookupValueName simply returns Nothing.
