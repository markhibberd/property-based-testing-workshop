# Property Based Testing

> [FunctionalConf 2018](https://functionalconf.com/)

Property based testing course with hedgehog.

## The Workshop

This workshop is an introduction to property based testing. The format
of the workshop will be an interactive session with a mix of
demonstration and practical exercises.

The material that we will work through is:

 - `Basics`: Getting started writing understand and writing properties.
 - `Generators`: Understanding how we generate data for our tests.
 - `Patterns`: The repeating patterns of property tests.
 - `Practice`: Getting better at finding and writing properties.
 - `Advanced`: Advanced techniques that can be useful for testing more complex scenarios.

Please work through the following set-up instructions before the
workshop. This will allow the workshop to commence on time.

Exercises will be exposed shortly before the workshop to ensure that
everyone starts at the same point.


## Getting Set-Up

There are three basic steps that should be done before the workshop:

 1. Install haskell.
 2. Clone this repository.
 3. Do an initial build so the dependencies are available.


### Installing Haskell

This workshop should work with any ghc/haskell 8.2, 8.4 or 8.6 you find
easy to get set-up with. One option for doing this is following the
instructions at [https://www.haskell.org/platform/](https://www.haskell.org/platform/).

To verify your install, you should be able to run:

```
ghc --version
```

### Clone this repository.

```
git clone https://github.com/markhibberd/property-based-testing-workshop.git
```


### Initial Build

There are two commonly used command line tools for building haskell
projects: `cabal` and `stack`. If you don't know or care about the
difference for now, just use the `cabal` option below, if you have
already used one of them and want to keep to what is familiar then
follow the relevant instructions.

```
# Set-up using cabal install

cabal new-build --enable-tests


# Or, set-up using stack.

stack init
stack build --test
```
