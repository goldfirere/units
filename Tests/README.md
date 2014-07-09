`units` Test Design
===================

There are two kinds of test:

 - `Compile` tests test only compilation, not any functionality. A new `Compile`
   test should be placed in the `Compile` directory and imported from
   `Tests.Main`, in alphabetical order with the others.

 - Other tests test both compilation and some functionality. These use the
   `tasty` testing framework, avaiable on Hackage. Each of these tests should
   export a `tests :: TestTree` definition which contain all the tests in
   the module. Then, this `tests` should be imported and run from within
   `Tests.Main`, following the style there.

To run the tests, just say `cabal test`.
