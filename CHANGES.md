Version 2.1
-----------

* Separated monomorphic and polymorphic interface.

* Includes a decently comprehensive test suite.

* Add support for unit parsing within expressions:

      g = 9.8 % [si| m/s^2 |]

  See `Data.Metrology.UnitParser`.

* Now, `Data.Metrology` exports operators that work with the default
  LCSU. Use `Data.Metrology.Poly` to get the old, more flexible operators.

Version 2.0
-----------

This is a major update. `units` now supports the notion of a local unit set
and of separable dimensions and units. See the description in the
[draft paper](http://www.cis.upenn.edu/~eir/papers/2014/units/units.pdf) for
more info.

This update will very likely break any code that used `units-1.x`.

The update was written in partnership with Takayuki Muranushi.

Version 1.1
-----------

* Added dependency on the singletons library

* Brought up to date with changes for GHC 7.8

* Generalized numerical representation

* Improved Haddock headers

Version 1.0.1
-------------

* Fixed dependency on base to force compilation with GHC >= 7.7

Version 1.0
-----------

 * First release
