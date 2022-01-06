Release notes for `units`
=========================

Version 2.4.1.4
---------------
* Use the `(-)` operator of the underlying `Num` instance in `(|-|)`.
  This prevents crashes when the underlying number type does not support
  negation. (Issue #69)

Version 2.4.1.3
---------------
* Compatibility with GHC 8.10, thanks to @ocharles.

Version 2.4.1.2
---------------
* Compatibility with singletons 2.6 and GHC 8.8, further thanks to @ocharles.

Version 2.4.1.1
---------------
* Fix some GHC compatibility issues, thanks to @ocharles.

Version 2.4.1
-------------
* Add `Units` superclass to `UnitFactor`, easing type inference in
GHC 8.0, thanks to @rimmington.

Version 2.4
-----------
* New interface with the `linear` package in `Data.Metrology.Linear`.

* New `Show` and `Read` instances for dimensionless quantities.

* New `NFData` instances for quantities, thanks to @rimmington.

* GHC 8 compatibility.

Version 2.3
-----------
* `Data.Metrology.TH.evalType` now works in GHC 7.10 on dimensions like `Volume` instead
of just base dimensions.

* Break out the units parser into its own package: `units-parser`.

* Add `Data.Metrology.Unsafe.UnsafeQu`, which has `Functor` and other
instances.

* Fix the fixity of `%`. It was way too high! Now it's 5.

* Add the ability to convert in and out of point quantities. See `quOfPoint`
and friends in `Data.Metrology.Vector`.

Version 2.2.1
-------------

* Compatibility with GHC 7.10.

* Added `AdditiveGroup` and `VectorSpace` instances for `Qu`.

Version 2.2
-----------

* Some types of arithmetic operations are different to aid in type inference.
For example, `*|` does not normalize its dimension list.

* The types of the `derive...` TH functions now allow for deriving units
based on composite dimensions/units.

* New TH function to help declare constants, called `declareConstant`.

Version 2.1
-----------

* Includes a decently comprehensive test suite.

* Add support for unit parsing within expressions:

      g = 9.8 % [si| m/s^2 |]

  See `Data.Metrology.Parser`.

* Now, `Data.Metrology` exports operators that work with the default
  LCSU. Use `Data.Metrology.Poly` to get the old, more flexible operators.

* Moved `showIn` from `Data.Metrology.Show` to `Data.Metrology.Poly`. This
  allows users to import `showIn` without a `Show` instance for quantities.

* Numeric operations are available based on vector spaces, as implemented in
  the `vector-space` library. See `Data.Metrology.Vector`.

* Some documentation cleanup.

* New function `evalType` that evaluates a type, using Template Haskell. This
  allows for easier instance declarations for quantities.

* New class `Quantity` that allows for easy conversions with non-`units` types.

* A few bugfixes.

* The `Eq` and `Ord` instances now work over any quantity, not just dimensionless ones.

* New functions in `Data.Metrology.TH` that define `Dimension` and `Unit` instances
  for you.

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
