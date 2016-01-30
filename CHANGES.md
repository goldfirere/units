Version 2.0.1
-------------

* Added the universal gravitational constant, thanks to @hesiod.

Version 2.0.0.1
---------------

* Typos in documentation. Thanks to Doug Burke for a pull request!

Version 2.0
-----------

* Full re-organization of modules.
* Addition of US customary units, CGS units, and a few other gubbins.

Version 1.1
-----------

* Added `si` parser.
* Moved `DefaultUnitOfDim` instances from `SI.MonoTypes` to `SI.Mono`. This allows
  client code to import the monomorphic types without getting these instances. But,
  it may mean that your imports have to change.

Version 1.0.1
-------------

 * Added derived SI units.
 * Added British-spelled synonyms for SI units.

Version 1.0
-----------

 * First release, contains implementations for SI.
