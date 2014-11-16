Version 2.2
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
