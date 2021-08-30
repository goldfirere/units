Version 2.2.1
-------------
* Actually export Imperial units, along with bugfixes (thanks to @Bebere).
* Export Eurocard units (thanks to @mlang).

Version 2.2
-----------

* Add support for new PlaneAngle and SolidAngle dimensions, thanks
to @gittywithexcitement.

Version 2.1.0.1
---------------

* Update .cabal file to export Data.Units.Astronomical.

Version 2.1
-----------

* Add support for Imperial and astronomical units, thanks to @Bebere.

Version 2.0.1.1
---------------

* Drop 7.8 support, fixing warnings on 8.0.

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
