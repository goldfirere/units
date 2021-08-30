units
=====

This repo is the home of the _units_ package for Haskell, which provides a
mechanism for compile-time dimensional analysis. There are two components
of this project, _units_ and _units-defs_. The _units_ package implements
all the type-level machinery for dimensional analysis, but it is completely
agnostic to the actual system of units used. Separately, the _units-defs_
package (which depends on _units_) defines commonly used units, including
implementing the SI (Système Internationale) system of units.

Look at the [README for _units_](units/) for more info.
