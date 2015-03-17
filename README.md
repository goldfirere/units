units
=====

The _units_ package provides a mechanism for compile-time dimensional analysis
in Haskell programs. It defines an embedded type system based on
units-of-measure. The units and dimensions defined are fully extensible, and
need not relate to physical properties. This package exports definitions
only for `Dimensionless` and `Number`. The set of units and dimensions from
the International System (SI) are exported from the companion package `units-defs`.

This package supports independent notions of _dimension_ and _unit_. Examples
of dimensions include length and mass. Examples of unit include meter and
gram. Every unit measures a particular dimension, but a given dimension
may be measure by many different units. For example, both meters and feet
measure length.

The package supports defining multiple inter-convertible units of the same
dimension, such as `Meter` and `Foot`. When extracting a numerical value from
a quantity, the desired unit must be specified, and the value is converted
into that unit.

The laws of nature have dimensions, and they hold true regardless of the units
used. For example, the gravitational force between two bodies is
`(gravitational constant) * (mass 1) * (mass 2) / (distance between body 1 and
2)^2`, regardless of whether the distance is given in meters or feet
or centimeters. In other words, every law of nature is unit-polymorphic.

The _units_ package supports unit-polymorphic programs through the coherent
system of units (CSU) mechanism. A CSU is essentially a mapping from
dimensions to the units. All dimensioned quantities (generally just called
quantities) are expressed using the `Qu` type. The `Qu` type constructor takes
a (perhaps compound) dimension, a CSU and a numerical value type as arguments.
Internally, the quantity is stored as a number in the units as specified in
the CSU -- this may matter if you are worried about rounding errors. In the
sequence of computations that works within one CSU, there is no unit
conversion. Unit conversions are needed only when putting values in and out of
quantities, or converting between two different CSUs.



User contributions
------------------

It is easy to imagine any number of built-in facilities that would go well
with this package (sets of definitions of units for various systems, vector
operations, a suite of polymorphic functions that are commonly needed but hard
to define, etc.). Yet, I (Richard) don't have the time to imagine or write all
of these. If you write code that is sufficiently general and might want to be
included with this package (but you don't necessarily want to create your own
new package), please write me!

Modules
-------

The _units_ package exports several key modules. Note that you will generally
import only *one* of `Data.Metrology`, `Data.Metrology.Poly`, or
`Data.Metrology.Vector`.

 -  __`Data.Metrology.Poly`__

    This is the main exported module. It exports all the necessary functionality
    for you to build your own set of units and operate with them.

 -  __`Data.Metrology`__

    This re-exports most of the definitions from `Data.Metrology.Poly`, but
    restricts a few operators to work only with the default LCSU, as this is
    simpler for new users to `units`.

 -  __`Data.Metrology.Vector`__

    This also re-exports a similar set of definitions as `Data.Metrology.Poly`,
    but provides numerical operations based on `vector-space` instead of the
    standard numerical classes.

 -  __`Data.Metrology.Internal`__

    This module contains mostly-internal definitions that may appear in GHC's
    error messages. Users will generally not need to use these definitions in
    their code. However, by exporting this module from within
    `Data.Metrology.Poly`, we can reduce the module-prefix clutter in error
    messages.

 -  __`Data.Metrology.Unsafe`__

    This module exports the constructor for the central datatype that stores
    quantities. With this constructor, you can arbitrarily change
    units! Use at your peril.

 -  __`Data.Metrology.Show`__

    This module defines a `Show` instance for quantities, printing
    out the number stored along with its canonical dimension. This behavior
    may not be the best for every setting, so it is exported separately.
    Importing this module reduces the guaranteed unit-safety of your code,
    because it allows you to inspect (in a round-about way) how your quantities
    are stored.

 -  __`Data.Metrology.Parser`__

    This module allows users to create custom unit parsers. The user specifies
    a set of prefixes and a set of units to parse, and then a quasi-quoting parser
    is generated. See the module documentation for details.

 -  __`Data.Metrology.TH`__

    This module exports several functions, written with Template Haskell, that
    make programming with `units` somewhat easier. In particular, see
    `declareMonoUnit`, which gets rid of a lot of the boilerplate if you don't
    want unit polymorphism.

 -  __`Data.Metrology.Quantity`__

    This module defines a `Quantity` class to enable easy, safe conversions with
    non-`units` types. See the module for more documentation.

Examples
========

We will build up a full working example in several sections. It is awkward to
explain the details of the pieces until the whole example is built, so please
read on to see how it all works. For more complete(-ish) examples, see [this
test
case](https://github.com/goldfirere/units/blob/master/Tests/Compile/Simulator.hs)
(for examples of how to use units) and
[units-defs](https://github.com/goldfirere/units-defs) (for examples of how to
define units).

Dimension definitions
---------------------

When setting up your well-typed units-of-measure program, the first step is
to define the dimensions you will be working in. (If your application involves
physical quantities, you may want to check `Data.Dimensions.SI` in the
`units-defs` package first.)

    data LengthDim = LengthDim  -- each dimension is a datatype that acts as its own proxy
    instance Dimension LengthDim

    data TimeDim = TimeDim
    instance Dimension TimeDim

We can now build up dimensions from these base dimensions:

    type VelocityDim = LengthDim :/ TimeDim

Unit definitions
----------------

We then define units to work with these dimensions. Here, we define two different
inter-convertible units for length. (Note that just about all of this boilerplate
can be generated by functions in the `Data.Metrology.TH` module.)

    data Meter = Meter
    instance Unit Meter where           -- declare Meter as a Unit
      type BaseUnit Meter = Canonical   -- Meters are "canonical"
      type DimOfUnit Meter = LengthDim  -- Meters measure Lengths
    instance Show Meter where           -- Show instances are optional but useful
      show _ = "m"                      -- do *not* examine the argument!

    data Foot = Foot
    instance Unit Foot where
      type BaseUnit Foot = Meter        -- Foot is defined in terms of Meter
      conversionRatio _ = 0.3048        -- do *not* examine the argument!
                                        -- We don't need to specify the `DimOfUnit`;
                                        -- it's implied by the `BaseUnit`.
    instance Show Foot where
      show _ = "ft"

    data Second = Second
    instance Unit Second where
      type BaseUnit Second = Canonical
      type DimOfUnit Second = TimeDim
    instance Show Second where
      show _ = "s"

A unit assignment
-----------------

To perform computations with _units_, we must define a so-called _local coherent
set of units_, or LCSU. This is a mapping from dimensions to units, and it informs
exactly how the quantities are stored. For example:

    type LCSU = MkLCSU '[(LengthDim, Meter), (TimeDim, Second)]

This definition says that we wish to store lengths in meters and times in seconds.
Note that, even though `Meter` is defined as the `Canonical` length, we could have
used `Foot` in our LCSU. Canonical units are used only in conversion between
units, not the choice of how to store a quantity.

Value types
-----------

To use all these pieces to build the actual type that will store quantities, we
use one of the `MkQu_xxx` type synonyms, as follows:

    type Length = MkQu_DLN LengthDim LCSU Double
      -- Length stores lengths in our defined LCSU, using `Double` as the numerical type
    type Length' = MkQu_ULN Foot LCSU Double
      -- same as Length. Note the `U` in `MkQu_ULN`, allowing it to take a unit

    type Time = MkQu_DLN TimeDim LCSU Double

Some computations
-----------------

We now show some example computations on the defined types:

    extend :: Length -> Length            -- a function over lengths
    extend x = redim $ x |+| (1 % Meter)

    inMeters :: Length -> Double          -- extract the # of meters
    inMeters = (# Meter)                  -- more on this later

    conversion :: Length                  -- mixing units
    conversion = (4 % Meter) |+| (10 % Foot)

    vel :: Length %/ Time                 -- The `%*` and `%/` operators allow
                                          -- you to combine types
    vel = (3 % Meter) |/| (2 % Second)

Explanation
-----------

Let's pick this apart. The `data LengthDim = LengthDim` declaration creates both the
type `LengthDim` and a term-level proxy for it. It would be possible to get away
without the proxies and use lots of type annotations, but who would want to?
We must define an instance of `Dimension` to declare that `LengthDim` is a dimension.
Why suffix with `Dim`? To distinguish the length dimension from the length type.
Generally, the type is mentioned more often and should be the shorter name.

We then create a `TimeDim` to operate alongside the `LengthDim`. Using the
`:/` combinator, we can create a `VelocityDim` out of the two dimensions defined
so far. See below for more information on unit combinators.

Then, we make some units, using similar `data` definitions. We define an
instance of `Unit` to make `Meter` into a proper unit. The `Unit` class is
primarily responsible for handling unit conversions. In the case of `Meter`,
we define that as the _canonical_ unit of length, meaning that all lengths
will internally be stored in meters. It also means that we don't need to
define a conversion ratio for meters. You will also see that we say that
`Meter`s measure the dimension `LengthDim`, through the `DimOfUnit` declaration.

We also include a `Show` instance for `Meter` so that lengths can be printed
easily. If you don't need to `show` your lengths, there is no need for this
instance.

When defining `Foot`, we say that its `BaseUnit` is `Meter`, meaning that
`Foot` is inter-convertible with `Meter`. This declaration also says that
the dimension measured by a `Foot` must be the same as the dimension for
a `Meter`. We must then define the conversion
ratio, which is the number of meters in a foot. Note that the
`conversionRatio` method must take a parameter to fix its type parameter, but
it _must not_ inspect that parameter. Internally, it will be passed
`undefined` quite often.

The definition for `Second` is quite similar to that for `Meter`.

The next section of code constructs an "LCSU" -- a local coherent set of units.
The idea is that we wish to be able to choose a set of units which are to be
used in the internal, concrete representation. An LCSU is just an association
list giving a concrete unit for each dimension in your domain. The particular
LCSU here says that length is stored in meters and time is stored in
seconds. It would be invalid to specify an LCSU with repeats for either
dimension or unit.

With all this laid out, we can make the types that store values. _units_
exports several `MkQu_xxx` type synonyms that vary in the arguments they
expect. `MkQu_DLN`, for example, takes a dimension, an LCSU, and a
numerical type. With the definition above, `Length` is now a type suitable
for storing lengths.

Note
that `Length` and `Length'` are _the same type_. The `MkQu` machinery notices
that these two are inter-convertible and will produce the same dimensioned
quantity.

Note that, as you can see in the function examples at the end, it is necessary
to specify the choice of unit when creating a quantity or
extracting from a quantity. Thus, other than thinking about the
vagaries of floating point wibbles and the `Show` instance, it is _completely
irrelevant_ which unit the concrete unit in the LCSU.
The type `Length` defined here could be
used equally well in a program that deals exclusively in feet as it could in a
program with meters.

As a tangential note: I have experimented both with definitions like `data
Meter = Meter` and `data Meter = Meters` (note the `s` at the end). The second
often flows more nicely in code, but the annoyance of having to remember
whether I was at the type level or the term level led me to use the former in
my work.

Other features
==============

Prefixes
--------

Here is how to define the "kilo" prefix:

    data Kilo = Kilo
    instance UnitPrefix Kilo where
      multiplier _ = 1000

    kilo :: unit -> Kilo :@ unit
    kilo = (Kilo :@)

We define a prefix in much the same way as an ordinary unit, with a datatype
and a constructor to serve as a proxy. Instead of the `Unit` class, though,
we use the `UnitPrefix` class, which contains a `multiplier` method. As with
other methods, this may *not* inspect its argument.

Due to the way units are encoded, it is necessary to explicitly apply prefixes
with the `:@` combinator (available at both the type and term level). It is often
convenient to then define a function like `kilo` to make the code flow more
naturally:

    longWayAway :: Length
    longWayAway = 150 % kilo Meter

    longWayAwayInMeters :: Double
    longWayAwayInMeters = longWayAway # Meter  -- 150000.0

Unit combinators
----------------

There are several ways of combining units to create other units.
Units can be multiplied and divided with the operators `:*` and `:/`, at either
the term or type level. For example:

    type MetersPerSecond = Meter :/ Second
    type Velocity1 = MkQu_ULN MetersPerSecond LCSU Double

    speed :: Velocity1
    speed = 20 % (Meter :/ Second)

The _units_ package also provides combinators "%*" and "%/" to combine the
types of quantities.

    type Velocity2 = Length %/ Time    -- same type as Velocity1
    
There are also exponentiation combinators `:^` (for units) and `%^` (for
quantities) to raise to a power. To represent the power, the
_units_ package exports `Zero`, positive numbers `One` through `Five`, and
negative numbers `MOne` through `MFive`. At the term level, precede the number
with a `p` (mnemonic: "power"). For example:

    type MetersSquared = Meter :^ Two
    type Area1 = MkQu_ULN MetersSquared LCSU Double
    type Area2 = Length %^ Two        -- same type as Area1

    roomSize :: Area1
    roomSize = 100 % (Meter :^ pTwo)

    roomSize' :: Area1
    roomSize' = 100 % (Meter :* Meter)
    
Note that addition and subtraction on units does not make physical sense, so
those operations are not provided.

Dimension-safe cast
-------------------

The haddock documentation shows the term-level quantity
combinators. The only one deserving special mention is `redim`, the
dimension-safe cast operator. Expressions written with the _units_ package can
have their types inferred. This works just fine in practice, but the types are
terrible, unfortunately. Much better is to use top-level annotations (using
abbreviations like `Length` and `Time`) for your functions. However, it may
happen that the inferred type of your expression and the given type of your
function may not exactly match up. This is because quantities have
a looser notion of type equality than Haskell does. For example, "meter *
second" should be the same as "second * meter", even though these are in
different order. The `redim` function checks (at compile time) to make sure its
input type and output type represent the same underlying dimension and then
performs a cast from one to the other. This cast is completely free at
runtime. When providing type annotations, it is good practice to start your
function with a `redim $` to prevent the possibility of type errors. For
example, say we redefine velocity a different way:

    type Velocity3 = (MkQu_ULN Number LCSU Double) %/ Time %* Length
    addVels :: Velocity1 -> Velocity1 -> Velocity3
    addVels v1 v2 = redim $ v1 |+| v2

This is a bit contrived, but it demonstrates the point. Without the `redim`, the
`addVels` function would not type-check. Because `redim` needs to know its
_result_ type to type-check, it should only be used at the top level, such as
here, where there is a type annotation to guide it.

Note that `redim` is _always_ dimension-safe -- it will not convert a time to a
length!

Monomorphic behavior
====================

_units_ provides a facility for ignoring LCSUs, if your application does not
need to worry about numerical precision. The facility is through the type
family `DefaultUnitOfDim`. For example, with the definitions above, we could
say

    type instance DefaultUnitOfDim LengthDim = Meter
    type instance DefaultUnitOfDim TimeDim   = Second

and then use the `DefaultLCSU` for our LCSU. To make the use of the default
LCSU even easier, the `MkQu_xxx` operators that don't mention an LCSU all
use the default one. So, we can say

    type Length = MkQu_D LengthDim

and get to work. (This uses `Double` as the underlying numerical representation.)

The module `Data.Metrology.SI` from the _units-defs_ package exports type
instances for `DefaultUnitOfDim` for the SI types, meaning that you can use
definitions like this right away.

More examples
=============

Check out some of the test examples we have written to get more of a feel for
how this all works,
[here](https://github.com/goldfirere/units/tree/master/Tests).
