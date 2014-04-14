units
=====

The _units_ package provides a mechanism for compile-time dimensional analysis
in Haskell programs. It defines an embedded type system based on
units-of-measure. The units defined are fully extensible, and need not relate
to physical properties. As a matter of convenience only, the core package
defines the dimensions and units for the international system (SI), and you
can find many additional units and dimensions in package _units-extra_.

The package supports defining multiple inter-convertible units, such
as `Meter` and `Foot`. When extracting a numerical value from a quantity,
the desired unit must be specified, and the value is converted into
that unit.

The laws of nature have dimensions, and they hold true regardless of the units
used. For example, the gravitational force between two bodies is
`(gravitational constant) * (mass 1) * (mass 2) / (distance between body 1 and
2)^2`, regardless of whether the distance is given in meters or feet
or centimeters. In other words, every law of nature is unit-polymorphic.

The _units_ package supports unit-polymorphic programs through the coherent
system of units (CSU) mechanism. A CSU is essentially a mapping from
dimensions (such as length or mass) to the units (such as meters or
kilograms). All dimensioned quantities (generally just called quantities) are
expressed using the `Qu` type. The `Qu` type constructor takes a (perhaps
compound) dimension, a CSU and a numerical value type as arguments.
Internally, the quantity is stored as a number in the units as specified in
the CSU -- this may matter if you are worried about rounding errors.
In the sequence of computations that works within one CSU,
there is no unit conversion. Unit conversions are needed only when
putting values in and out of quantities, or converting between two different
CSUs.



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

The _units_ package exports several modules. For any given project, you will
include some set of these modules. There are dependency relationships
between them. Of course, you're welcome to `import` a module without its
dependents, but it probably won't be very useful to you. I hope that this list
grows over time.

 -  __`Data.Metrology`__

    This is the main exported module. It exports all the necessary functionality
    for you to build your own set of units and operate with them. All modules
    implicitly depend on this one.

 -  __`Data.Metrology.Poly`__

    This module exports some more definitions that may be useful when writing
    functions polymorphic over the choice of dimension. These functions are
    sometimes challenging (or perhaps impossible) to write, as the system is
    designed more with _monomorphic_ use than polymorphic use.

 -  __`Data.Metrology.Unsafe`__

    This module exports the constructor for the central datatype that stores
    quantities. With this constructor, you can arbitrarily change
    units! Use at your peril.

 -  __`Data.Metrology.Show`__

    This module defines a `Show` instance for quantities, printing
    out the number stored along with its canonical dimension. This behavior
    may not be the best for every setting, so it is exported separately.

 -  __`Data.Metrology.SI`__

    This module exports unit definitions for the [SI][] system of units,
    re-exporting the three modules below.

[SI]: http://en.wikipedia.org/wiki/International_System_of_Units
 
 -  __`Data.Metrology.SI.Units`__

    This module exports only the SI units, such as `Meter` and `Ampere`.

 -  __`Data.Metrology.SI.Types`__

    This module exports pre-defined unit type synonyms for SI dimensions,
    convenient for use with the SI.Units module.
    For example, `Length` is the type of
    quantities with unit `Meter`s and with numerical type `Double`.

 -  __`Data.Metrology.SI.Prefixes`__

    This module exports the SI prefixes. Note that this does *not* depend on
    any of the other SI modules -- you can use these prefixes with any system
    of units.



Examples
========

**NOTE: THIS IS OUT OF DATE.**

Unit definitions
----------------

Here is how to define two inter-convertible units:

    data Meter = Meter    -- each unit is a datatype that acts as its own proxy
    instance Unit Meter where           -- declare Meter as a Unit
      type BaseUnit Meter = Canonical   -- Meters are "canonical"
    instance Show Meter where           -- Show instances are optional but useful
      show _ = "m"                      -- do *not* examine the argument!

    data Foot = Foot
    instance Unit Foot where
      type BaseUnit Foot = Meter        -- Foot is defined in terms of Meter
      conversionRatio _ = 0.3048        -- do *not* examine the argument!
    instance Show Foot where
      show _ = "ft"

    type Length = MkQu Meter           -- we will manipulate Lengths
    type Length' = MkQu Foot           -- this is the *same* as Length

    extend :: Length -> Length          -- a function over lengths
    extend x = dim $ x .+ (1 % Meter)   -- more on this later

    inMeters :: Length -> Double        -- extract the # of meters
    inMeters = (# Meter)                -- more on this later

Let's pick this apart. The `data Meter = Meter` declaration creates both the
type `Meter` and a term-level proxy for it. It would be possible to get away
without the proxies and lots of type annotations, but who would want to?
Then, we define an instance of `Unit` to make `Meter` into a proper unit.
The `Unit` class is primarily responsible for handling unit conversions.
In the case of `Meter`, we define that as the _canonical_ unit of length, meaning
that all lengths will internally be stored in meters. It also means that we
don't need to define a conversion ratio for meters.

We also include a `Show` instance for `Meter` so that lengths can be printed
easily. If you don't need to `show` your lengths, there is no need for this
instance.

When defining `Foot`, we say that its `BaseUnit` is `Meter`, meaning that
`Foot` is inter-convertible with `Meter`. We also must define the conversion
ratio, which is the number of meters in a foot. Note that the
`conversionRatio` method must take a parameter to fix its type parameter, but
it _must not_ inspect that parameter. Internally, it will be passed
`undefined` quite often.

The `MkQu` type synonym makes a quantity for a given unit. Note
that `Length` and `Length'` are _the same type_. The `MkQu` machinery notices
that these two are inter-convertible and will produce the same dimensioned
quantity.

Note that, as you can see in the function examples at the end, it is necessary
to specify the choice of unit when creating a quantity or
extracting from a quantity. Thus, other than thinking about the
vagaries of floating point wibbles and the `Show` instance, it is _completely
irrelevant_ which unit is canonical. The type `Length` defined here could be
used equally well in a program that deals exclusively in feet as it could in a
program with meters.

As a tangential note: I have experimented both with definitions like `data
Meter = Meter` and `data Meter = Meters` (note the `s` at the end). The second
often flows more nicely in code, but the annoyance of having to remember
whether I was at the type level or the term level led me to use the former in
my work.

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

There are several ways of combining units to create other units. Let's also
have a unit of time:

    data Second = Second
    instance Unit Second where
      type BaseUnit Second = Canonical
    instance Show Second where
      show _ = "s"

    type Time = MkQu Second

Units can be multiplied and divided with the operators `:*` and `:/`, at either
the term or type level. For example:

    type MetersPerSecond = Meter :/ Second
    type Velocity1 = MkQu MetersPerSecond

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
    type Area1 = MkQu MetersSquared
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
combinators. The only one deserving special mention is `dim`, the
dimension-safe cast operator. Expressions written with the _units_ package can
have their types inferred. This works just fine in practice, but the types are
terrible, unfortunately. Much better is to use top-level annotations (using
abbreviations like `Length` and `Time`) for your functions. However, it may
happen that the inferred type of your expression and the given type of your
function may not exactly match up. This is because quantities have
a looser notion of type equality than Haskell does. For example, "meter *
second" should be the same as "second * meter", even though these are in
different order. The `dim` function checks (at compile time) to make sure its
input type and output type represent the same underlying dimension and then
performs a cast from one to the other. This cast is completely free at
runtime. When providing type annotations, it is good practice to start your
function with a `dim $` to prevent the possibility of type errors. For
example, say we redefine velocity a different way:

    type Velocity3 = Scalar %/ Time %* Length
    addVels :: Velocity1 -> Velocity1 -> Velocity3
    addVels v1 v2 = dim $ v1 .+ v2

This is a bit contrived, but it demonstrates the point. Without the `dim`, the
`addVels` function would not type-check. Because `dim` needs to know its
_result_ type to type-check, it should only be used at the top level, such as
here, where there is a type annotation to guide it.

Note that `dim` is _always_ dimension-safe -- it will not convert a time to a
length!

