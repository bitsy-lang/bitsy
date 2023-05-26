Values and Shapes
=================
Bitsy separates out the world of "hardware components" from "hardware values".

Hardware components are the pieces of a Bitsy design which would come to correspond to a physical piece of silicon.
These include things like registers, module instances, and memories.

Hardware values only exist at simulation time.
A Bitsy design explains how values are transformed, but you never have direct access to one.

Every hardware value has a shape.
Bitsy includes the following built-in shapes:

`Bit`
-----
A `Bit` is a 1-bit boolean value.

`Word<n>`
---------
A `Word<n>` is an n-bit number.
It is nominally interpreted to be unsigned.

`Vec<n, S>`
-----------
A `Vec<n, S>` is an n-element vector of elements of a shape `S`.

`Valid<S>`
----------
A `Valid<S>` is an optional value of shape `S`.
It is used when a value may or may not be available.
It's values are `@Valid(s)` when there is data available, and `@Invalid` otherwise.

`Tuple<S, T>`
-------------
A `Tuple<S, T>` is a pair `tuple(s, t)` with `s` having shape `S` and `t` having shape `T`.

Structs
-------
You can define your own aggregate shapes using the `struct` keyword:

.. code-block:: bitsy

    struct shape Packet
        field packet_type of PacketType
        field data of Word<8>
    end

Enums
-----
You can define your own enumerations and tagged union shapes using the `enum keyword`:

.. code-block:: bitsy

    enum shape State
        @Idle
        @Processing of Tuple<Word<8>, Bit>
        @Done of Word<8>
    end
