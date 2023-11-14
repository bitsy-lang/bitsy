Modules
=======
Modules are the basic unit of reusable hardware.

Module Definitions
------------------
Modules are declared using the `mod` keyword.

A module has a list of pins, a list of components, and a list of wires.

The following example is a `Buffer` module, which demonstrates all three parts:

.. code-block::

    pub mod Buffer
        incoming in  of Bit
        outgoing out of Bit

        reg queue of Bit init false

        wire queue <= io.in
        wire io.out <= queue
    end

This module is marked `pub` and is thus public.
It is guaranteed not to be optimized away, and it may be imported from other packages.

This module has two pins, an incoming pins `in` and and outgoing pins `out`.
Both carry a value of shape `Bit` (a byte) to and from the module.

This module contains one subcomponent: a `reg` (register) named `queue`.
It stores a value of shape `Bit`.
The `init` clause shows the reset value of the register is `false`.

The final two statements are wires.
The first, `wire queue <= io.in` will connect the pin `io.in` to the data pin of the register `queue`.
The first, `wire io.out <= queue` will connect the output pin of the register `queue` to the pin `io.out`.
The `io` in `io.in` and `io.out` is a component representing the pins of the current module.

Since no domain information has been provided, the module `Buffer` implicitly operates on an implicit clock and reset.
Thus, on each clock cycle, the register `queue` will latch the value on `io.in`,
while simultaneously driving `io.out` with the value that `queue` had on the previous cycle.


Module Instances
----------------
Once a module is defined, it may be instantiated.

Let's assume the above definition for `Buffer` has been given.
We can use several buffers in a row to make a 4-bit shift register:

.. code-block::

    pub mod ShiftReg
        incoming cin  of Bit
        outgoing cout of Bit
        outgoing val  of Word<4>

        mod buf0 of Buffer
        mod buf1 of Buffer
        mod buf2 of Buffer
        mod buf3 of Buffer

        wire io.cout <= buf3.out
        wire buf3.in <= buf2.out
        wire buf2.in <= buf1.out
        wire buf1.in <= buf0.out
        wire buf0.in <= io.cin
        wire io.val <= cat(buf3.val, buf2.val, buf1.val, buf0.val)
    end


Extern Modules
--------------
In Bitsy, externally-defined modules are called extern mods.
They can be declared with the `extern mod` keyword, together with their list of pins.

The rules for their simulation are supplied through a bus functional model.

.. code-block::

   pub extern mod And
        incoming a of Bit
        incoming b of Bit
        outgoing z of Bit
    end


Parameters
----------
Modules and extern mods may be parameterized.
The parameters are given in angle brackets next to the name of the module or extern mod.

For example, a parameterized version of the buffer would be:

.. code-block::

    pub mod Buffer<S of Shape>
        incoming in  of S
        outgoing out of S

        reg queue of S

        wire queue <= io.in
        wire io.out <= queue
    end

A parameterized version of the `And` extern mod could work on words of length n:

.. code-block::

   pub extern mod And<n of Nat>
        incoming a of Word<n>
        incoming b of Word<n>
        outgoing z of Word<n>
    end
