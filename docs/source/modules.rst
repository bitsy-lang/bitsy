Modules
=======
Modules are the basic unit of reusable hardware.

Module Definitions
------------------
Modules are declared using the `mod` keyword.

A module has a list of pins, a list of components, and a list of wires.

The following example is a `Buffer` module, which demonstrates all three parts:

.. literalinclude:: examples/buffer.bitsy
   :language: bitsy
   :linenos:

This module has two ports, an incoming port `in` and and outgoing port `out`.
Both carry a value of type `Word<1>` (that is, a bit) to and from the module.

This module contains one subcomponent: a `reg` (register) named `queue`.
It stores a value of type `Word<1>`.
The `reset` clause declares that the reset value of the register is `0w1`.
This is an 1-bit representation of the number 0.

The final two statements are wires.

The first wire, `queue <= in`, will connect the incoming port `in` to the register `queue`.
The syntax `<=` is a *latching* wire.
It is only used with `reg`\s, and it tells us that the register will latch the value of `in` on the next clock cycle.

The second wire, `out := queue`, will connect the register `queue` to the outgoing port `out`.
The syntax `:=` is a *direct* connect.
It is used with ports and with nodes,
and it tells us that the value of `queue` will be sent to the port `out` on the same clock cycle.

In Bitsy, clocks and resets are usually passed implicitly to a module.
There is no need to declare them for synchronous circuits.

Module Instances
----------------
Once a module is defined, it may be instantiated.

Let's assume the above definition for `Buffer` has been given.
We can use several buffers in a row to make a 4-bit shift register:

.. literalinclude:: examples/tutorial_shift_reg.bitsy
   :language: bitsy
   :linenos:

You can see the experssion `cat(buf3.out, buf2.out, buf1.out, buf0.out)` is used
to concatenate the output of each of the buffers together.
Since each of the four values is a single bit, the result is a `Word<4>`.

You can see at a glance that because the module has four latch wires (`<=`),
a value will take four cycles to get from `cin` to `cout`.

External Modules
----------------
In Bitsy, you can declare modules whose behavior is determined by the simulator.
These are called external modules.
They are declared with the `ext` keyword, together with their list of ports.

The following would be a declaration for an external module
which might log the value presented on the port `in` on every cycle.

.. code-block::

   ext mod Monitor {
        incoming in of Word<8>;
    }

These can be instantiated with the `mod` keyword just like a normal module.

.. literalinclude:: examples/tutorial_ext.bitsy
   :language: bitsy
   :linenos:
