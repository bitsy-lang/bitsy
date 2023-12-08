Principles
==========
There are few principles Bitsy adheres to in its general design.


Simplicity
----------
Bitsy is simple and easy to learn.
It has a clean syntax and excellent developer tooling.


Strongly Typed
--------------
Mistakes in hardware can be expensive.
While Bitsy is primarily aimed at hobbyist FPGA designs,
it still detracts from the experience whenever a preventable error occurs.

Bitsy's strong type systems is designed so that if your circuit synthesizes, it works.

The expression sublanguage is a `total language`_:

* Operations must always be totally defined.
* All conditional expressions are required to have full case coverage.
* Indexing into a `Vec<T, n>` must be guaranteed to be in bounds.
* You are prohibited from observing the unset bits in a value.
* Bitsy's equivalent of Verilog's `X` values are handled in a principled way.

.. * For operations which might fail, we use the `Valid<T>` type.

Bitsy also avoids many of the pitfalls of working in Verilog:

* No components may be left uninitialized.
* All nets must have a single driver.
* Unsynchronized clock domain crossings are illegal.

.. Layout
.. ------
.. Outside of types annotated as such, bitsy does not guarantee the bit layout of its shapes.
.. This allows the compiiler total freedom in choosing a sutiable representation.


Digital
-------
Bitsy is oriented towards digital circuits.
Every module with a register operates on an implicit clock and reset domain.


Synthesizable
-------------
Bitsy has a clear delineation in terms of what is synthesizable and what is not.
The translation to actual hardware is always reasonable.


Fun
---
Bitsy is a fun language to write in.
Otherwise, what's the point?


.. _total language: https://www.jucs.org/jucs_10_7/total_functional_programming/jucs_10_07_0751_0768_turner.pdf

.. Footnotes
.. ---------
.. .. [#unset_bits] For example, you may not inspect the "payload" of a `Valid<S>` when the value is `@Invalid`.
