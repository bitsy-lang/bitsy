Principles
==========
There are few principles Bitsy adheres to in its general design.


Simplicity
----------
Verilog is a kitchen sink language.
It has way too more features than anyone should care about.
Bitsy is simple and easy to learn.


Totality
--------
The term `total language`_ refers to a programming language where "function" is used in a strict mathematical sense.
This concept is very natural in the context of hardware languages,
since it is a good description of combinational logic.

Bitsy heavily leans into this total property:

* Operations must always be totally defined.
* All conditional expressions are required to have full case coverage.
* Indexing into a `Vec<n, S>` must be guaranteed to be in bounds.
* For operations which might fail, we use the `Valid<S>` shape.
* Observing the unset bits in an enum [#unset_bits]_.
* Bitsy's equivalent of Verilog's `X` values are handled in a principled way.


Layout
------
Outside of types annotated as such, bitsy does not guarantee the bit layout of its shapes.
This allows the compiiler total freedom in choosing a sutiable representation.


Digital
-------
Bitsy is oriented towards digital circuits.
Every module with a register operates on an implicit clock and reset domain.


Fun
---
Bitsy is a fun language to write in.
Otherwise, what's the point?


.. _total language: https://www.jucs.org/jucs_10_7/total_functional_programming/jucs_10_07_0751_0768_turner.pdf

Footnotes
---------
.. [#unset_bits] For example, you may not inspect the "payload" of a `Valid<S>` when the value is `@Invalid`.
