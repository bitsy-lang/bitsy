Principles
==========
There are few principles Bitsy adheres to in its design with respect to shapes.

Totality
--------
The term `total language`_ refers to a functional programming language where functions:

* may not have any "side effects"
* must be "well-defined" for all inputs
* may not throw errors
* may not get stuck in loops and always return a value after finite time

This concept is very natural in a much more limited context of hardware languages.
It is a good description of combinational logic.

Bitsy heavily leans into this total property:

* Operations must always be totally defined.
* All conditional expressions are required to have full case coverage.
* Indexing into a `Vec<n, S>` must be guaranteed to be in bounds.
* For operations which might fail, we use the `Valid<S>` shape.
* Observing the unset bits in an enum [#unset_bits]_.
* Bitsy's equivalent of Verilog's `X` values are handled in a principled way.

Layout
------
For the time being, the layout of shapes is not specified in the language.
We reserve the ability to layout any shape however the compiler likes,
with the intention this will eventually be exposed as a control during elaboration.
Similarly, the bitwidth of user-defined types are not guaranteed.

.. _total language: https://www.jucs.org/jucs_10_7/total_functional_programming/jucs_10_07_0751_0768_turner.pdf

Footnotes
---------
.. [#unset_bits] For example, you may not inspect the "payload" of a `Valid<S>` when the value is `@Invalid`.
