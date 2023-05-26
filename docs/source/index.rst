The Bitsy Hardware Language
===========================

.. toctree::
  :hidden:
  :maxdepth: 2

  getting-started
  values-and-shapes

Bitsy is a modern hardware description language.

It was created in response to a simple ask:
If my design typechecks, it should do what I expect.

The industry standard Verilog fails this by having very loose rules for when designs are malformed.
Some notable painpoints include:

* automatic truncation of data
* flop inference from incomplete case coverage
* tool-dependent rules for invalid wire connections

The other industry standard VHDL suffers from the fact the author of Bitsy doesn't know it and doesn't want to know it.

Repository: `https://github.com/bitsy-lang/bitsy <https://github.com/bitsy-lang/bitsy>`_
