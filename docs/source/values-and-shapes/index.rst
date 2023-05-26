Values and Shapes
=================
Bitsy separates out the world of "hardware components" from "hardware values".

Hardware components are the pieces of a Bitsy design which would come to correspond to a physical piece of silicon.
These include things like registers, module instances, and memories.

Hardware values only exist at simulation time.
A Bitsy design explains how values are transformed, but you never have direct access to one.

Every hardware value has a shape.

.. toctree::
  :hidden:

  builtin-shapes
  user-defined
