Types
=====
Hardware components are the pieces of a Bitsy design which would come to correspond to a physical piece of silicon.
These include things like registers, ports, module instances, and memories.

At simulation time, values flow across hardware components.
The value can change over time, synchronously with the clock.

Every value has a type which tells you what kinds values can flow across a component.
Types also tell you what kinds of values may be stored in registers.

.. toctree::
  :maxdepth: 1

  builtin-types
  user-defined
