Nettle
======
Nettle is a circuit simulator intended for use with Bitsy.

Example Circuit
---------------
Here is an example circuit in Nettle:

.. code-block:: nettle

    top {
        reg sum of Word<32> reset 0w32;

        mod counter {
            outgoing out of Word<32>;
            reg c of Word<32> reset 1w32;
            c <= c + 1w32;
            out := c;
        }

        ext monitor {
            incoming in of Word<32>;
        }

        sum <= sum + counter.out;
        monitor.in := sum;
    }

As you can see, the top of a Nettle circuit is labeled with `top` and contains a number of declarations.

* `outgoing`\s represent outgoing ports.
* `incoming`\s represent input ports.
* `node`\s represent intermediate values.
* `reg`\s represent registers.
* `mod`\s represent submodules.
* `ext`\s represent an external submodule or functional model.

Paths
-----
Each one of the above constructs defines one or more **paths**.
These are dotted expressions which let you reference pieces of the circuit.

The above example creates the following paths:

.. code-block:: none

  top                 // the circuit itself
  top.sum             // the register `sum`
  top.sum.set         // the input of the register `sum`
  top.sum             // the output of the register `sum`
  top.counter.out     // the output port `out` in the submodule `counter`
  top.counter.c.set   // the input port of the register `c` in the submodule `counter`
  top.counter.c       // the output of the register `c` in the submodule `counter`
  top.monitor         // the external module `monitor`
  top.monitor.in      // the input port `in` on the external module `monitor`

Terminals
---------
Certain paths carry values during simulation.
These are called **terminals**.

Of the paths above, the following are **terminals**:

.. code-block:: none

  top.sum.set
  top.sum
  top.counter.out
  top.counter.c.set
  top.counter.c
  top.monitor.in

The rules for terminals are:

* Each `incoming`, `outgoing`, and `node` creates a terminal.
* Each `reg` creates two terminals:

  * The one with the same name as the register itself is the current stored value.

  * The one which ends in `.set` is the current staged value
    to be latched on the next clock cycle.

Wires
-----
In Nettle, wires connect together terminals.

The left hand side is called the **target**.
It may be one of:

* the name of a `node`, `reg`, or `outgoing` in the same module.
* the name of a submodule dotted with a `node` or `incoming` of that sub module.
* the name of an external module dotted with one of its `incoming` ports.

When connecting to a `node` `incoming` or `outgoing`, you use the syntax `:=`.
When connecting to a `reg`, you use the syntax `<=`.
This syntactic distinction allows you to equationally reason about the circuit without remembering the types.
It helps you remeber that `reg`\s targeted in this way implicitly refer to the `set` terminal.
The arrow head of `<=` helps remind you that the value is "on its way" to setting the register.
See **Registers** below.

On the right hand side of the arrow, we have the **driver**.
This is an expression that continuously drives a signal to the target.

Every module in a circuit is responsible for wiring up its own terminals
and wiring its terminals to the ports of its children.

Expressions
-----------
Expressions represent combinational logic.

**Literals**

To represent integers of arbitrary bitwdith, we annotate constants with their bitwidth.
The literal `42w16` is a 16-bit integer with the value 42.

**References**

A reference is a way to access the value of a terminal.

Similar to the target of a wire, a reference may be one of:

* the name of a `node`, `reg`, or `incoming` in the same module
* the name of a submodule dotted with a `node` or `outgoing` of that submodule.
* the name of an external module dotted with one of its `outgoing` ports.

The difference with references is that references to `reg`\s implicitly refer to the `val` terminal instead of `set`.
See **Registers** below.

**Operations**

Some basic operations are supported:

* `||` or
* `&&` and
* `!` not
* `^` xor
* `==` equals
* `!=` different
* `<` less than
* `+` sum (wrapping)
* `-` difference (wrapping)

**Concatenation, Indexing, and Slicing**

You can concatenate words with the syntax `cat(w1, w2)`.
If `w1` is `Word<n>` and `w2` is `Word<m>`, the result is `Word<n + m>`.
The bits of `w1` become the high-order bits and `w2` become the lower bits.

You can index into a `Word<n>` with the syntax `w[0]`.
Note that we use a plain old literal and not `0w8` here.
This creates a static indexing of the word.
It has type `Word<1>`.

If you do use `0w8` or any value `i` of shape `Word<k>`,
you end up with dynamic indexing.
An out of bounds dynamic index results in `XXX`.

You can also slice a word with the syntax `w[8..4]`.
This will give you a `Word<4>` with the same result as if you had written
`cat(w[7], w[6], w[5], w[4])`.
Pay attention!
This might surprise you coming from Verilog, since `w[8]` is *not* included in the result.
However, experience from programming languages such as Python and Rust shows
that this is a very natural way to handle slicing
(albeit the ordering is reversed to align with the way we write out bitstrings).

**if statements**

`if` statements create muxes.

All `if` statements must have an `else` branch.
If you don't care what the value is for the `else` branch, you can use `X`.

**Undefined Values**

The is the undefined value is used for terminals which aren't being driven or
invalid values are driven (eg, when `Valid` where the invalid bit is set but a payload is given).

The undefined value is written `XXX`.

Any operation involving `XXX` will result in `XXX`.
This includes `if` statements when the condition or any branch is `XXX`.

DON'T USE `XXX`.

Ports
-----
Ports allow a module to communicate with its parent.
Values for the top-level input ports must be provided by the testbench during simulation.

Ports may also appear in ext modules and must match the portlist for the implementation.

Only `outgoing` ports in current module or `incoming` ports of a submodule may be the target of a `wire` statement.
This is reversed for reference expressions: only `outgoing` ports of a submodule
or `incoming` ports of the current module may be referenced.

Nodes
-----
Nodes represent a intermediate value.
They are useful for naming common subexpressions in a circuit.

Nodes may only be referenced in the module they are declared in.

Registers
---------
Registers are stateful components.

All `reg`\s start off with an `XXX` value.
They may optionally have a reset value supplied using the syntax `reg r reset 0w8;`.

Unlike `node`\s, `reg`\s create two terminals.
One is named the same as the `reg` and the other is appended to with `.set`.

When a `reg` appears as the target of a wire, it implies we are connecting to its `set` terminal.
When a `reg` appears in an expression, it implies we are connecting to its value terminal.

Registers may only be referenced in the module they are declared in.

Modules
-------
A module is introduced with the `mod` keyword.
All modules definitions are inline.
In this way, a Nettle circuit is always fully elaborated.

A module can contain `node`\s, `reg`\s, `ext`\s, and other `mod`\s.
They also contain wire statements to connect the terminals created through those.

External Modules
----------------
An external module is introduced with the `ext` keyword.
It may only contain `port` declarations.
It may not contain wire statements.

When a Nettle circuit is loaded into the simulator, a suitable implementation must be linked against each external module.
An implementation governs the behavior of:

* peeking a port to read its value
* poking a port to set its value
* responding to a clock tick
* responding to a reset signal

Combinational Loops
-------------------
Nettle checks for combinational loops before simulation.
Circuits with loops are rejected.

Note that if `r` is a `reg`, the wire `r <= r + 1w8` is not a combinatorial loop,
since the `r` on the left hand side implies the `set` terminal while the `r` on the right hand side implies the current value terminal.

Connectivity
------------
All terminals must be given a single driver in a circuit.

Nets
----
A net is a collection of terminals, one of which is the driver.
On any clock cycle, all of the terminals in a net have the same value.

The nets of a Nettle circuit can be calculated by looking at the wire statements.
Whenever a wire statement has on its driver (the right-hand side).
If the driver is a reference, this indicates the two terminals are part of the same net.

Because a terminal can only appear as a target (the left hand side) of a connect statement once in a circuit,
we can create a tree of which terminals drive which terminals.
The root of this tree is the driver of the net.
Each net can be uniquely identified by its driver.

Combinational logic and registers separate the nets from one another.
