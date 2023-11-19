Nettle
======
Nettle is a circuit simulator intended for use with Bitsy.

Example Circuit
---------------
Here is an example circuit in Nettle:

.. code-block:: nettle

    top {
        reg sum reset 0w32;

        mod counter {
            port out;
            reg c reset 1w32;
            c <= c + 1w4;
            out <= c;
        }

        ext monitor {
            port out;
        }

        sum <= sum + counter.out;
        monitor.out <= sum;
    }

As you can see, the top of a Nettle circuit is labeled with `top` and contains a number of declarations.

* `ports`\s represent ports.
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
  top.sum.val         // the output of the register `sum`
  top.counter.out     // the port `out` in the submodule `counter`
  top.counter.c.set   // the input of the register `c` in the submodule `counter`
  top.counter.c.val   // the output of the register `c` in the submodule `counter`
  top.monitor         // the external module `monitor`
  top.monitor.in      // the port `in` on the external module `monitor`


Terminals
---------
Certain paths carry values during simulation.
These are called **terminals**.

Of the paths above, the following are **terminals**:

.. code-block:: none

  top.sum.set
  top.sum.val
  top.counter.out
  top.counter.c.set
  top.counter.c.val
  top.monitor.in

The rules for terminals are:

* Every `node` creates a terminal.
* Every `reg` creates two terminals.
  The one which ends in `.val` is the current stored value.
  The one which ends in `.set` is the current staged value,
  to be latched on the next clock cycle.

Wires
-----
In Nettle, wires are created with the `<=` syntax.
The left hand side is called the **target**.

The target may be one of:

* the name of a `node` in the same module
* the name of a `reg` in the same module
* the name of a submodule dotted with a `node`
* the name of an external module dotted with one of its `node`

`reg`\s targeted in this way implicitly refer to the `set` terminal.
See **Registers** below.

In this way, every module in a circuit is responsible for wiring up its own terminals
and wiring its terminals to the ports of its children.

On the right hand side of the `<=`, we have the **driver**.
This is an expression that continuously drives a signal to the target.

Expressions
-----------
Expressions represent combinational logic.

**Literals**

To represent integers of arbitrary bitwdith, we annotate constants with their bitwidth.
The literal `42w16` is a 16-bit integer with the value 42.

**References**

A reference is a way to access the value of a terminal.

Similar to the target of a wire, a reference may be one of:

* the name of a `node` in the same module
* the name of a `reg` in the same module
* the name of a submodule dotted with a `node`
* the name of an external module dotted with one of its `node`

The difference with references is that references to `reg`\s implicitly refer to the `val` terminal instead of `set`.
See **Registers** below.

**Operations**

Some basic operations are supported:

* `||` or
* `&&` and
* `!` not
* `==` equals
* `!=` different
* `<` less than
* `+` sum (wrapping)
* `-` difference (wrapping)

**if statements**

`if` statements create muxes.

All `if` statements must have an `else` branch.
If you don't care what the value is for the `else` branch, you can use `X`.

**X**

`X` is the undefined value.
Any operation involving an X will result in X.
This includes `if` statements when the condition or any branch is X.

Ports
-----
Ports allow a module to communicate with its parent.
Values for the top-level input ports must be provided by the testbench during simulation.

Ports may also appear in ext modules and must match the portlist for the implementation.

Nodes
-----
Nodes represent a intermediate value.
They are useful for naming common subexpressions in a circuit.

Registers
---------
Registers are stateful components.

All `reg`\s start off with an `X` value.
They may optionally have a reset value supplied using the syntax `reg r reset 0w8;`.

Unlike `node`\s, `reg`\s create two terminals.
One is named `val` and the other is `set`.
The `val` is the current value of the register, while `set` is the value to be latched on the next clock cycle.

When a `reg` appears as the target of a wire, it implies we are connecting to its `set` terminal.
When a `reg` appears in an expression, it implies we are connecting to its `val` terminal.

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
since the `r` on the left hand side implies the `set` terminal while the `r` on the right hand side implies the `val` terminal.
