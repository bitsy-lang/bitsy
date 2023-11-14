Getting Started
===============

A basic passthrough module in Bitsy looks like this:

.. code-block:: bitsy

    pub mod Passthrough
        incoming in  of Bit
        outgoing out of Bit

        wire io.out <= io.in
    end

The `mod Passthrough` defines a new module named `Passthrough`.

The `pub` keyword in the front marks this module as public.
Public modules may be used as "top" modules.
They will also be preserved when transpiling to Verilog,
and during simulation, all items marked `pub` will be preserved.

The next two lines define the pins.
Terminals are the pins that stick out of a module,
and can be used to send data in and out.

The first is an incoming pin named `in`.
The second, naturally, is an outgoing pin named `out`.
Both are 1-bit in size, and so we give it the shape `Bit`.

After that, we have the body of the module.
The `wire` statement will connect the input pin `io.in` to the output pin `io.out`.
The `io` refers to the pins for the module.

The module declaration ends with the `end` keyword.
