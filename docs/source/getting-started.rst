Getting Started
===============

A basic passthrough module in Bitsy looks like this:

.. code-block:: bitsy

    pub mod Passthrough
        port io
            incoming in  of Bit
            outgoing out of Bit

        wire io.out <= io.in
    end

The `mod Passthrough` defines a new module named `Passthrough`.

The `pub` keyword in the front marks this module as public.
Public modules may be used as "top" modules.
They will also be preserved when transpiling to Verilog,
and during simulation, all items marked `pub` will be preserved.

The next line, `port io` defines a new port.

A module may have any number of ports.
Ports group the input and outputs of the module together,
allowing you to connect them together with a single line.
Generally speaking, you should have one port for every component that this module connects to.
But for simple modules, we use the name `io` by convention.

The next two lines declare the two pins inside of the `io` port.
The first is an incoming pin named `in`.
The second, naturally, is an outgoing pin named `out`.
Both are 1-bit in size, and so we give it the shape `Bit`.

After that, we have the body of the module.
The `wire` statement will connect the input pin `io.in` to the output pin `io.out`.

The module declaration ends with the `end` keyword.
