Getting Started
===============

Tradition dictates that the first program that is written in any programming language should be Hello, World!
In that same spirit, let's look at the "hello world" of Bitsy: `Passthrough`:

.. literalinclude:: examples/passthrough.bitsy
   :language: bitsy
   :linenos:

We see a module declaration, `mod Passthrough`.
Inside, we see two ports, one `incoming` port named `out` and one `outgoing` port named `in`.
Both have type `Word[8]`, meaning that an 8-bit value passes across them.
The line `out := in` connects the input port `in` to the output port `out`.
The notation `:=` is called a wire.
