User-defined Shapes
===================
There are two classes of user-defined shapes in Bitsy.

Structs
-------
You can define your own aggregate shapes using the `struct` keyword:

.. code-block:: bitsy

    struct shape Packet
        field packet_type of PacketType
        field data of Word<8>
    end

Enums
-----
You can define your own enumerations and tagged union shapes using the `enum keyword`:

.. code-block:: bitsy

    enum shape State
        @Idle
        @Processing of Tuple<Word<8>, Bit>
        @Done of Word<8>
    end
