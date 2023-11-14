Interfaces
==========
A major source of tedium in hardware languages is connecting all the modules together.
Interfaces are a tool that helps connect modules together at a higher-level of abstraction.

When two modules are connected through their interface, it is as if you had written several `wire` statements.

.. code-block::

    pub enum shape Command
        @None
        @Read(Word<32>)
        @Write(Word<32>, Word<8>)
    end

    pub interface Mem
        mosi command of Valid<Command>
        miso read    of Valid<Word<8>>
    end


Once an interface is defined, a module may implement it:

.. code-block::

    pub extern mod Core
        outgoing command of Command
        incoming read  of Valid<Word<8>>
    end

    pub extern mod Ram
        incoming command of Command
        outgoing read    of Valid<Word<8>>
    end

    master Core of Mem
    slave Ram of Mem

In this example, the names of the pins on `Core` and `Ram` must match the names of the pins on the interface exactly.
The directions must also match: for `miso` pins on the interface, the master must have an `incoming` pin, etc.

Finally, to connect two module instances together:

.. code-block::

    pub mod Top
        mod core of Core
        mod ram  of Ram

        connect ram to core as Mem
    end

This will be just as if you had written:

.. code-block::

    pub mod Top
        mod core of Core
        mod ram  of Ram

        wire ram.commnd <= core.command
        wire core.read <= ram.read
    end



If the names of the pins do not match exactly, the implementation may contain wiring to make them match:

.. code-block::

    pub extern mod Device
        incoming cmd of Command
        outgoing rd  of Valid<Word<8>>
    end

    slave Device of Mem {
        command as cmd
        read as rd
    }
