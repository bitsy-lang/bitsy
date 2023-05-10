Bitsy
=====
Bitsy is a modern hardware description language.

Notes
-----
* It would be nice if you could specify a default value for a struct/enum type to optionally use in a register.
* Fin<n> and Byte types
* Do ports go on their own domains?


Port decls?
-----------

    port Wishbone
        aligned addr        : Valid<Word<32>>
        flipped read_data   : Valid<Word<32>>
        aligned write_data  : Valid<Word<32>>
        aligned cycle       : Bit
        aligned strobe      : Bit
        aligned sel         : Word<4>
    end


Convenience syntax for reading/saving to registers?
---------------------------------------------------

    reg foo of Word<8>

    wire foo.set <= ...
    set foo <= ...

... and letting the use of a register just act like a variable in expressions.


Ideas
-----
* Vecs should be parameterized by the index type (which could be an artibrary shape)
* Shapes should have both bitwidth and number of elements, since this is easy to calculate.
* Remove Eq from heavy data structures that should just be ptr-eq'ed
