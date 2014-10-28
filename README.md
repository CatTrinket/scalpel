arm-disassembler-in-haskell
---------------------------

Unsurprisingly, this is an ARM disassembler written in Haskell.  As soon as I
think of a better name, keep in mind that I'm going to rename it.

So far it only recognizes `B`, `BL`, and `BX` instructions, and no Thumb.  It
will, however, follow `B` and `BL` instructions and continue disassembling from
there, which is pretty cool.  I'd like to get it to follow *all* branch
instructions and disassemble the entire binary, which is why I've only been
focusing on branch instructions and program structure so far.
