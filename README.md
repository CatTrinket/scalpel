Scalpel
-------

Scalpel is an ARM disassembler written in Haskell.

So far it only recognizes `B`, `BL`, and `BX` ARM instructions, and `B` Thumb
instructions, and will only actually disassemble ARM.  It will, however, follow
`B` and `BL` instructions and continue disassembling from there, which is
pretty cool.  I'd like to get it to follow *all* branch instructions and
disassemble the entire binary, which is why I've only been focusing on branch
instructions and program structure so far.
