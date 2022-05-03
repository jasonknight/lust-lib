# lust-lib

This is a toy implementation of a Lis.py style interpreter by Peter Norvig in Rust, hence: lust :D.

Lust is halfway between Lis.py and TinyScheme, that is it's built to be embedded in a binary, and to allow users to extend the features of the language through implementing Handlers.

It is somewhat inspired also by MRI by Yukihiro Matsumoto, who designed Ruby originally to execute on the tree instead of a bytecode virtual machine for simplicity as speed was not the goal, but elegance.

