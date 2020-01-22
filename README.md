wonderlisp
====

wonderlisp is a self-hosting [Rosetta Lisp](https://github.com/yubrot/rosetta-lisp) implementation.

__NOTICE__: wonderlisp is extremely slow!

Any [Rosetta Lisp](https://github.com/yubrot/rosetta-lisp) implementation can run wonderlisp. wonderlisp itself is also [Rosetta Lisp](https://github.com/yubrot/rosetta-lisp) compatible.

    # Run Conway's Game of Life on ocalisp (slow)
    $ ./wonderlisp ../ocalisp/ocalisp rosetta-lisp/examples/conways-gol.lisp

    # Run REPL on wonderlisp on ocalisp (extremely slow)
    $ ./wonderlisp "./wonderlisp ../ocalisp/ocalisp"

