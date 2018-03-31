wonderlisp
====

wonderlisp is a self-hosting Lisp-1 implementation.

**NOTICE**: wonderlisp is extremely slow!

Any [lispboot](https://github.com/yubrot/lispboot)-compatible Lisp implementation can run wonderlisp. wonderlisp itself is also [lispboot](https://github.com/yubrot/lispboot)-compatible.

    # Run Conway's Game of Life on ocalisp (slow)
    $ ./wonderlisp ../ocalisp/ocalisp lispboot/examples/conways-gol.lisp

    # Run REPL on wonderlisp on ocalisp (extremely slow)
    $ ./wonderlisp "./wonderlisp ../ocalisp/ocalisp"

