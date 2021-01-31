=============
Github Matrix
=============

This project's aim is to give all repositories tested by
Github Actions a way to show the full build status.

Especially it is useful when tests are running under
a "matrix" combinations.

Here is an example of the render, when matrix has only two variables: ``os`` and ``lisp implementation``:

.. code::

   matrix:
     lisp:
       - sbcl-bin
       - ccl-bin
     os:
       - ubuntu-latest
       - macos-latest

.. image:: docs/images/minimal.png


And this is how more complex matrix is rendered:

.. code::

   matrix:
     os:
       - ubuntu-latest
       - macos-latest
     quicklisp-dist:
       - quicklisp
       - ultralisp
     lisp:
       - sbcl-bin
       - ccl-bin
       - ecl
       - abcl
       - allegro
       - clisp
       - cmucl

.. image:: docs/images/complex.png

