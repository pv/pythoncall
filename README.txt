==========
Pythoncall
==========

:author: Pauli Virtanen <pav@iki.fi>
:version: 0.3.1

Python embedded inside Matlab.

Conversion of basic types + numeric array types between Python and Matlab
is provided.


Usage
-----

- Evaluate python string::

      pythoncall('eval', command)

  ``command`` is a string containing the Python program to run.
  It is run in the __main__ module.

- Get variable values from the Python space::

      [value1, value2, ...] = pythoncall('get', varname1, varname2, ...)

  This gets variables from the top-level of __main__ module.

- Set variable values to the Python space::

      pythoncall('set', varname1, value1, varname2, value2, ...)

  This sets variables to the top-level of __main__ module.


Compilation (important!)
------------------------

.. warning::

   This is apparently outdated! Pythoncall may require some modifications
   before it will work with recent Matlab versions.

Note that Matlab apparently ``dlopens`` its extensions with RTLD_GLOBAL
disabled. This means that if you load the Python interpreter inside
Matlab without extra tricks, any extensions modules loaded by Python
will not be able to see Python symbols. Which means that they do not
work, which is especially annoying since you surely would like to use
for example numpy_.

To work around this nasty limitation, ``pythoncall.c`` has an equally nasty
hack: it explicitly loads symbols in the Python library to its own
"namespace", by ``dlopening`` the Python library file. To do this,
it needs to know the location of Python's ``libpython2.X.so`` file.
Have a look at the ``Makefile`` and fix the location of ``LIBPYTHON``
to match your version of Python.

While you're at it, also change ``-DNUMPY`` to ``-DNUMARRAY`` or
``-DNUMERIC`` if you use one of the earlier array libraries of Python.

.. _numpy: http://www.scipy.org/


Type conversion
---------------

The whole point of having Python inside Matlab is to have easy
conversion of variables. In ``pythoncall`` this goes as follows:

=================== ========================================================
From                To
=================== ========================================================
Python int          Matlab 1x1 int32
Python float        Matlab 1x1 double
Python complex      Matlab 1x1 double complex
Python bool         Matlab 1x1 logical
Python dict         Matlab 1x1 struct array. Field names: repr(key)
Python object array Matlab cell array
Python array        Matlab array
Python sequence     Matlab 1xN cell array
Python None         Matlab empty cell array
Python other        Matlab empty cell array + warning
------------------- --------------------------------------------------------
Matlab cell array   Python list if one-dim, otherwise python object array
Matlab char array   Python string if one-dim, otherwise python numeric array
Matlab struct array Python dict. If 1x1, values are converted directly.
                    If not scalar, values are python arrays/obj-arrays,
                    same size as the struct.
Matlab array        Python array
Matlab other        Python None + warning
=================== ========================================================

