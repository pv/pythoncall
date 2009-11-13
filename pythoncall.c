/**
 * ==========
 * Pythoncall
 * ==========
 *
 * :author: Pauli Virtanen <pav@iki.fi>
 *
 * Python embedded inside Matlab.
 *
 * Conversion of basic types + numeric array types between Python and Matlab
 * is provided.
 */

/*
 * Copyright (c) 2006 Pauli Virtanen
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * a. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * b. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * c. Neither the name of the copyright holder nor the names of
 *    the contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

static const char* __version__ = "0.3.1";

/*****************************************************************************/
/** Libs
 **/

#include <Python.h>
#include <compile.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <mex.h>

#ifdef NUMERIC
#include "Numeric/arrayobject.h"
#endif
#ifdef NUMARRAY
#include "numarray/arrayobject.h"
#include "numarray/libnumarray.h"
#endif
#ifdef NUMPY
#include "numpy/arrayobject.h"
#endif

#ifndef LIBPYTHON
#define LIBPYTHON "libpython2.3.so"
#endif

#ifndef LP64
#if defined(SIZEOF_VOID_P)
#if SIZEOF_VOID_P == 8
#define LP64 1
#else
#define LP64 0
#endif
#else
#define LP64 0
#endif
#endif

#ifdef DEBUG
#define _MSG mexWarnMsgTxt
#else
void _MSG(char *a, ...) {}
#endif


/**
 * Evil dlopen hack
 * ----------------
 *
 * Matlab apparently calls its MEX with RTLD_GLOBAL disabled.  Hence, symbols
 * dlopened by the MEX are restricted to per-submodule scope.  To make any
 * libraries imported by the Python interpreter to see the symbols in the
 * interpreter, we must manually import its library here!
 */
#ifndef NO_DLFCN_HACK
#include <dlfcn.h>
static int dlopen_hacked = 0;
int dlopen_python_hack()
{
    if (!dlopen_hacked) {
        dlopen(LIBPYTHON, RTLD_NOW|RTLD_GLOBAL);
        dlopen_hacked = 1;
    }
}
#else
#define dlopen_python_hack()
#endif


/*****************************************************************************/
/** Interpreter setup
 **/

struct _interpreter {
    int initialized;
    int array_imported;
    int session_key;
};
typedef struct _interpreter interpreter_t;


static interpreter_t interpreter = { 0, 0, 0 };

/**
 * Start up the Python interpreter if needed, importing all necessary stuff.
 */
void interpreter_initialize()
{
    if (!interpreter.initialized) {
        void *handle;
        void (*f)();

        dlopen_python_hack(); /* necessary evil :( */
        Py_Initialize();

        if (!interpreter.array_imported) {
#ifdef NUMERIC
            import_array();
#endif
#ifdef NUMARRAY
            import_libnumarray();
            import_libnumeric();
#endif
#ifdef NUMPY
            import_array();
#endif
        }
        interpreter.initialized = 1;
    }
}

/**
 * Put away the Python interpreter
 */
void interpreter_finalize()
{
    if (interpreter.initialized) {
        Py_Finalize();
        interpreter.initialized = 0;
    }
}

/**
 * Run a string in the Python interpreter, in __main_%d__.
 *
 * :param command: Python command to execute
 */
void interpreter_run(const char *command)
{
    char modname[1024] = "__main__";
    PyObject *m, *d, *v = NULL;
    PyCompilerFlags flags;
    flags.cf_flags = CO_FUTURE_DIVISION;
    m = PyImport_AddModule(modname);
    if (m == NULL)
        goto error;
    d = PyModule_GetDict(m);
    v = PyRun_StringFlags(command, Py_file_input, d, d, &flags);
    if (v == NULL) goto error;
    Py_DECREF(v);
    return;

 error:
    if (v) {
        Py_DECREF(v);
    }
    PyErr_Print();
    mexErrMsgTxt("Python error");
    return;
}

/**
 * Get a variable from __main__
 *
 * :param varname: Name of the variable to return
 * :return: Object, or NULL if it doesn't exist
 */
PyObject *interpreter_get(const char* varname)
{
    PyObject *m, *d, *result = NULL;
    m = PyImport_AddModule("__main__");
    if (m == NULL) {
        PyErr_Print();
        return NULL;
    }
    d = PyModule_GetDict(m);
    result = PyMapping_GetItemString(d, (char*)varname);
    if (!result) {
        PyErr_Print();
        return NULL;
    }
    return result;
}

/**
 * Set a variable to __main__
 *
 * :param varname: Name of the variable to set
 * :param obj: Value to set
 */
void interpreter_set(const char* varname, PyObject *obj)
{
    PyObject *m, *d;
    m = PyImport_AddModule("__main__");
    if (m == NULL) {
        PyErr_Print();
        return;
    }
    d = PyModule_GetDict(m);
    if (PyMapping_SetItemString(d, (char*)varname, obj) == -1) {
        PyErr_Print();
        return;
    }
}


/*****************************************************************************/
/** Handling strided arrays
 **/

struct _stride
{
    /** Number of dimensions */
    int nd;
    /** Positions at each dimension */
    char **c;
    /** End positions for each dimension */
    char **end;
    /** Offsets of end positions for each dimension */
    long *end_offset;
    /** Strides for each dimension */
    const int* strides;
};
typedef struct _stride stride_t;

/** Initialize stepping through a strided array */
int stride_init(char* addr, int nd, const int* dims,
                const int* strides, stride_t *s)
{
    int i;
    s->nd = nd;
    s->c = mxCalloc(nd, sizeof(char*));
    s->end = mxCalloc(nd, sizeof(char*));
    s->end_offset = mxCalloc(nd, sizeof(long));
    s->strides = strides;
    for (i = 0; i < nd; ++i) {
        s->c[i] = addr;
        s->end_offset[i] = strides[i]*dims[i];
        s->end[i] = addr + s->end_offset[i];
        if (dims[i] < 0) {
            mexErrMsgTxt("Negative dimension encountered, aborting!");
            return;
        } else if (dims[i] == 0) {
            return 0;
        }
    }
    return 1;
}

/** Step the first index by one, wrapping around if necessary */
int stride_step(stride_t* s)
{
    int i, j;
    s->c[0] += s->strides[0];
    for (i = 0; i < s->nd; ++i) {
        if (s->c[i] != s->end[i])
            break;
        else if (i+1 == s->nd)
            return 0;
        else {
            s->c[i+1] += s->strides[i+1];
        }
    }
    for (j = i-1; j >= 0; --j) {
        s->c[j] = s->c[j+1];
        s->end[j] = s->c[j+1] + s->end_offset[j];
    }
    return 1;
}

/** Return the current position */
char* stride_pos(stride_t* s)
{
    return s->c[0];
}

/**
 * Copy a strided array to a continuous one. Elements in source and destination
 * arrays must be of the same size -- no conversion is done here.
 *
 * :param from: Pointer where to copy from
 * :param nd: Number of array dimensions
 * :param dims: Size of each array dimensions
 * :type dims: int[nd]
 * :param strides: Strides for each dimension. Can be also negative.
 * :type strides: int[nd]
 * :param to: Pointer where to copy to
 * :param to2:
 *     Pointer where to copy an element immediately following the previous one.
 * :param tstride:
 *     Size of the elements in `to` (and `to2`). Must be the same as the
 *     size of elements in `from`!
 */
void copy_to_contiguous(char *from, int nd, int *dims, int *strides,
                        char *to, char* to2, int tstride)
{
    char *p, *p2, *r;
    stride_t s;

    if (!stride_init(from, nd, dims, strides, &s)) return;
    
    p = to;
    p2 = to2;

    do {
        r = stride_pos(&s);
        
        memcpy(p, r, tstride);
        p += tstride;

        if (p2) {
            memcpy(p2, r + tstride, tstride);
            p2 += tstride;
        }
    } while (stride_step(&s));
}

/**
 * Copy a continuous array to a strided one. Elements in source and destination
 * arrays must be of the same size -- no conversion is done here.
 *
 * :param to: Pointer where to copy from
 * :param nd: Number of array dimensions
 * :param dims: Size of each array dimensions
 * :type dims: int[nd]
 * :param strides: Strides for each dimension. Can be also negative.
 * :type strides: int[nd]
 * :param from: Pointer where to copy to
 * :param from2:
 *     Pointer where from copy an element immediately following the previous
 *     one.
 * :param tstride:
 *     Size of the elements in `to` (and `to2`). Must be the same as the
 *     size of elements in `from`!
 */
void copy_from_contiguous(char *to, int nd, int *dims, int *strides,
                          char *from, char* from2, int tstride)
{
    char *p, *p2, *r;
    stride_t s;

    if (!stride_init(to, nd, dims, strides, &s)) return;

    p = from;
    p2 = from2;
    
    do {
        r = stride_pos(&s);
        
        memcpy(r, p, tstride);
        p += tstride;

        if (p2) {
            memcpy(r + tstride, p2, tstride);
            p2 += tstride;
        }
    } while (stride_step(&s));
}



/*****************************************************************************/
/** Type conversion: Python -> Matlab
 **/

mxArray *mx_from_py(PyObject* obj);
mxArray *mx_from_py_unknown(PyObject* obj);

/** Convert Python integer to Matlab integer
 * :param obj: Object to convert [Borrow reference]
 */
mxArray *mx_from_py_int(PyObject* obj)
{
    mxArray *r;
    int dims[1] = { 1 };
    r = mxCreateNumericArray(1, dims, mxINT32_CLASS, mxREAL);
    *((long*)mxGetData(r)) = PyInt_AS_LONG(obj);
    return r;
}

/** Convert Python bool to Matlab bool
 * :param obj: Object to convert [Borrow reference]
 */
mxArray *mx_from_py_bool(PyObject* obj)
{
    mxArray *r;
    int dims[1] = { 1 };
    r = mxCreateNumericArray(1, dims, mxLOGICAL_CLASS, mxREAL);
    if (PyObject_Compare(obj, Py_True) == 0) {
        *((char*)mxGetData(r)) = 1;
    } else {
        *((char*)mxGetData(r)) = 0;
    }
    return r;
}

/** Convert Python float to Matlab double
 * :param obj: Object to convert [Borrow reference]
 */
mxArray *mx_from_py_float(PyObject* obj)
{
   mxArray *r;
   int dims[1] = { 1 };
   r = mxCreateNumericArray(1, dims, mxDOUBLE_CLASS, mxREAL);
   *((double*)mxGetData(r)) = PyFloat_AS_DOUBLE(obj);
   return r;
}

/** Convert Python complex to Matlab double complex
 * :param obj: Object to convert [Borrow reference]
 */
mxArray *mx_from_py_complex(PyObject* obj)
{
    Py_complex c;
    mxArray *r;
    int dims[1] = { 1 };
    c = PyComplex_AsCComplex(obj);
    r = mxCreateNumericArray(1, dims, mxDOUBLE_CLASS, mxCOMPLEX);
    *mxGetPr(r) = c.real;
    *mxGetPi(r) = c.imag;
    return r;
}

/** Convert Python sequence to 1D Matlab cell array
 * :param obj: Object to convert [Borrow reference]
 */
mxArray *mx_from_py_sequence(PyObject* obj)
{
    mxArray *r;
    int dims[1];
    int k;
    dims[0] = PySequence_Size(obj);
    r = mxCreateCellArray(1, dims);
    for (k = 0; k < dims[0]; ++k) {
        PyObject *o;
        o = PySequence_GetItem(obj, k);
        if (o == NULL) {
            mexWarnMsgTxt("Couldn't get item in sequence");
            PyErr_Clear();
        } else {
            mxSetCell(r, k, mx_from_py(o));
            Py_DECREF(o);
        }
    }
    return r;
}

/** Convert Python string to Matlab char array
 * :param obj: Object to convert [Borrow reference]
 */
mxArray *mx_from_py_string(PyObject* obj)
{
    mxArray *r;
    int dims[2];
    char *buf;
    int len;
    mxChar* p;

    PyString_AsStringAndSize(obj, &buf, &len);
    dims[0] = 1;
    dims[1] = len;
    r = mxCreateCharArray(2, dims);
    p = mxGetData(r);
    for (; len > 0; --len) {
        *p = *buf;
        ++p; ++buf;
    }
    return r;
}

/**
 * Dump a __repr__ of the given object to warnings. Useful for debugging.
 * :param msg: Text to show before the dump
 * :param obj: The object whose __repr__ to dump [borrow reference]
 */
void dump_repr(const char *msg, PyObject* obj)
{
    char *buf;
    mexWarnMsgTxt(msg);
    PyObject *repr = PyObject_Repr(obj);
    buf = PyString_AsString(repr);
    mexWarnMsgTxt(buf);
    Py_DECREF(repr);
}

/**
 * Convert Python dict to 1x1 Matlab struct array.
 *
 * :param obj: Object to convert [Borrow reference]
 *
 * :note: If keys are not strings, their __repr__ is used instead!
 *        Fields that fail to convert to strings are silently skipped.
 *        Also, strings are chopped off at \x00 characters.
 */
mxArray *mx_from_py_dict(PyObject* obj)
{
    mxArray *r;
    int dims[1] = { 1 };
    PyObject *items;
    int nitems;
    int k;
    char *buf;
    int len;
    char **fieldnames;
    PyObject *repr = NULL;
    
    items = PyDict_Items(obj);
    if (!items) goto error;

    nitems = PyList_Size(items);
    fieldnames = mxCalloc(nitems, sizeof(char*));

    for (k = 0; k < nitems; ++k) {
        PyObject *o;
        
        o = PyList_GetItem(items, k);
        if (o == NULL) goto error;

        o = PyTuple_GetItem(o, 0);
        if (o == NULL) goto error;

        if (PyString_Check(o)) {
            PyString_AsStringAndSize(o, &buf, &len);
        } else {
            repr = PyObject_Repr(o);
            if (repr == NULL)
                continue; /* ... FIXME */
            buf = PyString_AsString(repr);
            len = strlen(buf);
        }

        fieldnames[k] = mxCalloc(len + 1, sizeof(char));
        memcpy(fieldnames[k], buf, len);
        fieldnames[k][len] = '\0';

        if (repr) {
            Py_DECREF(repr);
            repr = NULL;
        }
    }

    r = mxCreateStructArray(1, dims, nitems, (const char**)fieldnames);
    if (!r) goto error;

    for (k = 0; k < nitems; ++k) {
        PyObject *o;
        o = PyList_GetItem(items, k);
        if (o == NULL) goto error;

        o = PyTuple_GetItem(o, 1);
        if (o == NULL) goto error;

        mxSetFieldByNumber(r, 0, k, mx_from_py(o));
    }

    Py_DECREF(items);
    
    return r;

 error:
    PyErr_Clear();
    if (items) {
        Py_DECREF(items);
    }
    return mx_from_py_unknown(obj);
}

/**
 * Convert unknown python object to empty cell array.
 * Also send a warning with __repr__ of `obj` shown.
 *
 * :param obj: Object to convert [Borrow reference]
 */
mxArray *mx_from_py_unknown(PyObject* obj)
{
    int dims[1] = { 0 };
    char *buf;

    PyObject *type = PyObject_Type(obj);
    PyObject *repr = PyObject_Repr(type);
    buf = PyString_AsString(repr);
    
    mexWarnMsgTxt("Unknown Python object seen / conversion failed:");
    mexWarnMsgTxt(buf);

    Py_DECREF(repr);
    Py_DECREF(type);
    
    return mxCreateCellArray(1, dims);
}

#if defined(NUMERIC) || defined(NUMARRAY) || defined(NUMPY)
/** Convert a numeric Python object array to a Matlab cell array
 * :param obj: Object to convert [Borrow reference]
 */
mxArray *mx_from_py_arrayobject_object(PyArrayObject* obj)
{
    stride_t s;
    unsigned long index;
    PyObject **r;
    mxArray *arr;
    
    arr = mxCreateCellArray(obj->nd, (int*)obj->dimensions);
    if (!stride_init(obj->data, obj->nd, obj->dimensions, obj->strides, &s))
        return arr;

    index = 0;
    do {
        r = (PyObject**)stride_pos(&s);
        mxSetCell(arr, index, mx_from_py(*r));
        ++index;
    } while (stride_step(&s));

    return arr;
}

/** Convert a numeric Python array to Matlab array of the same data type
 * :param obj: Object to convert [Borrow reference]
 */
mxArray *mx_from_py_arrayobject(PyObject* obj_)
{
    PyArrayObject *obj = (PyArrayObject*)obj_;
    mxArray *r;
    mxClassID class;
    mxComplexity complexity;
    int stride;
    char *p;
    char *ip, *rp;
    int k, dim;
    int dummy_dim[2];

    switch (obj->descr->type_num) {
    case PyArray_CHAR:
    case PyArray_UBYTE:
        class=mxUINT8_CLASS;  complexity=mxREAL;
        break;
#ifdef NUMPY
    case NPY_BYTE:
#else
    case PyArray_SBYTE:
#endif
        class=mxINT8_CLASS;   complexity=mxREAL;
        break;
    case PyArray_SHORT:
        class=mxINT16_CLASS;  complexity=mxREAL;
        break;
    case PyArray_USHORT:
        class=mxUINT16_CLASS; complexity=mxREAL;
        break;
#if LP64
    case PyArray_LONG:
        class=mxINT64_CLASS; complexity=mxREAL;
        break;
#else
    case PyArray_LONG:
#endif
    case PyArray_INT:
        class=mxINT32_CLASS;  complexity=mxREAL;
        break;
    case PyArray_UINT:
        class=mxUINT32_CLASS;  complexity=mxREAL;
        break;
    case PyArray_FLOAT:
        class=mxSINGLE_CLASS; complexity=mxREAL;
        break;
    case PyArray_DOUBLE:
        class=mxDOUBLE_CLASS; complexity=mxREAL;
        break;
    case PyArray_CFLOAT:
        class=mxSINGLE_CLASS; complexity=mxCOMPLEX;
        break;
    case PyArray_CDOUBLE:
        class=mxDOUBLE_CLASS; complexity=mxCOMPLEX;
        break;
    case PyArray_OBJECT:
        return mx_from_py_arrayobject_object(obj);
#ifdef NUMPY
    case PyArray_STRING:
        /* 0d-string arrays */
        if (obj->nd == 0) {
            mxChar *cp;
            int len;
            char buf[1024];
            
            dummy_dim[0] = 1;
            dummy_dim[1] = obj->descr->elsize;
            
            r = mxCreateCharArray(2, dummy_dim);

            cp = mxGetData(r);
            p = obj->data;

            for (len = dummy_dim[1]; len > 0; --len) {
                *cp = *p;
                ++cp; ++p;
            }
            return r;
        } else {
            return mx_from_py_unknown(obj_);
        }
        break;
#endif
    default:
        return mx_from_py_unknown(obj_);
    }

    if (obj->nd == 0) {
        /* array scalar */
        dummy_dim[0] = 1;
        r = mxCreateNumericArray(1, dummy_dim, class, complexity);
        if (complexity == mxCOMPLEX) {
            memcpy(mxGetData(r),
                   obj->data, obj->descr->elsize/2);
            memcpy(mxGetImagData(r),
                   obj->data+obj->descr->elsize/2, obj->descr->elsize/2);
        } else {
            memcpy(mxGetData(r), obj->data, obj->descr->elsize);
        }
        return r;
    } else {
        r = mxCreateNumericArray(obj->nd, obj->dimensions, class, complexity);
    }

    stride = mxGetElementSize(r);

    if (complexity == mxCOMPLEX) {
        copy_to_contiguous(obj->data, obj->nd, obj->dimensions, obj->strides,
                           mxGetData(r), mxGetImagData(r), stride);
    } else {
        copy_to_contiguous(obj->data, obj->nd, obj->dimensions, obj->strides,
                           mxGetData(r), NULL, stride);
    }

    return r;
}
#endif

/** Convert Python none to an empty cell array
 * :param obj: Object to convert [Borrow reference]
 */
mxArray *mx_from_py_none(PyObject* obj)
{
    int dims[1] = { 0 };
    return mxCreateCellArray(1, dims);
}

/**
 * Convert a Python object to a Matlab object.
 * 
 * Supports the following types:
 * - int, float, bool, dict, string, complex, sequence, None
 * - Numeric, NumArray or Numpy arrays
 *
 * :param obj: Object to convert [Borrow reference]
 */
mxArray *mx_from_py(PyObject* obj)
{
    if (PyInt_Check(obj))
        return mx_from_py_int(obj);
    else if (PyFloat_Check(obj))
        return mx_from_py_float(obj);
    else if (PyBool_Check(obj))
        return mx_from_py_bool(obj);
    else if (PyDict_Check(obj))
        return mx_from_py_dict(obj);
    else if (PyString_Check(obj))
        return mx_from_py_string(obj);
#if defined(NUMERIC) || defined(NUMARRAY) || defined(NUMPY)
    else if (PyArray_Check(obj))
        return mx_from_py_arrayobject(obj);
#endif
    else if (PyComplex_Check(obj))
        return mx_from_py_complex(obj);
    else if (PySequence_Check(obj))
        return mx_from_py_sequence(obj);
    else if (PyObject_Compare(obj, Py_None) == 0)
        return mx_from_py_none(obj);
    else
        return mx_from_py_unknown(obj);
}


/*****************************************************************************/
/** Type conversion: Matlab -> Python
 **/

PyObject *py_from_mx(const mxArray* arr);
PyObject *py_from_mx_unknown(const mxArray* arr);

int mx_is_string(const mxArray *a)
{
    return mxIsChar(a) && mxGetM(a) == 1;
}
char *string_from_mx(const mxArray* a, unsigned int *buflen, char* errmsg)
{
    char *buf;

    *buflen = mxGetM(a) * mxGetN(a) + 1;
    buf = mxCalloc(*buflen, sizeof(char));
    
    if (mxGetString(a, buf, *buflen) != 0) {
        if (errmsg == NULL) {
            mexErrMsgTxt("not a string");
        } else {
            mexErrMsgTxt(errmsg);
        }
    }

    return buf;
}

int mx_is_scalar(const mxArray *a)
{
    int nd;
    const int *dims;
    nd = mxGetNumberOfDimensions(a);
    dims = mxGetDimensions(a);

    return (nd == 1 && dims[0] == 1)
        || (nd == 2 && dims[0] == 1 && dims[1] == 1);
}


#define PY_OBJECT_ARRAY_FROM_MX(arr, obj, index, mxcall)        \
do {                                                            \
    int nd;                                                     \
    const int *dims;                                            \
    int *i;                                                     \
    stride_t s;                                                 \
    PyObject **r;                                               \
    PyArrayObject *obj_;                                        \
                                                                \
    nd = mxGetNumberOfDimensions(arr);                          \
    dims = mxGetDimensions(arr);                                \
                                                                \
    /* non-multidimensional arrays => lists */                  \
    if (nd == 1 || (nd == 2 && dims[0] == 1)) {                 \
        obj = PyList_New(nd == 1 ? dims[0] : dims[1]);          \
        for (index = 0; index < PyList_Size(obj); ++index) {    \
            mxArray *a;                                         \
            a = mxcall;                                         \
            PyList_SET_ITEM(obj, index, py_from_mx(a));         \
        }                                                       \
        break;                                                  \
    } else if (nd == 2 && (dims[0] == 0 || dims[1] == 0)) {     \
        obj = PyList_New(0);                                    \
        break;                                                  \
    }                                                           \
                                                                \
    /* multidimensional arrays => obj.arrays */                 \
    obj = PyArray_FromDims(nd, (int*)dims, PyArray_OBJECT);     \
    if (!obj)                                                   \
        goto _objarray_error;                                   \
    obj_ = (PyArrayObject*)obj;                                 \
    if (!stride_init(obj_->data, obj_->nd, obj_->dimensions,    \
                     obj_->strides, &s))                        \
        break;                                                  \
                                                                \
    index = 0;                                                  \
    do {                                                        \
        mxArray *a;                                             \
                                                                \
        r = (PyObject**)stride_pos(&s);                         \
                                                                \
        a = mxcall;                                             \
        if (a) {                                                \
            *r = py_from_mx(a);                                 \
        } else {                                                \
            *r = Py_None;                                       \
            Py_INCREF(Py_None);                                 \
        }                                                       \
                                                                \
        ++index;                                                \
    } while (stride_step(&s));                                  \
    break;                                                      \
 _objarray_error:                                               \
    if (obj) Py_DECREF(obj);                                    \
    mexWarnMsgTxt("Failed to convert cell array");              \
    obj = py_from_mx_unknown(arr);                              \
} while (0)



/** Matlab cell array to Numeric/numpy object arrays */
PyObject *py_from_mx_cell(const mxArray *arr)
{
    PyObject *obj = NULL;
    unsigned long index;

    PY_OBJECT_ARRAY_FROM_MX(arr, obj, index, mxGetCell(arr, index));
    return obj;
}

/** Matlab numeric array to Python numeric array */
PyObject *py_from_mx_numeric(const mxArray* arr)
{
    int nd;
    const int *dims;
    int *i;
    PyArrayObject *obj = NULL;
    int type = -1;

    switch (mxGetClassID(arr)) {
    case mxDOUBLE_CLASS:
        type = mxIsComplex(arr) ? PyArray_CDOUBLE : PyArray_DOUBLE;
        break;
    case mxSINGLE_CLASS:
        type = mxIsComplex(arr) ? PyArray_CFLOAT : PyArray_FLOAT;
        break;
    case mxINT8_CLASS:
    case mxINT16_CLASS:
    case mxINT32_CLASS:
    case mxINT64_CLASS:
        if (mxIsComplex(arr)) return py_from_mx_unknown(arr);
        switch (mxGetElementSize(arr)) {
#ifdef NUMPY
        case 1: type = PyArray_BYTE; break;
#else
        case 1: type = PyArray_SBYTE; break;
#endif
        case 2: type = PyArray_SHORT; break;
        case 4: type = PyArray_INT; break;
        default: return py_from_mx_unknown(arr);
        }
        break;
    case mxCHAR_CLASS:
    case mxLOGICAL_CLASS:
    case mxUINT8_CLASS:
    case mxUINT16_CLASS:
    case mxUINT32_CLASS:
    case mxUINT64_CLASS:
        if (mxIsComplex(arr)) return py_from_mx_unknown(arr);
        switch (mxGetElementSize(arr)) {
        case 1: type = PyArray_UBYTE; break;
        case 2: type = PyArray_USHORT; break;
        case 4: type = PyArray_UINT; break;
        default: return py_from_mx_unknown(arr);
        }
        break;
    default:
        return py_from_mx_unknown(arr);
    }

    nd = mxGetNumberOfDimensions(arr);
    dims = mxGetDimensions(arr);
    obj = (PyArrayObject*)PyArray_FromDims(nd, (int*)dims, type);
    if (!obj)
        goto error;

    if (mxIsComplex(arr)) {
        copy_from_contiguous(obj->data, obj->nd, obj->dimensions, obj->strides,
                             mxGetData(arr), mxGetImagData(arr),
                             mxGetElementSize(arr));
    } else {
        copy_from_contiguous(obj->data, obj->nd, obj->dimensions, obj->strides,
                             mxGetData(arr), NULL, mxGetElementSize(arr));
    }

    return (PyObject*)obj;

 error:
    if (obj) Py_DECREF(obj);
    mexWarnMsgTxt("Failed to convert cell array");
    return py_from_mx_unknown(arr);
}

/** Matlab char array to Python string etc. */
PyObject *py_from_mx_char(const mxArray* arr)
{
    char *buf;
    int buflen;
    PyObject *obj;

    buf = string_from_mx(arr, &buflen, "");
    obj = PyString_FromStringAndSize(buf, buflen-1); /* chop trailing \x00 */
    
    return obj;
}

/** Matlab 1x1 struct array to Python dict */
PyObject *py_from_mx_struct(const mxArray* arr)
{
    int nfields;
    int field_number;
    PyObject *obj;

    nfields = mxGetNumberOfFields(arr);

    obj = PyDict_New();

    for (field_number = 0; field_number < nfields; ++field_number) {
        const char *name;
        
        name = mxGetFieldNameByNumber(arr, field_number);

        if (mx_is_scalar(arr)) {
            mxArray *a;
            a = mxGetFieldByNumber(arr, 0, field_number);
            PyDict_SetItemString(obj, name, py_from_mx(a));
        } else {
            unsigned long index;
            PyObject* o;
            PY_OBJECT_ARRAY_FROM_MX(
                arr, o, index, mxGetFieldByNumber(arr, index, field_number));
            PyDict_SetItemString(obj, name, (PyObject*)o);
        }
    }
    
    return obj;
}

/** Matlab unknown array to Python array */
PyObject *py_from_mx_unknown(const mxArray* arr)
{
    mexWarnMsgTxt("Failed to convert unknown Matlab object to Python");
    Py_INCREF(Py_None);
    return Py_None;
}

/** Matlab object to Python object */
PyObject *py_from_mx(const mxArray* arr)
{

    if (mxIsCell(arr))
        return py_from_mx_cell(arr);
    else if (mx_is_string(arr))
        return py_from_mx_char(arr);
    else if (mxIsNumeric(arr) || mxIsLogical(arr))
        return py_from_mx_numeric(arr);
    else if (mxIsStruct(arr))
        return py_from_mx_struct(arr);
    else
        return py_from_mx_unknown(arr);
}


/*****************************************************************************/
/** Entry point
 **/

/**
 * Run a Python string in the interpreter.
 * Translate a return value and return it to Matlab.
 */
void mexFunction(int nlhs, mxArray *plhs[],
                 int nrhs, const mxArray *prhs[])
{
    unsigned int buflen;
    char* action;

    if (nrhs < 1) goto argument_error;
    if (!mx_is_string(prhs[0])) goto argument_error;

    mexAtExit(interpreter_finalize);
    interpreter_initialize();

    action = string_from_mx(prhs[0], &buflen, NULL);

    if (strcmp(action, "eval") == 0) {
        char *cmd;
        if (nlhs > 0) goto argument_error;
        if (nrhs != 2) goto argument_error;

        cmd = string_from_mx(prhs[1], &buflen, NULL);
        interpreter_run(cmd);
    } else if (strcmp(action, "get") == 0) {
        int i;
        PyObject *o;
        char *name;
        
        if (nlhs != nrhs - 1) goto argument_error;

        for (i = 0; i < nlhs; ++i) {
            name = string_from_mx(prhs[i+1], &buflen, NULL);
            o = interpreter_get(name);
            if (o) {
                plhs[i] = mx_from_py(o);
                Py_DECREF(o);
            }
        }
    } else if (strcmp(action, "set") == 0) {
        int i;
        char *name;
        PyObject *o;

        if (nlhs > 0) goto argument_error;
        if (nrhs-1 != 2*((int)((nrhs-1)/2))) goto argument_error;

        for (i = 1; i < nrhs; i += 2) {
            name = string_from_mx(prhs[i], &buflen, NULL);
            o = py_from_mx(prhs[i+1]);
            if (o) {
                interpreter_set(name, o);
                Py_DECREF(o);
            }
        }
    } else {
        goto argument_error;
    }

    return;

 argument_error:
    mexErrMsgTxt(
        "Invalid arguments.\n"
        "    Usage: pythoncall(action, *arguments)\n"
        "    Actions:\n"
        "      'get':  [x,y,...] = pythoncall('get', 'x', 'y', ...)\n"
        "      'set':  pythoncall('set', 'x', x, 'y', y, ...)\n"
        "      'eval': pythoncall('eval', 'print \"FOO\"')");
    return;
}
