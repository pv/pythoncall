all: pythoncall.mexglx

LIBPYTHON=$(firstword $(wildcard /usr/lib/libpython2.4.so.*))

NUMPYINC=/usr/local/lib/python2.4/site-packages/numpy/core/include

CFLAGS=-g -DDEBUG -I/usr/include/python2.4 -I$(NUMPYINC) -DNUMPY -DLIBPYTHON='\"$(LIBPYTHON)\"'
LDFLAGS=-lpython2.4

pythoncall.mexglx: pythoncall.c
	mex $(CFLAGS) $^ $(LDFLAGS)

clean:
	rm -f pythoncall.o pythoncall.mexglx
