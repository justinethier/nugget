all: static rtl libs
static:
	gcc -c -o lib2.o lib2.c
	gcc -c -o lib1.o lib1.c
	gcc -c -o main.o main.c
	gcc -o main main.o lib1.o lib2.o
rtl:
	gcc -o main-rtl main-rtl.c -ldl
#	gcc -rdynamic -o main-rtl main-rtl.c -ldl
libs:
	gcc -c -fPIC -o lib2.o lib2.c
	gcc -shared -Wl,-soname,lib2.so -o lib2.so lib2.o
	gcc -c -fPIC -o lib1.o lib1.c
	gcc -shared -Wl,-soname,lib1.so lib1.o -L. -l2 -o lib1.so
clean:
	rm -rf *.o *.so main main-rtl
