all:
	gcc -c -o lib2.o lib2.c
	gcc -c -o lib1.o lib1.c
	gcc -c -o main.o main.c
	gcc -o main main.o lib1.o lib2.o
clean:
	rm -rf *.o main
