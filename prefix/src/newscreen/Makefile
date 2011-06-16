all: newscreen

clean:
	rm -f *.o newscreen

newscreen: newscreen.c
	gcc -arch x86_64 -o newscreen -g -I/usr/local/include -L/usr/local/lib newscreen.c -framework ApplicationServices

.PHONY: all clean
