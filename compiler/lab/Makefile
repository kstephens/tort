tort_calls.o : tort_calls.s
	gcc --verbose -fPIC -DPIC -D__DYNAMIC__ -export-dynamic -fno-common -I ../../core/include -c -o $@ $<
tort_calls.s : tort_calls.c
	gcc --verbose -fPIC -DPIC -D__DYNAMIC__ -export-dynamic -fno-common -I ../../core/include -S -o $@ $<
