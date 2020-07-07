cd out
gcc -c -fdump-ada-spec ../src/test.h
UWRAP "-l ada -w `pwd`/../test.wrp `pwd`/test_h.ads"
cd ../user
gprbuild -p -q
./obj/main
