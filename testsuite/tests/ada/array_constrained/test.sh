cd out
UWRAP "-l ada -I$ROOT/include/ -w `pwd`/../test.wrp -P `pwd`/../src/p.gpr `pwd`/../src/*.ads"
cd ../user
gprbuild -q
./obj/main
