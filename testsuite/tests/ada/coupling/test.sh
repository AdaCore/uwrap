cd out
UWRAP "-l ada -w `pwd`/../test.wrp -P `pwd`/../src/p.gpr `pwd`/../src/*.ads"
cd ../user
gprbuild -q -p
./obj/main
