cd out
UWRAP "-l ada -w `pwd`/../test.wrp -P `pwd`/../src/p.gpr `pwd`/../src/spec.ads"
cd ../user
gprbuild -q
./main
