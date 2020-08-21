cd out
UWRAP "-l ada -w `pwd`/../test.wrp `pwd`/../src/*.ads"
cd ../user/proxy
gprbuild -q -P proxy
