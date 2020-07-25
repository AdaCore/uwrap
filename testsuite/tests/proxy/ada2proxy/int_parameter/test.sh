cd out
UWRAP "-l ada -w `pwd`/../test.wrp `pwd`/../src/*.ads"
cd ../user/main
gprbuild -q -P main
PATH=../proxy/lib:PATH obj/main
