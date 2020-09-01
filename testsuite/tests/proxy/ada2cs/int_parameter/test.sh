cd out
UWRAP "-l json -w `pwd`/../test.wrp `pwd`/../../../ada2proxy/int_parameter/out/*.json"
cd ../user
csc -nologo ../out/*.cs *.cs
PATH=`pwd`/../../../ada2proxy/int_parameter/user/proxy/lib:$PATH ./main.exe
