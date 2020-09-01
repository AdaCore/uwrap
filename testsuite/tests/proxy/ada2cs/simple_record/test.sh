cd out
UWRAP "-l json -w `pwd`/../test.wrp `pwd`/../../../ada2proxy/simple_record/out/*.json"
cd ../user
csc -nologo ../out/*.cs *.cs
PATH=`pwd`/../../../ada2proxy/simple_record/user/proxy/lib:$PATH ./main.exe
