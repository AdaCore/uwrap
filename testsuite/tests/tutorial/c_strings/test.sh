cd out
cp -r ../../../../../documentation/source/tutorial/c_strings/* .
mkdir obj
UWRAP "-l ada -w ./c_strings.wrp -P prj.gpr src/test_h.ads"
cp *.ad* src
gprbuild -q -P prj.gpr
./obj/main
