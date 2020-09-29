cd out
cp -r ../../../../../documentation/source/tutorial/wrap_names/* .
mkdir obj
UWRAP "-l ada -w wrap_names.wrp -P prj.gpr src/some_package.ads"
cp *.ad* src
gprbuild -q -P prj.gpr
./obj/main

