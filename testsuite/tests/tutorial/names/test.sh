cd out
cp -r ../../../../../documentation/source/tutorial/names/* .
mkdir obj
UWRAP "-l ada -w names.wrp -P prj.gpr src/test.ads"
