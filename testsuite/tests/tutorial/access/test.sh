cd out
cp -r ../../../../../documentation/source/tutorial/access/* .
mkdir obj
UWRAP "-l ada -w ./access.wrp -P prj.gpr src/test.adb"
