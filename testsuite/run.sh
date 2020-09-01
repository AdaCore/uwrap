function test () {
    echo `pwd`
    rm -rf out
    mkdir out
    if [ "$MODE" == "" ]; then
      ./test.sh > out/test.out 2>&1
      diff --strip-trailing-cr -u expected.out out/test.out 
    else
      ./test.sh
    fi
}

function test_dir () {
(
  if [ -d $1 ]; then
    if [ $1 != ".." ]; then
      if  [ $1 != "." ]; then
        cd $1

        if [ -f test.sh ]; then
          test
        elif [ -f run.sh ]; then
          ./run.sh
        else
          for j in *; do
            test_dir $j
          done
        fi
      fi
    fi
  fi
)
}

function UWRAP () {
  if [ "$MODE" == "" ]; then
    $ROOT/obj/uwrap $1
  elif [ "$MODE" == "gs" ]; then
    gnatstudio -P $ROOT/uwrap --debug="$ROOT/obj/uwrap $1"
  elif [ "$MODE" == "gdb" ]; then
    gdb --args $ROOT/obj/uwrap $1
  fi
}

export MODE="$2"
export ROOT=`pwd`/../
export -f UWRAP
export -f test_dir
export -f test

if [ "$1" = "" ]; then
  test_dir "tests" 
else
  test_dir $1
fi


