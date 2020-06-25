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
  cd $1

  if [ -f test.sh ]; then
    test
  else
    for j in *; do
      if [ -d $j ]; then
        if [ $j != ".." ]; then
          if  [ $j != "." ]; then
            test_dir $j
          fi
        fi
      fi;
    done
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

if [ "$1" = "" ]; then
  test_dir "tests" 
else
  cd $1
  test
fi


