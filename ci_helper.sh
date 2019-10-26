make test
./test # so output gets printed to stdout

success=$(./test | grep SUCCESSFUL!)
if [ "$success" == "" ]; then
  echo TEST FAILURE
  exit 1
fi
