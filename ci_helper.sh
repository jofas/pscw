make test
success=$(./test | grep SUCCESSFUL!)
if [ "$success" == "" ]; then
  echo TEST FAILURE
  exit 1
fi
