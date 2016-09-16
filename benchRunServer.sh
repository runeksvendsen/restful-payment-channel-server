if [ -z $1 ]; then
   echo "Usage: $0 <num_cores>"
   exit 1
fi

./runTestServer.sh config/debug/ 1000 10 $1 | egrep "^real| payments"
