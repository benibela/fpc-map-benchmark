#!/bin/bash
if [[ -z "$1" ]]; then
  mkdir /tmp/hashmarkcache

  if [[ ! -f /tmp/hashmarkcache/dics ]]; then 
    if [[ -d /usr/share/hunspell/ ]]; then dicpath=/usr/share/hunspell/; 
    elif [[ -d /usr/share/myspell/ ]]; then dicpath=/usr/share/myspell/;
    else echo no dics; exit;
    fi
    dics="$dicpath/de_DE.dic:$dicpath/en_US.dic"
    ./hashbenchmark --sources=$dics  --keycount=100000 --mode=dumpdata --dumpdata=/tmp/hashmarkcache/dics
  fi
  if [[ ! -f /tmp/hashmarkcache/200 ]]; then 
    ./hashbenchmark --keylen=200  --keycount=100000 --mode=dumpdata --dumpdata=/tmp/hashmarkcache/200
  fi
  if [[ ! -f /tmp/hashmarkcache/8 ]]; then 
    ./hashbenchmark --keylen=8  --keycount=100000 --mode=dumpdata --dumpdata=/tmp/hashmarkcache/200
  fi

  mkdir results
  ./hashbenchmark --mode=list | while read map; do
    ./runhashbenchmark.sh "$map"
  done
  exit;
fi;




./hashbenchmark --sources=$dics --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 0 --failqueriesperkey 0 > results/$1.dics.0.0
./hashbenchmark --sources=$dics --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 3 --failqueriesperkey 3 > results/$1.dics.3.3
./hashbenchmark --sources=$dics --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 20 --failqueriesperkey 2 > results/$1.dics.20.2
./hashbenchmark --sources=$dics --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 2 --failqueriesperkey 20 > results/$1.dics.2.20

./hashbenchmark --keylen=8 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 0 --failqueriesperkey 0 > results/$1.8.0.0
./hashbenchmark --keylen=8 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 3 --failqueriesperkey 3 > results/$1.8.3.3
./hashbenchmark --keylen=8 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 20 --failqueriesperkey 2 > results/$1.8.20.2
./hashbenchmark --keylen=8 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 2 --failqueriesperkey 20 > results/$1.8.2.20

./hashbenchmark --keylen=200 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 0 --failqueriesperkey 0 > results/$1.200.0.0
./hashbenchmark --keylen=200 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 3 --failqueriesperkey 3 > results/$1.200.3.3
./hashbenchmark --keylen=200 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 20 --failqueriesperkey 2 > results/$1.200.20.2
./hashbenchmark --keylen=200 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 2 --failqueriesperkey 20 > results/$1.200.2.20



#echo "::" $1 $options
#failslist="0 1 100"
#readslist="0 1 20 100"
#lenlist="4 32 200"
#for fails in $failslist; do
# for reads in $readslist; do
#   ./hashbenchmark --sources=$dics --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 > results/$1.$reads.$fails.dics
#   for len in $lenlist; do
#     ./hashbenchmark --filter=$1 --mode=multi-run --keycount=1000 --keylen=4 --memlimit=4096 --timelimit=180000 > results/$1.$reads.$fails.$len
#   done
# done
#done;
#
