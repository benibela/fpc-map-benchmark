#!/bin/bash
if [[ -z "$1" ]]; then
  maxkeycount=80000000

  echo Initializing data
  mkdir -p /tmp/hashmarkcache
  if [[ ! -f /tmp/hashmarkcache/dics ]]; then 
    if [[ -d /usr/share/hunspell/ ]]; then dicpath=/usr/share/hunspell/; 
    elif [[ -d /usr/share/myspell/ ]]; then dicpath=/usr/share/myspell/;
    else echo no dics; exit;
    fi
    dics="$dicpath/de_DE.dic:$dicpath/en_US.dic"
    ./hashbenchmark --sources=$dics --maxkeycount=$maxkeycount --failqueriesperkey=0 --mode=dumpdata --dumpdata=/tmp/hashmarkcache/dics
  fi
  if [[ ! -f /tmp/hashmarkcache/200 ]]; then 
    ./hashbenchmark --keylen=200  --maxkeycount=$maxkeycount --keycount=100000 --failqueriesperkey=0 --mode=dumpdata --dumpdata=/tmp/hashmarkcache/200
  fi
  if [[ ! -f /tmp/hashmarkcache/8 ]]; then 
    ./hashbenchmark --keylen=8  --maxkeycount=$maxkeycount --keycount=100000 --failqueriesperkey=0 --mode=dumpdata --dumpdata=/tmp/hashmarkcache/8
  fi	

  echo Benchmarking...
  mkdir -p results
  ./hashbenchmark --mode=list | while read map; do
    echo Benchmarking map $map
    ./runhashbenchmark.sh "$map"
  done
  exit;
fi;
name=$1
args="--filter=$name --mode=multi-run --memlimit=8192 --timelimit=300000"

function run(){
  cache="--cacheddata /tmp/hashmarkcache/$1"
  queriesperkey=$2
  failqueriesperkey=$3
  result=results/$name.$1.$2.$3
  if [[ ! -f $result ]]; then 
    keycount=1000;
    basekeycount=1000;
  else
    keycount=$(tail -n 1 $result | tail -1 | cut -f 2 -d' ')
    basekeycount=$(sed -e 's/[^0]/1/' <<<$keycount)  
  fi
  ./hashbenchmark $args $cache --keycount=$keycount --basekeycount=$basekeycount --queriesperkey $queriesperkey --failqueriesperkey $failqueriesperkey >> $result
}

run dics 0 0
run dics 3 3
run dics 20 2
run dics 2 20

run 200 0 0
run 200 3 3
run 200 20 2
run 200 2 20

run 8	 0 0
run 8 3 3
run 8 20 2
run 8 2 20
exit
#./hashbenchmark --sources=$dics --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 0 --failqueriesperkey 0 > results/$1.dics.0.0
#./hashbenchmark --sources=$dics --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 3 --failqueriesperkey 3 > results/$1.dics.3.3
#./hashbenchmark --sources=$dics --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 20 --failqueriesperkey 2 > results/$1.dics.20.2
#./hashbenchmark --sources=$dics --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 2 --failqueriesperkey 20 > results/$1.dics.2.20
#
#./hashbenchmark --keylen=8 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 0 --failqueriesperkey 0 > results/$1.8.0.0
#./hashbenchmark --keylen=8 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 3 --failqueriesperkey 3 > results/$1.8.3.3
#./hashbenchmark --keylen=8 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 20 --failqueriesperkey 2 > results/$1.8.20.2
#./hashbenchmark --keylen=8 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 2 --failqueriesperkey 20 > results/$1.8.2.20
#
#./hashbenchmark --keylen=200 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 0 --failqueriesperkey 0 > results/$1.200.0.0
#./hashbenchmark --keylen=200 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 3 --failqueriesperkey 3 > results/$1.200.3.3
#./hashbenchmark --keylen=200 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 20 --failqueriesperkey 2 > results/$1.200.20.2
#./hashbenchmark --keylen=200 --filter=$1 --mode=multi-run --keycount=1000 --memlimit=4096 --timelimit=180000 --queriesperkey 2 --failqueriesperkey 20 > results/$1.200.2.20



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
