#!/bin/bash
if [[ -z "$1" ]]; then
  mkdir results
  ./hashbenchmark --mode=list | while read map; do
    ./runhashbenchmark.sh "$map"
  done
  exit;
fi;


if [[ -d /usr/share/hunspell/ ]]; then dicpath=/usr/share/hunspell/; 
elif [[ -d /usr/share/myspell/ ]]; then dicpath=/usr/share/myspell/;
else echo no dics; exit;
fi
dics="$dicpath/de_DE.dic:$dicpath/en_US.dic"

echo "::" $1 $options
failslist="0 1 100"
readslist="0 1 20 100"
lenlist="4 32 200"
for fails in $failslist; do
 for reads in $readslist; do
   echo ./hashbenchmark --sources=$dics --filter=$1 --mode=multi-run --keycount=10 --memlimit=4096 results/$1.$reads.$fails.dics
   for len in $lenlist; do
     echo ./hashbenchmark --filter=$1 --mode=multi-run --keycount=10 --keylen=4 --memlimit=4096 results/$1.$reads.$fails.$len
   done
 done
done;
#
