Free Pascal hash maps
======================

This repository contains the benchmark code to compare various string-key based (hash)maps for Free Pascal. 



Results:
----------------------------
  
See [http://www.benibela.de/fpc-map-benchmark_en.html](http://www.benibela.de/fpc-map-benchmark_en.html) for the results.
  
Summary:
  
*  contnrs.TFPHashList is usable for shortstring keys and old fpc versions

*  rtl-generics.collections works on newer fpc versions

*  3rd party maps perform generally well


Compiling
------------------------

Just compile `hashbenchmark.lpr` with fpc or lazarus.

You need to have my bbutils and rcmdline libraries in the source search path. 

You need to have all maps that should be compiled in the source search path. The urls to download the maps are given in the uses section of hashbenchmark.lpr. You can disable not installed maps by disabling the corresponding define at the top of the file.

Usage
------------------------

* Compare all maps to each other for a constant keycount ("keycount" refers to the number of elements inserted in the maps):

    ./hashbenchmark
    
* Benchmark a single map for various keycounts:
    
    ./hashbenchmark --filter $MAPNAME --mode multi-run
    
* Get the names of all maps:

    ./hashbenchmark --mode list
    
    
* Run all the benchmarks on all maps for various inputs and parameters:

    ./runhashbenchmark.sh
    
`./runhashbenchmark.sh` will first write a list of keys to `/tmp/hashmarkcache/`, which require several dozens GB.  All benchmark output is then written to files in `./results`. The benchmarks take several days to run. 
    
* See more options:

    ./hashbenchmark --help

Understanding the raw output
-------------------------

`./hashbenchmark` prints lines to stdout in the format

      mapname keycount time +- timestddev memory +- memorystddev

`keycount` is the number of elements inserted in the map. `time` is the required time in milliseconds. `memory` is the memory usage in bytes. *stddev is the standard deviation of multiple runs on the same input (currently multiple runs are performed for low keycounts, but the standard deviation calculation is disabled, always printing 0).

Before each line on stdout it writes the current parameters to stderr, for example:

      key count: 20000000  keylen: 15 read/write: 100 fail/write: 10
      
meaning 20000000 keys are inserted in the map. After each insertion 100 successful lookups and 10 failed lookups are performed. 

