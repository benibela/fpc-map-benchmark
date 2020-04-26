#!/bin/bash

(
cat <<EOF
  <!DOCTYPE html>
  <html lang="en">
    <title>Free Pascal hash maps: a benchmark</title>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link rel="canonical" href="http://www.benibela.de/fpc-map-benchmark_en.html">    
    <script src="js/Chart-2.9.3.min.js"></script>
    <link rel="stylesheet" href="css/Chart-2.9.3.css">
    <style>.prefix { min-width: 8.5em; display: inline-block }
           li {padding-bottom: 0.5em}
           body { line-height: 1.3 } 
    </style>
    
    <body>
     <h1>Free Pascal hash maps</h1>
          
     This is an exhausive benchmark of string-key based (hash)maps available for Free Pascal.
          
     <p><b>Maps to compare:</b> <button onclick="autocheck('map_', true)">all</button><button onclick="autocheckshortstring(true)">all except shortstring</button><button onclick="autocheckshortstring(false)">no shortstring</button><button onclick="autocheck('map_', false)">none</button>
     
     <br><em>built-in:</em>
     
     $(lastunit=""; LD_LIBRARY_PATH=$PWD:$LD_LIBRARY_PATH ./hashbenchmark --mode=list | while read map; do
       prettyname=$(tr _ ' ' <<<"$map" | sed -e "s/s /'s /")
       unit=$(grep -oE '^[^. _]+' <<<"$prettyname")
       if [[ $unit != $lastunit ]]; then 
         prefix=$(grep -oE "^([^']+'s +|[^.]+[.])" <<<"$prettyname")
         echo '<br><span class="prefix">'$prefix'</span>' 
       fi
       lastunit=$unit;
       if [[ $map =~ contnrs|ghashmap|gmap|rtl-generic ]] && [[ ! $map =~ shortstring ]]; then checked="checked"; else checked=""; fi
       if [[ $prettyname = "rtl-generic's linear.shortstring" ]]; then echo '<br><span class="prefix"></span>'; fi
       echo '<input type="checkbox" name="map_'$map'" '$checked'>'
       if [[ $prettyname = "BeniBela's Hashmap" ]]; then echo 'TXQHashmapStr'; else echo ${prettyname#"$prefix"}; fi
       if [[ $prettyname = "rtl-generic's linear" ]]; then echo ' (TDictionary)'; 
       elif [[ $prettyname = "rtl-generic's cuckoo6.shortstring" ]]; then echo '<br><em>3rd party:</em>'; 
       elif [[ $prettyname = "avk959's GLiteChainHashMap" ]]; then echo '<br><br><em>non-Pascal:</em>'; 
       fi
     done )<br>
     
     <p><b>Datasets:</b> 
     <input type="checkbox"  name="ds_dics" checked> Dictionary words (~16 characters)
     <input type="checkbox"  name="ds_8"> Short random keys (8 bytes) 
     <input type="checkbox"  name="ds_200"> Long random keys (200 bytes)
     
     <p><b>Model:</b> <sup>(writes,succeeding lookups,failed lookups)</sup>
     <input type="checkbox"  name="model_1_0_0">Only writes <sup>(1,0,0)</sup>
     <input type="checkbox"  name="model_1_3_3"> Balanced <sup>(1,3,3)</sup>
     <input type="checkbox"  name="model_1_20_2" checked>Many Reads <sup>(1,20,2)</sup>
     <input type="checkbox"  name="model_1_2_20">Many Failed Lookups <sup>(1,2,20)</sup>
     <input type="checkbox"  name="model_pred">Custom prediction: <input id="pred_write" value="1" size=5 oninput="regen()">, <input id="pred_read" value="100" size=5 oninput="regen()">, <input id="pred_fail" value="100" size="5" oninput="regen()">

     <p><b>Metric:</b> 
     <input type="checkbox"  name="metric_0"> absolute time (ms)
     <input type="checkbox"  name="metric_1"> absolute memory (bytes)
     <input type="checkbox"  name="metric_2" checked> keys / time 
     <input type="checkbox"  name="metric_3" checked> keys / memory
     <input type="checkbox"  name="metric_4"> time, memory
     <input type="checkbox"  name="metric_5"> memory, time
     <input type="checkbox"  name="metric_6"> time / memory
     <input type="checkbox"  name="metric_7"> memory / time
     <input type="checkbox"  name="metric_8"> time / keys 
     <input type="checkbox"  name="metric_9"> memory / keys<br>
     bubbles: 
     <input type="checkbox"  name="metric_10"> absolute time (abs. memory)
     <input type="checkbox"  name="metric_11"> abs. memory (abs. time)
     <input type="checkbox"  name="metric_12"> keys/time (keys/memory)
     <input type="checkbox"  name="metric_13"> memory/ keys (keys/time)
     <input type="checkbox"  name="metric_14" checked> keys/time (memory/keys)
     <input type="checkbox"  name="metric_15"> memory/keys (time/keys) <br>
     ranking: <input type="checkbox"  name="ranking_0"> time <input type="checkbox"  name="ranking_1"> memory

     <p><b>x-axis:</b> 
     <input type="checkbox"  name="xaxis_linear"> linear
     <input type="checkbox"  name="xaxis_logarithmic" checked> logarithmic

     &nbsp;&nbsp;&nbsp;&nbsp;<b>y-axis:</b> 
     <input type="checkbox"  name="yaxis_linear"> linear
     <input type="checkbox"  name="yaxis_logarithmic" checked> logarithmic

     &nbsp;&nbsp;&nbsp;&nbsp;<b>keys limit:</b> 
     <input id="key_min" value="1000" oninput="regen()"> to
     <input id="key_max" value="100000000" oninput="regen()">  
     <button onclick="changecolorprompts()">change colors</button>

  <div id="plotoutput"></div>
  <h3>General observations:</h3>
  
  <h4>Good built-in maps:</h4>
  <ul>
  <li>For small keys, TFPHashList is the fastest map of all maps provided by FreePascal. However, for longer keys or dozens of lookups the performances degrades drastically. It uses shortstrings, so there is a hard-limit of 256 byte keys (which can also explain the lookup slowdown, as the benchmark uses ansistrings), and it seems to copy all strings, as its memory usage is very low for small keys, but also increases drastically with the key length.<br/> 
  The extreme performance drop at higher key counts might occur, because the computer ran out of memory and used the swap file. </li>
  
  <li>The maps of the rtl-generic.collections package can be classified in two groups: non-cuckoo maps (linear, quadratic, double hashing) and cuckoo maps. <ul>
  <li>With the parameters actually used in this benchmark, the non-cuckoo maps are always faster than the cuckoo map or other maps. Slightly slower than TFPHashList for small keys, but faster for long keys. The linear map appears to be slightly faster than the double hashing one (at least when the keys are truly random). The quadratic map is often faster than the linear map, but it was broken in older fpc versions, so it should only be used with fpc 3.2 or newer. The linear map is also called <code>TDictionary</code>. </li>  
  <li>When the number of reads increases to a few dozen queries/key, the cuckoo maps will be faster than the non-cuckoo maps (shown by the difference between writing and reading speed. Cuckoo-2 is predicted to become faster than any other built-in map). They also use less memory and especially cuckoo-6 is the most memory efficient hash map of them all, but also the slowest. Cuckoo-2 is the fastest cuckoo-map, but uses nearly the same memory as the non-cuckoo maps. Cuckoo-4 is in-between. </li></ul></li>
  </li>
  
  </ul>
  <h4>Good 3rd-party maps:</h4>
  
  Generally, all 3rd-party maps perform well, but some are especially notable as they are faster than the fpc provided maps in most situations:
  
  <ul>
  <li> <a href="https://github.com/avk959/LGenerics">avk959's LGenerics</a> are overall the fastest maps in the benchmark. For insertions only, GLiteChainHashMap is the fastest, followed by GOrderedHashMap and GChainHashMap. However, for lookups there is a turning point at around 100 000 keys, where the GHashMapLP, GHashMapLPT, GHashMapQP and GLiteHashMapLP maps become the fastest maps. They are faster than rtl generics; and faster than TFPHashList, except in the case of only writing short keys. Unfortunately, these maps require fpc 3.2. </li>
  <li>  <a href="http://yann.merignac.free.fr/unit-gcontnrs.html">Yamer's TGenHashMap</a> in the gcontnrs package is fast and memory efficient. It is the fastest map for lookups of short and dictionary keys. It faster than the rtl generics maps for short keys, and in-between for long keys. Similarly, its memory usage is between the best and worst maps of rtl generics. For short keys, it behaves similar to the maps of avk959 with better memory usage; for long keys, it is slower.  <!-- and uses less memory than the non-cuckoo maps, but requires more memory than the memory-efficient cuckoo-maps. <!--Its insertion speed is slower than TFPHashList, but for less than 200 000 small keys the insertion performance is better than avk959's GLiteHashMapLP and worse than GLiteChainHashMap. The lookup performance is better than the one of TFPHashList, but worse than GLiteHashMapLP, but for short keys better than .-->
  </li>
  <li>  <a href="https://github.com/BeRo1985/flre/">Bero's FLRE cache map</a> is an internally used map in the FLRE-package, so it is not the easiest map to use. It is similar to TFPHashList, avk959's LGenerics and Yamer's map for short keys, and used to be the fastest map for long keys in older versions of the benchmark. However, it has slightly higher memory usage. A closer examination of its code shows that it uses a very fast hash function, so the performance might come from the hash function not the map construction. It does not handle deletions well and keeps memory unfreed till the next resize. Currently it keeps items in insertion-order, but there is no guarantee of that.
  </li>
  <li> <a href="https://github.com/benibela/internettools">BeniBela's Hashmap</a> is my own map used in the internet tools. It is just a modification of Bero's FLRE cache map to support generic types, so it behaves the same. However, it does not use Bero's fast hashfunction, so it is worse on long keys. On the other hand, it uses less memory. </li>
  </ul>
  
  <h4>Other built-in maps:</h4>
  
  <ul>
  <li>The array based TStringList and TFPGMap are the most memory efficient, using less memory than even the cuckoo maps. However, as they are not hash maps, their usage is extremely slow, even if the maps are sorted, so they are nearly useless as maps. Their default setting of unsorted and (in case of TStringList) case-insensitive and local-aware matching are quite dangerous. </li>
  
  <li>ghashmap is as fast as the rtl-generics maps for short keys, but similar to TFPHashList its performance degrades with longer keys. Unlike TFPHashList it is not the fastest before the slow-down afterwards, so it becomes one of the slowest. It also has a high memory usage. </li>

  <li>gmap.TMap is a slow map, because it is implemented as tree.</li>
  
  <li>TFPDataHashTable (as well as TLazFPGHashTable)  is an interesting map. At first it appears very fast, but with more elements it becomes very slow. The explanation is that it has a high-default capacity of around 200 000 items and does not grow. Colliding items are stored in a tree-structure, which becomes inefficient as soon as the initial capacity is exceeded.  </li>
  
  <li>laz2_xmlutils.THashTable is also very peculiar. It begins slowly; but around 100 to 10 000 items, it is the fastest built-in map; then it becomes slower than TFPHashList; but, for more than 100 000 items, it is again often the fastest built-in map. However, there is a big difference between read and write speed, so it is predicted to become one of the slowest map for a lot of lookups. Generally, it behaves similarly to TFPHashList, although with slower insertion speed, but faster lookup speed. It also has high memory usage.<br>
  Its most important feature is that it supports lookups of pchars, e.g. to lookup individual words of a sentence, without making copies of each word.</li>
    
  </ul>

  <h4>How to choose a hashmap</h4>
  
  This benchmark can be used to select the ideal map for a specific use case:
  
  <ul>
  <li>Enter the minimal and maximal count of expected items at "keys limit".</li>
  <li>Select the model "custom prediction" and enter how often each item will be inserted, read, and not found.</li>
  <li>Select the dataset that corresponds to your key length.</li>
  <li>Select the maps you consider using.</li>
  </ul>
  
  Then you can choose a metric:
  <ul>
  <li>The <i>"ranking" metrics</i> might be the easiest metric to understand. <br>
  The rankings for time or memory sort all selected maps according to their time and memory performance. Then the best map is shown at the top of the list.<br>
  The ranking is only calculated for the upper limit entered in the "keys limit" field.
  </li>

  <li>The bubble metric <i>"keys/time (memory/keys)"</i> shows speed and memory usage for varying key counts.<br>
      The fastest map will be shown at the top of the plot. The most memory efficient map has the smallest bubbles (only the bubbles in the same column can be compared to each other).<br>
      You can easily see how the performance of the maps changes when the key count increases. Hover the mouse over a bubble to see the exact performance stats in a tooltip.
  </li>
  
  <li>The metric <i>"time, memory"</i> shows how time and memory performance relate to each other. <br>
  The points of fastest map will be on the left side, the maps with the lowest memory usage will be at the bottom.<br> Therefore, if you think of the plot as split into quarters, the bottom left will have optimal maps, the top left will have fast maps with huge memory usage, the bottom right will have memory efficient slow maps, and the top right will have useless maps.  <br>
  This might be the most important metric, but also the hardest to understand. If you set minimal and maximal keys limit to the same value, each map is shown as single point, which makes it easier to see which map is the fastest and which one is the most lightweight.
  </li>
  </ul>
  
  If this page is too slow, you can uncheck all checkboxes in a category, then no plots are shown and selections are instantanously. You can also disable maps by clicking their name on the legend without affecting other plots.
  Some maps have no datapoints for high key count, then only points with low key counts are shown and the map might appear more efficient than it is in the scatter plot. Move the mouse over the data points to check, the tooltip shows the actual number of keys, time and memory usage. 
  
  <br><br>This benchmark was compiled with freepascal 3.3.1 (r44777), -O4, and run on a 64-bit Intel(R) Core(TM) i7-3770 CPU @ 3.40GHz openSUSE 12.2 system. <!-- gcc8.3.0, -O4 -->
  Older versions: <a href="fpc-map-benchmark-r44126_en.html">fpc 3.3.1 (r44126), -O3</a> <a href="fpc-map-benchmark-r35800_en.html">fpc 3.1.1 (r35800)</a>, <a href="fpc-map-benchmark-r34557_en.html">fpc 3.1.1 (r34557)</a>
  
       <script>
EOF


resultpath=/tmp/results
echo 'var rawdata = ['
for res in $resultpath/*; do #*/
  [[ ${res#$resultpath/} =~ (.*)[.](.*)[.](.*)[.](.*) ]]
  name=${BASH_REMATCH[1]}
  source=${BASH_REMATCH[2]}
  queriesperkey=${BASH_REMATCH[3]}
  failqueriesperkey=${BASH_REMATCH[4]}
  echo '{"map": "'$name'", "source": "'$source'", "queriesperkey": '$queriesperkey', "failqueriesperkey": '$failqueriesperkey', '
  echo '"rawdata": ['
  cut  $res -f 2,3,6 -d' ' | sed -Ee 's/^([0-9]+) +([0-9.]+) +([0-9]+)/\1,\2,\3,/' | tr -d '\n'
  echo ']},'
done
echo '];'

cat <<EOF
  String.prototype.contains = function(s){ return this.indexOf(s) >= 0 } 

  var data = {};
  for (var i=0;i<rawdata.length;i++) {
    var row = rawdata[i];
    if (!data[row.map]) data[row.map] = {};
    if (!data[row.map][row.source]) data[row.map][row.source] = {};
    if (!data[row.map][row.source][row.queriesperkey]) data[row.map][row.source][row.queriesperkey] = {};
    //if (!data[row.map][row.source][row.queriesperkey][row.failqueriesperkey]) data[row.map][row.source][row.queriesperkey][row.failqueriesperkey] = [];
    var newdata = [];
    for (var j=0;j<row.rawdata.length;j+=3) {
      if (newdata.length > 0 && newdata[newdata.length - 1].x == row.rawdata[j] ) {
        newdata[newdata.length - 1].t = (newdata[newdata.length - 1].t + row.rawdata[j+1]) / 2;
        newdata[newdata.length - 1].m = (newdata[newdata.length - 1].m + row.rawdata[j+2]) / 2;
      } else 
        newdata.push({x: row.rawdata[j], t: row.rawdata[j+1], m:row.rawdata[j+2]});
    }
    data[row.map][row.source][row.queriesperkey][row.failqueriesperkey] = newdata;
  }


  var metricKeysLabel = "keys";
  var metricMemoryLabel = "memory (bytes)";
  var metricTimeLabel = "time (ms)";
  var axes = {"time": {"label": metricTimeLabel},
              "memory": {"label": metricMemoryLabel},
              "keys": {"label": metricKeysLabel}
             }
  var metrics = [
    {title: "absolute time (lower is better)",   
     xAxes: axes.keys,
     yAxes: axes.time, 
     datamap: function(d) { return {x: d.x, y:  d.t}; }},
    {title: "absolute memory (lower is better)", 
     xAxes: axes.keys,   
     yAxes: axes.memory,
     datamap: function(d) { return {x: d.x, y:  d.m}; }},
    {title: "keys / time (higher is better)",    
     xAxes: axes.keys,   
     yAxesLabel: "",
     datamap: function(d){ return {x: d.x, y:  d.x / Math.max(0.001,d.t)} }},
    {title: "keys / memory (higher is better)",  
     xAxes: axes.keys,
     yAxesLabel: "",
     datamap: function(d){ return {x: d.x, y:  d.x / Math.max(1,d.m)} }},
    {title: "time , memory (bottom left is best)",   
     xAxes: axes.time,
     yAxes: axes.memory,
     datamap: function(d){ return {x: d.t, y:  d.m, k: d.x} }},
    {title: "memory , time (bottom left is best)", 
     xAxes: axes.memory, 
     yAxes: axes.time,
     datamap: function(d){ return {x: d.m, y:  d.t, k: d.x} }},
    {title: "time / memory",
     xAxes: axes.memory, 
     yAxesLabel: "", 
     datamap: function(d){ return {x: d.m, y:  d.t / Math.max(1,d.m), k: d.x} }},
    {title: "memory / time",
     xAxes: axes.time,
     yAxesLabel: "", 
     datamap: function(d){ return {x: d.t, y:  d.m / Math.max(0.001,d.t), k: d.x} }},
    {title: "time / keys (lower is better)",
     xAxes: axes.keys,
     yAxesLabel: "",
     datamap: function(d){ return {x: d.x, y:  d.t / Math.max(1,d.x), k: d.x} }},
    {title: "memory / keys (lower is better)",
     xAxes: axes.keys,
     yAxesLabel: "",
     datamap: function(d){ return {x: d.x, y:  d.m / Math.max(1,d.x), k: d.x} }},
     
    {title: "keys, time, (memory) (lower and smaller is better)",
     type: "bubble",
     xAxes: axes.keys,
     yAxes: axes.time,
     datamap: function(d){ return {x: d.x, y:  d.t, r: Math.sqrt(d.m)/1000} }},
    {title: "keys, memory (time) (lower and smaller is better)",
     type: "bubble",
     xAxes: axes.keys,
     yAxes: axes.memory,
     datamap: function(d){ return {x: d.x, y:  d.m, r: Math.sqrt(d.t)/10} }},
    {title: "keys / time (keys / memory) (higher and larger is better)",
     type: "bubble",
     xAxes: axes.keys,   
     yAxesLabel: "",
     datamap: function(d){ return {x: d.x, y:  d.x / Math.max(0.001,d.t), r: Math.sqrt(d.x / Math.max(1,d.m))*100} }},
    {title: "keys / memory (keys / time) (higher and larger is better)",
     type: "bubble",
     xAxes: axes.keys,
     yAxesLabel: "",
     datamap: function(d){ return {x: d.x, y:  d.x / Math.max(1,d.m), r: Math.sqrt(d.x / Math.max(0.001,d.t))} }},
    {title: "keys / time (memory / keys) (higher and smaller is better)",
     type: "bubble",
     xAxes: axes.keys,   
     yAxesLabel: "",
     datamap: function(d){ return {x: d.x, y:  d.x / Math.max(0.001,d.t), r: Math.sqrt(d.m / Math.max(1,d.x))} }},
    {title: "keys / memory (time / keys) (higher and smaller is better)",
     type: "bubble",
     xAxes: axes.keys,
     yAxesLabel: "",
     datamap: function(d){ return {x: d.x, y:  d.x / Math.max(1,d.m), r: Math.sqrt(d.t / Math.max(1,d.x))*100} }},
  ];
  var rankings = [
    {title: "time",
     xAxes: axes.time,
     score: function(d) { return d.t }},
    {title: "memory",
     xAxes: axes.memory,
     score: function(d) { return d.m }},
  ];
  
  function prettyTime(t) {
    if (t == 0) return "0";
    if (t < 2*1000) return t + "ms";
    if (t < 1000*60) return Math.round(t*10 / 1000)/10 + "s";
    return Math.round(t*10 / 1000/60)/10 + "min";
  }
  function prettyMem(t) {
    if (t == 0) return "0";
    if (t < 1024) return t + "B";//"bytes";
    if (t < 1024*1024) return Math.round(t*10 / 1024)/10 + "KB";
    if (t < 1024*1024*1024) return Math.round(t*10 / 1024/1024)/10 + "MB";
    //if (t < 1024*1024*1024) 
    return Math.round(t*10 / 1024/1024/1024)/10 + "GB";
  }
  function prettyKeyCount(c){
    var e = "";
    var l = "";
    if (c.toExponential) e = c.toExponential() + " keys";
    if (c.toLocaleString) l = c.toLocaleString() + " keys";

    if (!e && !l) e = c + " keys";
    
    if (e && l) return l + " ("+e+")";
    if (l) return l;
    return e;
  }
  
  function makeAxes(type,position,options,ranking){
    var axis = {
              type: type,
              position: position
            }
    if (options) {
      axis.scaleLabel = {labelString: options.label, display: !!options.label}      
      if (options === axes.time || options === axes.memory) 
        axis.ticks = {
          beginAtZero: ranking,
          autoSkip: true,
          autoSkipPadding: 5,
          callback: (options === axes.time ? function(value, index, values) {
              return prettyTime(value);
          } : function(value, index, values) {
              return prettyMem(value);
          })
       }
    }
    return [axis];
  }
  
  function boxes(filter){
    if (window["boxescache"+filter]) return window["boxescache"+filter];
    var res = [];
    var inps = document.getElementsByTagName("input");
    for (var i=0;i<inps.length;i++) 
      if (inps[i].type == "checkbox" && (!filter || inps[i].name.indexOf(filter) === 0 ))
        res.push(inps[i]);
    window["boxescache"+filter] = res;
    return res;
  }
  
  var colorOfCat = ["#4daf4a", "#e41a1c", "#377eb8", "#984ea3", "#FFFF33"]
  var mapInCat = [0,0, 0,0, 0,0, 0,0, 0,0]
  var colors = {};
  var oldcharts = [];
  var chartmeta = [];
  rawdata.map(function(d){
      if (d.map in colors) return;
      var cat;
      if (/contnrs.TFP|ghashmap|lazfglhash/.test(d.map)) cat = 0;
      else if (/fgl.TFP|gmap|lazfglhash|classes/.test(d.map)) cat = 1;
      else if (/rtl-generics/.test(d.map)) cat = 2;
      else if (/std::unordered/.test(d.map)) cat = 4;
      else cat = 3;   
      //colorCategories[d.map] = cat;
      var c = Color(colorOfCat[cat]);
      cat = 2 * cat; //todo: this line is useless?
      if (/shortstring/.test(d.map)) {c.hue(c.hue()+70); cat++;}
      if (mapInCat[cat] < 3) c.darken(0.2 * (3 - mapInCat[cat]));
      else if (mapInCat[cat] > 3 && mapInCat[cat] < 7) c.lighten(0.2 * (mapInCat[cat] - 3));
      else if (mapInCat[cat] != 3){
        c.hue(c.hue() + 30);
        c.lighten(0.2 * (10 - mapInCat[cat]));
      } 
      colors[d.map] = c.hexString();
      mapInCat[cat]++;
  }) 
  
  function changecolorprompts(){
    var mapname = prompt("Which map color should be changed?", "")
    if (!mapname) return;
    mapname = mapname.toLowerCase();
    var found = false
    var asked = {};
    for (var i=0;i<rawdata.length;i++) { 
      var name = rawdata[i].map; 
      if (!(name in asked) && name.toLowerCase().contains(mapname)) {
        asked[name] = true;
        found = true;
        var color = prompt("Choose color for map " + name, colors[name])
        if (color) colors[name] = color; else break;
      }
    }
    if (!found) { alert("Map " + mapname + " not found"); return; }
    regen()
  }
  
  function myhighlight(chartid, datasetid){
    var chart = oldcharts[chartid];
    var meta = chartmeta[chartid];
    if ("_highlighted" in meta) { 
      if (meta._highlighted == datasetid) return;
      chart.data.datasets[meta._highlighted].borderWidth = 3;
    } 
    if (datasetid >= 0) {
      meta._highlighted = datasetid;
      chart.data.datasets[datasetid].borderWidth = 7;
    } else
      delete meta._highlighted
    chart.update();
  }

  function autocheck(prefix, state){
    boxes(prefix).map(function(cb){cb.checked = state;})
    regen()
  }
  function autocheckshortstring(enableothers){
    var bs = boxes("map_")
    if (enableothers) bs.map(function(cb){cb.checked = true;})
    bs.map(function(cb){if (cb.name.contains("shortstring") || cb.name.contains("TFPHashList")) cb.checked = false;})
    regen()
  }
  
  function regen(){
    oldcharts.map(function(c){c.destroy()});
    oldcharts = [];
    chartmeta = [];
    var output = document.getElementById("plotoutput");
    output.innerHTML = "";
    
    function chosenoption(prefix, callback) {
      boxes(prefix).map(function(i){ if (i.checked) callback(i.name.slice(prefix.length), i.nextSibling.textContent, i) })
    }
    
    
    var activemaps = [];
    var activemapslabels = [];
    chosenoption("map_", function(map, text, input) { activemaps.push(map); 
      var span = input;
      while (span && span.nodeName != "SPAN") span = span.previousElementSibling;
      if (span) text = span.textContent +  text
      activemapslabels.push(text);
    });
    
    chosenoption("ds_", function(source, sourcetext){
    chosenoption("model_", function(model, modeltext){
      var writes = 1;
      var queriesperkey;
      var failqueriesperkey;
      if (model != "pred") {
        var tmpregex = /([0-9]+)_([0-9]+)$/.exec(model);
        queriesperkey = tmpregex[1];
        failqueriesperkey = tmpregex[2];
      } else {
        writes = document.getElementById("pred_write").value*1;
        queriesperkey = document.getElementById("pred_read").value*1;
        failqueriesperkey = document.getElementById("pred_fail").value*1;
      }
      var key_min = document.getElementById("key_min").value*1;
      var key_max = document.getElementById("key_max").value*1;
    
      var activemapsdatarows = [];
      for (var i=0;i<activemaps.length;i++) {
        function getRow(){
          var row = data[activemaps[i]];
          if (!row) return;
          row = row[source];
          if (!row) return;
          if (model != "pred") {
            row = row[queriesperkey];
            if (!row) return;
            row = row[failqueriesperkey];
            if (!row) return;        
          } else {
            if (!row._predcache) {
              if (!row[0] || !row[0][0] || !row[3] || !row[3][3] || !row[20] || !row[20][2] || !row[2] || !row[2][20] ) return;
              var r0_0 = row[0][0];
              var r3_3 = row[3][3];
              var r2_20 = row[2][20];
              var r20_2 = row[20][2];
              var minlength = Math.min(Math.min(r0_0.length, r3_3.length), Math.min(r20_2.length, r2_20.length));
              var ok = true;
              for (var j=0;j<minlength;j++) if (r0_0[j].x != r3_3[j].x || r0_0[j].x != r2_20[j].x || r0_0[j].x != r20_2[j].x) {
                console.log(data[activemaps[i]]);
		  if (!activemaps[i].contains("TStringList"))
                  alert("fail: " + activemaps[i] + " " +j+ " other runs do not have "+r0_0[j].x+" keys");
                ok = false;
                break;
              }
              if (!ok) return;
              row._predcache = [];
              for (var j=0;j<minlength;j++) {
                var a = r0_0[j].t; var b = r3_3[j].t; var c = r20_2[j].t; var d = r2_20[j].t;
                //Pseudo inverse of [1,0,0; 1,3,3; 1,20,2; 1,2,20]'
                var subtiming = [
                 0.662269 * a + 0.46438 * b - 0.063325 * c - 0.063325 * d,
                -0.032982 * a - 0.01715 * b + 0.052844 * c - 0.002712 * d,
                -0.032982 * a - 0.01715 * b - 0.002712 * c + 0.052844 * d
                ];
                if (subtiming[0] < 0) subtiming[0] = 0; //some maps work as time machine
                if (subtiming[1] < 0) subtiming[1] = 0;
                if (subtiming[2] < 0) subtiming[2] = 0;
                row._predcache.push(subtiming);
              }
            }
            if (!row._predcache) return;
            var predcache = row._predcache;
            var r0_0 = row[0][0];
            if (!row[queriesperkey]) row[queriesperkey] = {};
            row = row[queriesperkey];
            if (!row[failqueriesperkey]) {
              var newarray = [];
              for (var j=0;j<predcache.length;j++) 
                newarray.push({
                  x: r0_0[j].x, 
                  m: r0_0[j].m,
                  t: Math.max(1, writes * predcache[j][0] + queriesperkey * predcache[j][1] + failqueriesperkey * predcache[j][2])
                })
              row[failqueriesperkey] = newarray;
            }
            row = row[failqueriesperkey];
          }
          return row
        }
        var row = getRow()
        activemapsdatarows.push(row);
      }


    chosenoption("metric_", function(metric, metrictext){
      var metricf = metrics[metric].datamap;
      var chartType = "type" in metrics[metric] ? metrics[metric].type: "line";
    chosenoption("xaxis_", function(xaxis, xaxistext){
    chosenoption("yaxis_", function(yaxis, yaxistext){
   

    var datasets = [];
    var basedatasets = [];
    var basedatasetsoffset = [];
    for (var i=0;i<activemaps.length;i++) {
      var row = activemapsdatarows[i];
      if (!row) continue;
      basedatasets.push(row)
      basedatasetsoffset.push(0);
      var newdata = [];
      for (var j=0;j<row.length;j++) {
        if (row[j].x < key_min ) basedatasetsoffset[basedatasetsoffset.length-1] = j + 1;
        else if (row[j].x > key_max ) break;
        else newdata.push(metricf(row[j]));
      }
      var dataset = {
         label: activemapslabels[i],
         data: newdata,
         fill: false,
         borderWidth: 3,
         borderColor: colors[activemaps[i]],
         backgroundColor: colors[activemaps[i]],
         hitRadius: 5
      }
      if (chartType == "bubble") {
        dataset.backgroundColor = dataset.backgroundColor + "44"; //add alpha transparancy
        dataset.hoverBorderWidth = 6;
      }
      datasets.push(dataset);
    }
    
    var ctx = document.createElement("canvas")
    output.appendChild(ctx);
    var tempchartcount = oldcharts.length;
    
      var myChart;
      myChart = new Chart(ctx, {
        "type": chartType,
        "data": {
          datasets: datasets 
        }, options: {
            scales: {
                xAxes: makeAxes(xaxis, "bottom", metrics[metric].xAxes),
                yAxes: makeAxes(yaxis, "left", metrics[metric].yAxes)
            },
         title: {
                    display: true,
                    text: sourcetext + " " + modeltext + (model == "pred" ? "estimate for " + writes+","+queriesperkey+","+failqueriesperkey:"") + ": " + metrics[metric].title,
                    titleFontColor: "black"
                },
          tooltips: {
            mode: 'point', 
            itemSort: function(a,b){
              var pa = basedatasets[a.datasetIndex][a.index + basedatasetsoffset[a.datasetIndex]];
              var pb = basedatasets[b.datasetIndex][b.index + basedatasetsoffset[b.datasetIndex]];
              
              if (pa.t < pb.t) return -1;
              else if (pa.t > pb.t) return 1;
              else if (pa.m < pb.m) return -1;
              else if (pa.m > pb.m) return 1;
              else if (a.datasetIndex < b.datasetIndex) return -1;
              else if (a.datasetIndex > b.datasetIndex) return 1;
              else return 0;
            },
            callbacks: {
            afterLabel: function(ti){
             // console.log 	(data.toSource())
              var p = basedatasets[ti.datasetIndex][ti.index + basedatasetsoffset[ti.datasetIndex]];
              return prettyKeyCount(p.x) + ", " + prettyTime(p.t)+ ", "+prettyMem(p.m);
            }}},
          legend: {
            //labels: {usePointStyle: true},
            onHover: function(event, item){
              myhighlight(tempchartcount, item.datasetIndex)
            }
          }
        }
        
      }); 
      ctx.onmouseout = function(){myhighlight(tempchartcount, -1);}
      oldcharts.push(myChart)
      chartmeta.push({})
    
    })
    })
    })
    
 
 
      chosenoption("ranking_", function(rankingid, text) { 
        var ranking = rankings[rankingid]
        var targetKeyCount = 0;
        activemapsdatarows.map(function(row){if (row) row.map(function(d){
          if (d.x >= key_min && d.x <= key_max && d.x > targetKeyCount) targetKeyCount = d.x;
        })});
        
        if (targetKeyCount == 0) return;
        var mapPerformance = [];
        for (var i=0;i<activemaps.length;i++) {
          var row = activemapsdatarows[i];
          if (!row) continue;
          var p = null
          row.map(function(d){if (d.x == targetKeyCount) p = d})
          if (p) mapPerformance.push({
            "i": i,
            "testcase": p,
            "score": ranking.score(p)
          })
        }
        if (mapPerformance.length == 0) return;
        mapPerformance.sort(function(a,b) { return a.score - b.score })
        console.log(mapPerformance)
        var ctx = document.createElement("canvas")
        ctx.height = mapPerformance.length * 4+30;
        output.appendChild(ctx);
        var myChart = new Chart(ctx, {
          type: "horizontalBar",
          "data": {
            datasets: [{
              data: mapPerformance.map(function(it){return it.score}),
              fill: true,
              borderWidth: 3,
              borderColor: mapPerformance.map(function(it){return colors[activemaps[it.i]]}),
              backgroundColor: mapPerformance.map(function(it){return colors[activemaps[it.i]]}),
            }] 
          }, options: {
            responsive: true,
            showInlineValues : true,
            legend: {
              position: 'right',
            },
            title: {
              display: true,
              text: sourcetext + " " + modeltext + (model == "pred" ? "estimate for " + writes+","+queriesperkey+","+failqueriesperkey:"") + ": "+ ranking.title + " ranking for " + prettyKeyCount(targetKeyCount)
            },
            tooltips: {
              callbacks: {
              afterLabel: function(ti){
              // console.log 	(data.toSource())
                var p = mapPerformance[ti.index].testcase;
                return prettyKeyCount(p.x) + ", " + prettyTime(p.t)+ ", "+prettyMem(p.m);
              }}},
            legend: { display: false }, 
            scales: {
              xAxes: makeAxes("linear", "bottom", ranking.xAxes, true),
              yAxes: [{
                position: 'left',
                type: "category",
                labels: mapPerformance.map(function(it){return activemapslabels[it.i]}),
                scaleLabel: {labelString: "map", display: true}
              }]  
            },            
            "hover": {
              "animationDuration": 0
            },
            "animation": {
              "duration": 1,
              "onComplete": function() {
                //https://stackoverflow.com/questions/42556835/show-values-on-top-of-bars-in-chart-js/46803027#46803027
                var chartInstance = this.chart,
                  ctx = chartInstance.ctx;

                ctx.font = Chart.helpers.fontString(Chart.defaults.global.defaultFontSize, Chart.defaults.global.defaultFontStyle, Chart.defaults.global.defaultFontFamily);
                ctx.textAlign = 'left';
                ctx.textBaseline = 'bottom';

                this.data.datasets.forEach(function(dataset, i) {
                  var meta = chartInstance.controller.getDatasetMeta(i);
                  meta.data.forEach(function(bar, index) {
                    var p = mapPerformance[index].testcase;
                    var data =  prettyTime(p.t)+ ", "+prettyMem(p.m)
                    ctx.fillText(data, bar._model.x + 5, bar._model.y + 7);
                  });
                });
              }
            },
          }
        });
        oldcharts.push(myChart)
      });
 
    })
    })
  }




    boxes().map(function(i){i.onclick = regen});
    regen()
  </script>
  

  </html>
EOF
) > /tmp/plot.html
