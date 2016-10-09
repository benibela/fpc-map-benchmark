#!/bin/bash

(
cat <<EOF

  <html>
    <title>FreePascal hash maps: a benchmark</title>
    <script src="Chart.min.js"></script>
    
    <body>
     <h1>FreePascal hash maps</h1>
     <h2>One Benchmark to Rule Them All</h2>
          
     <p><b>Maps to compare:</b>
     
     $(lastunit=""; ./hashbenchmark --mode=list | while read map; do
       unit=$(grep -oE '^[^. _]+' <<<"$map")
       if [[ $unit != $lastunit ]]; then echo '<br>'; fi
       lastunit=$unit;
       if [[ $map =~ contnrs|ghashmap|gmap|rtl-generic ]] && [[ ! $map =~ shortstring ]]; then checked="checked"; else checked=""; fi
       echo '<input type="checkbox" name="map_'$map'" '$checked'>'$(tr _ ' ' <<<"$map")'</input>  ';
     done )
     
     <p><b>Datasets:</b> 
     <input type="checkbox"  name="ds_dics" checked> Dictionary words 
     <input type="checkbox"  name="ds_8"> Short (8) random keys
     <input type="checkbox"  name="ds_200"> Long (200) random keys
     
     <p><b>Model:</b> 
     <input type="checkbox"  name="model_1_0_0"> Only writes
     <input type="checkbox"  name="model_1_3_3" checked> Writes/Balanced Reads (1,3,3)
     <input type="checkbox"  name="model_1_20_2">Writes/Many Reads/Failed Lookups (1,20,2)
     <input type="checkbox"  name="model_1_2_20">Writes/Reads/Many Failed Lookups (1,2,20)

     <p><b>Metric:</b> 
     <input type="checkbox"  name="metric_0"> absolute time (ms)
     <input type="checkbox"  name="metric_1"> absolute memory (bytes)
     <input type="checkbox"  name="metric_2" checked> keys / time 
     <input type="checkbox"  name="metric_3" checked> keys / memory
     <input type="checkbox"  name="metric_4"> time , memory
     <input type="checkbox"  name="metric_5"> memory , time
     <input type="checkbox"  name="metric_6"> time / memory
     <input type="checkbox"  name="metric_7"> memory / time
     <input type="checkbox"  name="metric_8"> time / keys 
     <input type="checkbox"  name="metric_9"> memory / keys

     <p><b>x-axis:</b> 
     <input type="checkbox"  name="xaxis_linear"> linear
     <input type="checkbox"  name="xaxis_logarithmic" checked> logarithmic

     &nbsp;&nbsp;&nbsp;&nbsp;<b>y-axis:</b> 
     <input type="checkbox"  name="yaxis_linear"> linear
     <input type="checkbox"  name="yaxis_logarithmic" checked> logarithmic

     &nbsp;&nbsp;&nbsp;&nbsp;<b>keys limit:</b> 
     <input id="key_min" value="0" oninput="regen()"> to
     <input id="key_max" value="1000000000" oninput="regen()">  

     <script>
EOF


resultpath=/tmp/results
echo 'var data = ['
for res in $resultpath/*; do #*/
  [[ ${res#$resultpath/} =~ (.*)[.](.*)[.](.*)[.](.*) ]]
  name=${BASH_REMATCH[1]}
  source=${BASH_REMATCH[2]}
  queriesperkey=${BASH_REMATCH[3]}
  failqueriesperkey=${BASH_REMATCH[4]}
  echo '{"map": "'$name'", "source": "'$source'", "queriesperkey": '$queriesperkey', "failqueriesperkey": '$failqueriesperkey', '
  echo '"data": ['
  cut  $res -f 2,3,6 -d' ' | sed -Ee 's/^([0-9]+) +([0-9]+) +([0-9]+)/{x: \1, t: \2, m: \3},/'
  echo ']},'
done
echo '];'


cat <<EOF
  var metricKeysLabel = "keys";
  var metricMemoryLabel = "memory (bytes)";
  var metricTimeLabel = "time (ms)";
  var metrics = [
    "absolute time (lower is better)",   metricKeysLabel,   metricTimeLabel, function(d) { return {x: d.x, y:  d.t}; },
    "absolute memory (lower is better)", metricKeysLabel,   metricMemoryLabel, function(d) { return {x: d.x, y:  d.m}; },
    "keys / time (higher is better)",     metricKeysLabel,   "", function(d){ return {x: d.x, y:  d.x / Math.max(1,d.t)} },
    "keys / memory (higher is better)",   metricKeysLabel,   "", function(d){ return {x: d.x, y:  d.x / Math.max(1,d.m)} },
    "time , memory (bottom left is best)",   metricTimeLabel,   metricMemoryLabel, function(d){ return {x: d.t, y:  d.m, k: d.x} },
    "memory , time (bottom left is best)",   metricMemoryLabel, metricTimeLabel, function(d){ return {x: d.m, y:  d.t, k: d.x} },
    "time / memory",   metricMemoryLabel, "", function(d){ return {x: d.m, y:  d.t / Math.max(1,d.m), k: d.x} },
    "memory / time",   metricTimeLabel,   "", function(d){ return {x: d.t, y:  d.m / Math.max(1,d.t), k: d.x} },
    "time / keysc (lower is better)",     metricKeysLabel,   "", function(d){ return {x: d.x, y:  d.t / Math.max(1,d.x), k: d.x} },
    "memory / keys (lower is better)",   metricKeysLabel,   "", function(d){ return {x: d.x, y:  d.m / Math.max(1,d.x), k: d.x} },
  ];
  
  function prettyTime(t) {
    if (t < 2*1000) return t + "ms";
    if (t < 1000*60) return Math.round(t*10 / 1000)/10 + "s";
    return Math.round(t*10 / 1000/60)/10 + "min";
  }
  function prettyMem(t) {
    if (t < 1024) return t + "bytes";
    if (t < 1024*1024) return Math.round(t*10 / 1024)/10 + "KB";
    if (t < 1024*1024*1024) return Math.round(t*10 / 1024/1024)/10 + "MB";
    //if (t < 1024*1024*1024) 
    return Math.round(t*10 / 1024/1024/1024)/10 + "GB";
  }
  function prettyCount(c){
    if (c.toExponential) return c.toExponential();
    return c+"";
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
  
  var colorOfCat = ["#4daf4a", "#e41a1c", "#377eb8", "#984ea3"]
  var mapInCat = [0,0,0,0]
  var colors = {};
  var oldcharts = [];
  data.map(function(d){
      if (d.map in colors) return;
      var cat;
      if (/contnrs.TFP|ghashmap|lazfglhash/.test(d.map)) cat = 0;
      else if (/fgl.TFP|gmap|lazfglhash|classes/.test(d.map)) cat = 1;
      else if (/rtl-generics/.test(d.map)) cat = 2;
      else cat = 3;   
      //colorCategories[d.map] = cat;
      var c = Color(colorOfCat[cat]);
      if (mapInCat[cat] < 3) c.darken(0.2 * (3 - mapInCat[cat]));
      else if (mapInCat[cat] > 3) c.lighten(0.2 * (mapInCat[cat] - 3));
      colors[d.map] = c.hexString();
      mapInCat[cat]++;
  })
  
  function regen(){
    oldcharts.map(function(c){c.destroy()});
    oldcharts = [];
    var output = document.getElementById("plotoutput");
    output.innerHTML = "";
    
    function chosenoption(prefix, callback) {
      boxes(prefix).map(function(i){ if (i.checked) callback(i.name.slice(prefix.length), i.nextSibling.textContent) })
    }
    
    
    var activemaps = {};
    chosenoption("map_", function(map) { activemaps[map] = true; });
    
    chosenoption("ds_", function(source, sourcetext){
    chosenoption("model_", function(model, modeltext){
      var tmpregex = /([0-9]+)_([0-9]+)$/.exec(model);
      var queriesperkey = tmpregex[1];
      var failqueriesperkey = tmpregex[2];
    chosenoption("metric_", function(metric, metrictext){
      var metricf = metrics[metric * 4 + 3];
    chosenoption("xaxis_", function(xaxis, xaxistext){
    chosenoption("yaxis_", function(yaxis, yaxistext){
   
    var key_min = document.getElementById("key_min").value*1;
    var key_max = document.getElementById("key_max").value*1;

    var datasets = [];
    var basedatasets = [];
    var basedatasetsoffset = [];
    for (var i=0;i<data.length;i++) {
      if (activemaps[data[i].map] && data[i].source == source && data[i].queriesperkey == queriesperkey && data[i].failqueriesperkey == failqueriesperkey) {
         basedatasets.push(data[i]);
         basedatasetsoffset.push(0);
         var newdata = [];
         for (var j=0;j<data[i].data.length;j++) {
           if (data[i].data[j].x < key_min ) basedatasetsoffset[basedatasetsoffset.length-1] = j + 1;
           else if (data[i].data[j].x > key_max ) break;
           else newdata.push(metricf(data[i].data[j]));
         }
         datasets.push( {
             label: data[i].map,
             data: newdata,
             fill: false,
             fill: false,
             borderWidth: 3,
             borderColor: colors[data[i].map]
         } );
      }
    }
    
    var ctx = document.createElement("canvas")
    output.appendChild(ctx);
    
      var myChart = new Chart(ctx, {
        "type": "line",
        "data": {
          datasets: datasets 
        }, options: {
            scales: {
                xAxes: [{
                    type: xaxis,
                    position: 'bottom',
                    scaleLabel: {labelString: metrics[metric*4 + 1], display: metrics[metric*4 + 1] != ""}
                }],
                yAxes: [{
                    type: yaxis,
                    position: 'left',
                    scaleLabel: {labelString: metrics[metric*4 + 2], display: metrics[metric*4 + 1] != ""}
                }]  

            },
         title: {
                    display: true,
                    text: sourcetext + " " + modeltext + " " + metrics[metric*4],
                    titleFontColor: "black"
                },
          tooltips: {callbacks: {
            afterLabel: function(ti){
             // console.log		(data.toSource())
              var p = basedatasets[ti.datasetIndex].data[ti.index + basedatasetsoffset[ti.datasetIndex]];
              return prettyCount(p.x) + " keys, " + prettyTime(p.t)+ ", "+prettyMem(p.m);
            }}}
        }
        
      }); 
      oldcharts.push(myChart)
    
    })
    })
    })
    })
    })
    
 
  }

  </script>
  <div id="plotoutput"></div>
  <script>
    boxes().map(function(i){i.onclick = regen});
    regen()
  </script>
  
  <h3>General observations:</h3>
  
  <h4>Good built-in maps:</h4>
  <ul>
  <li>For small keys, TFPHashList is the fastest map of all maps provided by FreePascal. However, for longer keys or many lookups the performances degrades drastically. It uses shortstrings, so there is a hard-limit of 256 byte keys, and it seems to copy all strings, as its memory usage is very low for small keys, but also increases drastically with the key length. </li>
  
  <li>The non-cuckoo maps of the rtl-generic.collections package, i.e. linear and double hashing maps, are more generic, use slightly less memory and are slightly slower for shorter keys, but faster than TFPHashList for longer keys. The linear map appears to be slightly faster than the double hashing one, when the keys are truly random.  </li>
  
  <li>The cuckoo maps use less memory, at the cost of lower performance. Especially cuckoo-6 is the most memory efficient hash map of them all, but also the slowest. Cuckoo-2 uses nearly the same memory as the non-cuckoo maps, but is still slower. Cuckoo-4 is in-between. </li>
  </ul>
  <h4>Good 3rd-party maps:</h4>
  
  Generally, all 3rd-party maps perform well, but two are especially notable as they are much faster than the fpc provided maps:
  
  <ul>
  <li>  <a href="http://yann.merignac.free.fr/unit-gcontnrs.html">Yamer's TGenHashMap</a> in the gcontnrs package is faster and uses less memory than the non-cuckoo maps, but requires more memory than the memory-efficient cuckoo-maps. For small keys its characterists even matches TFPHashList.
  </li>
  <li>  <a href="https://github.com/BeRo1985/flre/">Bero's FLRE cache map</a> is an internal used map in the FLRE-package, so it is not the easiest map to use. It is the fastest map for long keys, and similar to TFPHashList and Yamer's map for short keys. However, it has slightly higher memory usage.
  </li>
  </ul>
  
  <h4>Other built-in maps:</h4>
  
  <ul>
  <li>The array based TStringList and TFPGMap are the most memory efficient, using less memory than even the cuckoo maps. However, as they are not hash maps, their usage is extremely slow, even if the maps are sorted, so they are nearly useless as maps. Their default setting of unsorted and (in case of TStringList) case-insensitive matching are quite dangerous. </li>
  
  <li>ghashmap is as fast as the rtl-generics map for short keys, but similar to TFPHashList its performance degrades with longer keys. Unlike TFPHashList it is not the fastest before the slow-down afterwards, so it becomes one of the slowest. It also has a high memory usage.</li>

  <li>gmap.TMap is a slow map, because it is implemented as tree.</li>
  
  <li>TFPDataHashTable (as well as TLazFPGHashTable)  is an interesting map. At first it appears very fast, but with more elements it becomes very slow. The explanation is that it has a high-default capacity of around 200 000 items and does not grow. Colliding items are stored in a tree-structure, which becomes inefficient as soon as the initial capacity is exceeded.  </li>
  
    
  </ul>
  
  </html>
EOF
) > /tmp/plot.html