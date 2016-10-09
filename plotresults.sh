#!/bin/bash

(
cat <<EOF
  <!DOCTYPE html>
  <html lang="en">
    <title>Free Pascal hash maps: a benchmark</title>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link rel="canonical" href="http://www.benibela.de/fpc-map-benchmark_en.html">    
    <script src="js/Chart.min.js"></script>
    
    <body>
     <h1>Free Pascal hash maps</h1>
     <h2>One Benchmark to Rule Them All</h2>
          
     This is an exhausive benchmark of string-key based (hash)maps available for Free Pascal.
          
     <p><b>Maps to compare:</b> <button onclick="autocheck('map_', true)">all</button><button onclick="autocheck('map_', false)">none</button>
     
     $(lastunit=""; ./hashbenchmark --mode=list | while read map; do
       unit=$(grep -oE '^[^. _]+' <<<"$map")
       if [[ $unit != $lastunit ]]; then echo '<br>'; fi
       lastunit=$unit;
       if [[ $map =~ contnrs|ghashmap|gmap|rtl-generic ]] && [[ ! $map =~ shortstring ]]; then checked="checked"; else checked=""; fi
       echo '<input type="checkbox" name="map_'$map'" '$checked'>'$(tr _ ' ' <<<"$map" | sed -e "s/s /'s /");
     done )
     
     <p><b>Datasets:</b> 
     <input type="checkbox"  name="ds_dics" checked> Dictionary words (~16 characters)
     <input type="checkbox"  name="ds_8"> Short random keys (8 bytes) 
     <input type="checkbox"  name="ds_200"> Long random keys (200 bytes)
     
     <p><b>Model:</b> <sup>(writes,succeeding lookups,failed lookups)</sup>
     <input type="checkbox"  name="model_1_0_0">Only writes <sup>(1,0,0)</sup>
     <input type="checkbox"  name="model_1_3_3" checked> Balanced <sup>(1,3,3)</sup>
     <input type="checkbox"  name="model_1_20_2">Many Reads <sup>(1,20,2)</sup>
     <input type="checkbox"  name="model_1_2_20">Many Failed Lookups <sup>(1,2,20)</sup>
     <input type="checkbox"  name="model_pred">Custom prediction: <input id="pred_write" value="1" size=5 oninput="regen()">, <input id="pred_read" value="100" size=5 oninput="regen()">, <input id="pred_fail" value="100" size="5" oninput="regen()">

     <p><b>Metric:</b> 
     <input type="checkbox"  name="metric_0"> absolute time (ms)
     <input type="checkbox"  name="metric_1"> absolute memory (bytes)
     <input type="checkbox"  name="metric_2" checked> keys / time 
     <input type="checkbox"  name="metric_3" checked> keys / memory
     <input type="checkbox"  name="metric_4" checked> time, memory
     <input type="checkbox"  name="metric_5"> memory, time
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

  <div id="plotoutput"></div>
  <h3>General observations:</h3>
  
  <h4>Good built-in maps:</h4>
  <ul>
  <li>For small keys, TFPHashList is the fastest map of all maps provided by FreePascal. However, for longer keys or dozens of lookups the performances degrades drastically. It uses shortstrings, so there is a hard-limit of 256 byte keys (which can also explain the lookup slowdown, as the benchmark uses ansistrings), and it seems to copy all strings, as its memory usage is very low for small keys, but also increases drastically with the key length. </li>
  
  <li>The maps of the rtl-generic.collections package can be classified in two groups: non-cuckoo maps (linear, double hashing) and cuckoo maps. <ul>
  <li>With the parameters actually used in this benchmark, the non-cuckoo maps are always faster than the cuckoo map or other maps. Slightly slower than TFPHashList for small keys, but faster for long keys. The linear map appears to be slightly faster than the double hashing one (at least when the keys are truly random). </li>  
  <li>When the number of reads increases to a few dozen queries/key, the cuckoo maps will be faster than the non-cuckoo maps (shown by the difference between writing and reading speed). They also use less memory and especially cuckoo-6 is the most memory efficient hash map of them all, but also the slowest. Cuckoo-2 is the fastest cuckoo-map, but uses nearly the same memory as the non-cuckoo maps. Cuckoo-4 is in-between. </li></ul></li>
  </li>
  
  </ul>
  <h4>Good 3rd-party maps:</h4>
  
  Generally, all 3rd-party maps perform well, but two are especially notable as they are faster than the fpc provided maps in most situations:
  
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

  <h4>How to choose a hashmap</h4>
  
  This benchmark can be used to select the ideal map for a specific use case:
  
  <ul>
  <li>Enter the minimal and maximal count of expected items at "keys limit".</li>
  <li>Select the metric "time, memory".</li>
  <li>Select the model "custom prediction" and enter how often each item will be inserted, read, not found.</li>
  <li>Select the dataset that corresponds to your key length.</li>
  <li>Select the maps you consider using.</li>
  <li>The scatter plot will then show an estimate of time and memory requirements. 
  The points of fastest map will be on the left side, the maps with the lowest memory usage will be at the bottom.<br> Therefore, if you think of the plot as split into quarters, the bottom left will have optimal maps, the top left will have fast maps with huge memory usage, the bottom right will have memory efficient slow maps, and the top right will have useless maps.  </li>
  </ul>
  
  If this page is too slow, you can uncheck all checkboxes in a category, then no plots are shown and selections are instantanously. You can also disable maps by clicking their name on the legend without affecting other plots.
  Some maps have no datapoints for high key count, then only points with low key counts are shown and the map might appear more efficient than it is in the scatter plot. Move the mouse over the data points to check. 
  
  <br><br>This benchmark was compiled with freepascal 3.1.1 (r34557) and run on a 64-bit 1.6 GHz openSUSE 12.2 computer.
  
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
  cut  $res -f 2,3,6 -d' ' | sed -Ee 's/^([0-9]+) +([0-9]+) +([0-9]+)/\1,\2,\3,/' | tr -d '\n'
  echo ']},'
done
echo '];'

cat <<EOF
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
  var metrics = [
    "absolute time (lower is better)",   metricKeysLabel,   metricTimeLabel, function(d) { return {x: d.x, y:  d.t}; },
    "absolute memory (lower is better)", metricKeysLabel,   metricMemoryLabel, function(d) { return {x: d.x, y:  d.m}; },
    "keys / time (higher is better)",     metricKeysLabel,   "", function(d){ return {x: d.x, y:  d.x / Math.max(1,d.t)} },
    "keys / memory (higher is better)",   metricKeysLabel,   "", function(d){ return {x: d.x, y:  d.x / Math.max(1,d.m)} },
    "time , memory (bottom left is best)",   metricTimeLabel,   metricMemoryLabel, function(d){ return {x: d.t, y:  d.m, k: d.x} },
    "memory , time (bottom left is best)",   metricMemoryLabel, metricTimeLabel, function(d){ return {x: d.m, y:  d.t, k: d.x} },
    "time / memory",   metricMemoryLabel, "", function(d){ return {x: d.m, y:  d.t / Math.max(1,d.m), k: d.x} },
    "memory / time",   metricTimeLabel,   "", function(d){ return {x: d.t, y:  d.m / Math.max(1,d.t), k: d.x} },
    "time / keys (lower is better)",     metricKeysLabel,   "", function(d){ return {x: d.x, y:  d.t / Math.max(1,d.x), k: d.x} },
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
  var chartmeta = [];
  rawdata.map(function(d){
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
  
  function regen(){
    oldcharts.map(function(c){c.destroy()});
    oldcharts = [];
    chartmeta = [];
    var output = document.getElementById("plotoutput");
    output.innerHTML = "";
    
    function chosenoption(prefix, callback) {
      boxes(prefix).map(function(i){ if (i.checked) callback(i.name.slice(prefix.length), i.nextSibling.textContent) })
    }
    
    
    var activemaps = [];
    var activemapslabels = [];
    chosenoption("map_", function(map, text) { activemaps.push(map); activemapslabels.push(text); });
    
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
    chosenoption("metric_", function(metric, metrictext){
      var metricf = metrics[metric * 4 + 3];
    chosenoption("xaxis_", function(xaxis, xaxistext){
    chosenoption("yaxis_", function(yaxis, yaxistext){
   
    var key_min = document.getElementById("key_min").value*1;
    var key_max = document.getElementById("key_max").value*1;

    var datasets = [];
    var basedatasets = [];
    var basedatasetsoffset = [];
    for (var i=0;i<activemaps.length;i++) {
      row = data[activemaps[i]];
      if (!row) continue;
      row = row[source];
      if (!row) continue;
      if (model != "pred") {
        row = row[queriesperkey];
        if (!row) continue;
        row = row[failqueriesperkey];
        if (!row) continue;        
      } else {
        if (!row._predcache) {
          if (!row[0] || !row[0][0] || !row[3] || !row[3][3] || !row[20] || !row[20][2] || !row[2] || !row[2][20] ) continue;
          var r0_0 = row[0][0];
          var r3_3 = row[3][3];
          var r2_20 = row[2][20];
          var r20_2 = row[20][2];
          var minlength = Math.min(Math.min(r0_0.length, r3_3.length), Math.min(r20_2.length, r2_20.length));
          var ok = true;
          for (var j=0;j<minlength;j++) if (r0_0[j].x != r3_3[j].x || r0_0[j].x != r2_20[j].x || r0_0[j].x != r20_2[j].x) {
            console.log(data[activemaps[i]]);
            alert("fail: " + activemaps[i] + " " +j+ " other runs do not have "+r0_0[j].x+" keys");
            ok = false;
            break;
          }
          if (!ok) continue;
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
        if (!row._predcache) continue;
        var predcache = row._predcache;
        var r0_0 = row[0][0];
        if (!row[queriesperkey]) row[queriesperkey] = {};
        row = row[queriesperkey];
        if (!row[failqueriesperkey]) {
          var newarray = [];
          for (var j=0;j<predcache.length;j++) 
            newarray.push({
              x: r0_0[j].x, m: r0_0[j].m,
              t: Math.max(1, writes * predcache[j][0] + queriesperkey * predcache[j][1] + failqueriesperkey * predcache[j][2])
            })
          row[failqueriesperkey] = newarray;
        }
        row = row[failqueriesperkey];
      }
      
      basedatasets.push(row);
      basedatasetsoffset.push(0);
      var newdata = [];
      for (var j=0;j<row.length;j++) {
        if (row[j].x < key_min ) basedatasetsoffset[basedatasetsoffset.length-1] = j + 1;
        else if (row[j].x > key_max ) break;
        else newdata.push(metricf(row[j]));
      }
      datasets.push( {
         label: activemapslabels[i],
         data: newdata,
         fill: false,
         borderWidth: 3,
         borderColor: colors[activemaps[i]],
         backgroundColor: colors[activemaps[i]]
      } );
    }
    
    var ctx = document.createElement("canvas")
    output.appendChild(ctx);
    var tempchartcount = oldcharts.length;
    
      var myChart;
      myChart = new Chart(ctx, {
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
                    text: sourcetext + " " + modeltext + (model == "pred" ? "estimate for " + writes+","+queriesperkey+","+failqueriesperkey:"") + " " + metrics[metric*4],
                    titleFontColor: "black"
                },
          tooltips: {callbacks: {
            afterLabel: function(ti){
             // console.log 	(data.toSource())
              var p = basedatasets[ti.datasetIndex][ti.index + basedatasetsoffset[ti.datasetIndex]];
              return prettyCount(p.x) + " keys, " + prettyTime(p.t)+ ", "+prettyMem(p.m);
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
    })
    })
    
 
  }

    boxes().map(function(i){i.onclick = regen});
    regen()
  </script>
  

  </html>
EOF
) > /tmp/plot.html