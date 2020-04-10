program hashbenchmark;

{$mode objfpc}{$H+}
{$define UseCThreads}

{$define benchmarkGenerics}
{$define benchmarkGenericsQuadraticProbing}
//{$define benchmarkIniFiles} just a wrapper around TFPDataHashTable
{$define benchmarkLAZFGLHash} //test failed.
{$define benchmarkLAZXMLUtils}
{$define benchmarkBEROsFLRE}
{$define benchmarkBEROsPASMP}
{$define benchmarkYAMERsHashmap}
{$define benchmarkBARRYKELLYsHashlist}
{$define benchmarkCL4L}
{$define benchmarkFundamentals}
{$define benchmarkLightContainers}
{$define benchmarkDeCAL}
{$define benchmarkJUHAsStringHashMap}
//{$define benchmarkBBHAMT}
//{$define benchmarkBBHashmap}
//{$define benchmarkCustomMap}
//{$define benchmarkKEALONsCL4FPC} conflicts with benchmarkCL4L as you cannot access generic hashmap when unit hashmap is used (#30646)

{$ifdef benchmarkGenerics}
{$define referenceIsTheSixthCuckooOnTheSky}
{$endif}


uses
  //heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  bbutils, rcmdline,(*{$ifdef unix}unix, unixutil,baseunix,
  {$IFDEF LINUX}
  Linux,  // for clock_gettime() access
  {$ENDIF}
  {$IFDEF FreeBSD}
  FreeBSD,  // for clock_gettime() access
  {$ENDIF}{$endif}*)
  Classes,sysutils,math,contnrs,ghashmap
  {$ifdef benchmarkIniFiles},IniFiles{$endif},fgl,gmap
  {$ifdef benchmarkLAZFGLHash},lazfglhash{$endif}
  {$ifdef benchmarkLAZXMLUtils},laz2_xmlutils{$endif}
  {$ifdef benchmarkGenerics}, Generics.Collections{$endif} //https://github.com/dathox/generics.collections
  {$ifdef benchmarkBEROsFLRE}, FLRE{$endif} //https://github.com/BeRo1985/flre/
  {$ifdef BENCHMARKBEROSPASMP}, PasMP{$endif} //https://github.com/BeRo1985/pasmp/
  {$ifdef benchmarkYAMERsHashmap}, gcontnrs{$endif} //http://yann.merignac.free.fr/
  {$ifdef benchmarkBARRYKELLYsHashlist},HashList{$endif} //no idea where it came from, but aminer used it there https://sites.google.com/site/aminer68/scalable-parallel-hashlist
  {$ifdef benchmarkCL4L},Hashmap{$endif} //https://github.com/CynicRus/CL4L
  {$ifdef benchmarkFundamentals},flcDataStructs{$endif}//https://github.com/fundamentalslib/fundamentals5
  {$ifdef benchmarkLightContainers},LightContainers,genlight{$endif} //http://www.stack.nl/~marcov/lightcontainers.zip
  {$ifdef benchmarkDeCAL}, DeCAL{$endif}//from https://bitbucket.org/hovadur/decal
  {$ifdef benchmarkJUHAsStringHashMap},StrHashMap{$endif}//from http://wiki.freepascal.org/StringHashMap
  {$ifdef benchmarkKEALONsCL4FPC},hashmaps{$endif} //https://sourceforge.net/projects/cl4fpc/
  {$ifdef benchmarkBBHAMT},hamt.maps{$endif} //https://www.benibela.de/sources_en.html#hamt
  {$ifdef benchmarkBBHashmap},xquery.internals.common{$endif} //https://www.benibela.de/sources_en.html#internettools (although currently only on http://github.com/benibela/internettools )

  {$ifdef benchmarkCustomMap},custommap{$endif} //add your own map !

  { you can add units after this };

//set trees (usable as map with your own pair class): AvgLvlTree.TStringToStringTree, AVL_Tree

type TMapKind = (mkHash, mkParallelHash, mkTree, mkArray);
     TBenchmarkFunc = function: TObject;

procedure flushall;
begin
  flush(system.output);
  flush(stderr);
  flush(stderr);
end;

procedure fail;
begin
  writeln(stderr, 'failed');
  flushall();
  halt;
end;

(*//from epiktimer
type  TickType = int64;
function SystemTicks: TickType;
{$IFDEF WINDOWS}
begin
  QueryPerformanceCounter(Result);
{$ELSE}
  {$IF defined(LINUX)} {or defined(FreeBSD)}  // FreeBSD disabled - waiting for FPC to catch up
  { Experimental }
  function newGetTickCount: Cardinal;
  const
    NanoPerSec = 1000000000;
    NanoPerMilli = 1000000;
    MilliPerSec = 1000;
  var
    ts: TTimeSpec;
    i: TickType;
    t: timeval;
  begin
    // use the Posix clock_gettime() call
    if clock_gettime(CLOCK_MONOTONIC, @ts)=0 then
    begin
      // Use the FPC fallback
      fpgettimeofday(@t,nil);
      // Build a 64 bit microsecond tick from the seconds and microsecond longints
      Result := (TickType(t.tv_sec) * NanoPerMilli) + t.tv_usec;
      Exit;
    end;
    i := ts.tv_sec;
    i := (i*MilliPerSec) + ts.tv_nsec div NanoPerMilli;
    Result := i;
  end;

begin
    Result := newGetTickCount;
  {$ELSE}
  var
    t: timeval;
  begin
    // Use the FPC fallback
    fpgettimeofday(@t,nil);
    // Build a 64 bit microsecond tick from the seconds and microsecond longints
    Result := (TickType(t.tv_sec) * NanoPerMilli) + t.tv_usec;
  {$ENDIF LINUX}
{$ENDIF WINDOWS}
end;           *)

var data, faildata: array of string;
    randomqueries: array of integer;
    keycount, failkeycount, keylen, queryperkey, failqueryperkey: integer;
    timelimit, memlimit: int64;
    queryMode: (qmFPCRandomQueryList, qmXorShift);
    runMode: (rmList, rmDumpData, rmSingleRun, rmAddativeKeyCount);
    mapFilter: string;



function benchmarkf(kind: TMapKind; name: string; p: TBenchmarkFunc): boolean;
type tmeasurednumber = {double}int64;
const repcount = 1;
var
  r, oversum, j: Integer;
  tms: tdatetime;
  mean, meanmemory: tmeasurednumber;
  std, stdmemory: tmeasurednumber;
  timing: array[1..repcount] of tmeasurednumber;
  m: TMemoryManager;
  memory: array[1..repcount] of tmeasurednumber;
  heapstatus: TFPCHeapStatus;
  maps: array[1..1000] of tobject;
begin
  result := false;
  GetMemoryManager(m);
  mean := 0;
  meanmemory := 0;
  result := true;
  oversum := 1;
  if keycount < 500 then oversum := 1000
  else if keycount < 4000 then oversum := 100
  else if (keycount < 50000) and (kind = mkHash) then oversum := 10;
  for r := 1 to repcount do begin
    heapstatus := m.GetFPCHeapStatus();
    tms := now;
    for j := 1 to oversum do
      maps[j] := p();
    timing[r] := round((now - tms)*MSecsPerDay);;
    memory[r] := (m.GetFPCHeapStatus().CurrHeapUsed - heapstatus.CurrHeapUsed) div oversum;
    for j := 1 to oversum do
      maps[j].free;
    if timing[r] > timelimit then begin
      writeln(stderr, name, ' time limit exceeded: ', round(timing[r]), ' > ', timelimit);
      flushall;
      result := false;
      if r <> repcount then exit;
    end;
    if memory[r] > memlimit then begin
      writeln(stderr, name, ' memory limit exceeded: ', round(memory[r]), ' > ', memlimit);
      flushall;
      result := false;
      if r <> repcount then exit;
    end;
    ClearExceptions();
    mean += timing[r];
    meanmemory += memory[r];

  end;
  std := 0;
  stdmemory := 0;
  if repcount > 1 then begin
    mean := round(mean / repcount);
    meanmemory := round(meanmemory / repcount);
    //writeln('t',timing[1],' ',timing[2],' ',timing[3]);
    //writeln('m',memory[1],' ',memory[2],' ',memory[3]);
    std := round(stddev(pdouble(@timing[1]), repcount)); //sometimes this does not work (ofc it was a double array before). sqrt fail? Negative variance? Either the FPU is in an invalid state from one of the maps, or casting double to extended rounds in the wrong direction.
    stdmemory := round(stddev(pdouble(@memory[1]), repcount));
  end;
  if oversum <= 1 then
    writeln(name, ' ', keycount, ' ', mean, ' +- ', std, ' ', meanmemory, ' +- ',  stdmemory)
   else
    writeln(name, ' ', keycount, ' ', (mean/oversum):3:3, ' +- ', std, ' ', meanmemory, ' +- ',  stdmemory);
  flushall;
end;

procedure benchmark(kind: TMapKind; name: string; args: string; p: TBenchmarkFunc);
begin
  name := StringReplace(name,' ','_',[rfReplaceAll]);
  name := StringReplace(name,'''','',[rfReplaceAll]);
  if (mapFilter <> '') and (name <> mapFilter) then exit();
  case runMode of
    rmList: begin
      writeln(name);
      exit;
    end;
    rmDumpData: exit;
    rmSingleRun: if mapfilter = '' then begin
      if (kind = mkArray) and (keycount > 10000) then exit;
      if (kind = mkTree) and (keycount > 100000) then exit;
    end;
  end;
  if not benchmarkf(kind, name, p) then begin
    flush(system.output);
    flush(stdout);
    halt;
  end;
end;


type generic TG_CallAddXCast<__TMap, TCast> = class
  class procedure add(map: __TMap; const key: string; value: TCast); static; inline;
  end;
  generic TG_CallAdd<TMap> = class(specialize TG_CallAddXCast<TMap, pointer>);
  generic TG_CallAddObjectXCast<TMap, TCast> = class
    class procedure add(map: TMap; const key: string; value: TCast); static; inline;
  end;
  generic TG_CallAddObject<TMap> = class(specialize TG_CallAddObjectXCast<TMap, TObject>);
  generic TG_CallInsert<TMap> = class
    class procedure add(map: TMap; const key: string; value: pointer); static; inline;
  end;
  generic TG_CallSetKeyValue<TMap> = class
    class procedure add(map: TMap; const key: string; value: pointer); static; inline;
  end;
  generic TG_CallGetValue<TMap> = class
    class function get(map: TMap; const key: string): pointer; static; inline;
  end;
  generic TG_CallGetDefault<TMap> = class
    class function get(map: TMap; const key: string): pointer; static; inline;
  end;
  generic TG_CallGetFind<TMap> = class
    class function get(map: TMap; const key: string): pointer; static; inline;
  end;
  generic TG_CallGetLocate<TMap> = class
    class function get(map: TMap; const key: string): pointer; static; inline;
  end;
  generic TG_CallContains<TMap> = class
    class function contains(map: TMap; const key: string): boolean; static; inline;
  end;
  generic TG_CallContainsKey<TMap> = class
    class function contains(map: TMap; const key: string): boolean; static; inline;
  end;
  generic TG_CallContainsGetNil<TMap, TGetter> = class
    class function contains(map: TMap; const key: string): boolean; static; inline;
  end;
  generic TG_CallContainsIndexOf<TMap> = class
    class function contains(map: TMap; const key: string): boolean; static; inline;
  end;
  generic TG_CallContainsLocate<TMap> = class
    class function contains(map: TMap; const key: string): boolean; static; inline;
  end;
  generic TG_TestXXXX<TMap, TAdder, TGetter, TContains, TCast> = class
    class function test: TObject; static;
  end;
  generic TG_TestXXX<TMap, TAdder, TGetter, TCast> = class(specialize TG_TestXXXX<TMap, TAdder, TGetter, specialize TG_CallContainsGetNil<TMap, TGetter>, TCast>);
  generic TG_TestXDefaultXCast<__TMap, TAdder, TCast> = class(specialize TG_TestXXX<__TMap, TAdder, specialize TG_CallGetDefault<__TMap>, TCast>);
  generic TG_TestXDefault<__TMap, TAdder> = class(specialize TG_TestXDefaultXCast<__TMap, TAdder, pointer>);
  generic TG_TestAddDefault<__TMap> = class(specialize TG_TestXDefault<__TMap, specialize TG_CallAdd<__TMap>>);
  generic TG_TestAddDefaultContainsKey<__TMap> = class(specialize TG_TestXXXX<__TMap, specialize TG_CallAdd<__TMap>, specialize TG_CallGetDefault<__TMap>, specialize TG_CallContainsKey<__TMap>, pointer>);
  generic TG_TestInsertDefaultContains<__TMap> = class(specialize TG_TestXXXX<__TMap, specialize TG_CallInsert<__TMap>,  specialize TG_CallGetDefault<__TMap>, specialize TG_CallContains<__TMap>,  pointer>);

class procedure TG_CallAddXCast.add(map: __TMap; const key: string; value: TCast); static; inline;
begin
  map.add(key, value);
end;
class procedure TG_CallAddObjectXCast.add(map: TMap; const key: string; value: TCast); static; inline;
begin
  map.addObject(key, TCast(value));
end;
class procedure TG_CallInsert.add(map: TMap; const key: string; value: pointer); static; inline;
begin
  map.insert(key, value);
end;
class procedure TG_CallSetKeyValue.add(map: TMap; const key: string; value: pointer); static; inline;
begin
  map.SetKeyValue(key, value);
end;
class function TG_CallGetValue.get(map: TMap; const key: string): pointer; static; inline;
begin
  result := pointer(map.getvalue(key));
end;
class function TG_CallGetDefault.get(map: TMap; const key: string): pointer; static; inline;
begin
  result := pointer(map[key]);
end;
class function TG_CallGetFind.get(map: TMap; const key: string): pointer; static; inline;
begin
  result := pointer(map.find(key));
end;
class function TG_CallGetLocate.get(map: TMap; const key: string): pointer; static; inline;
begin
  map.locate(key, result);
end;
class function TG_CallContains.contains(map: TMap; const key: string): boolean; static; inline;
begin
  result := map.contains(key);
end;
class function TG_CallContainsKey.contains(map: TMap; const key: string): boolean; static; inline;
begin
  result := map.containsKey(key);
end;
class function TG_CallContainsIndexOf.contains(map: TMap; const key: string): boolean; static; inline;
begin
  result := map.IndexOf(key) >= 0;
end;
class function TG_CallContainsGetNil.contains(map: TMap; const key: string): boolean; static; inline;
begin
  result := TGetter.get(map, key) <> nil;
end;
class function TG_CallContainsLocate.contains(map: TMap; const key: string): boolean; static; inline;
var temp: pointer;
begin
  result := map.locate(key, temp);
end;

procedure updateXorShift(var xorshift: cardinal); inline;
begin
  xorshift := xorshift xor (xorshift shl 13);
  xorshift := xorshift xor (xorshift shr 17);
  xorshift := xorshift xor (xorshift shl 5);
end;

class function TG_TestXXXX.test(): TObject;
var q, i, j: integer;
  map: TMap;
  xorshift: cardinal;
begin
  map := TMap.create;
  q := 0;
  xorshift := 314159265;
  for i := 0 to keycount - 1 do begin
    //writeln(stderr, 'add');
    TAdder.add(map, data[i], TCast(@data[i]));
    //writeln(stderr, 'query');
    case queryMode of
      qmFPCRandomQueryList: begin
        for j := 0 to queryperkey - 1 do begin
          if TGetter.get(map, data[randomqueries[q]]) <> @data[randomqueries[q]] then fail;
          inc(q);
        end;
        for j := 0 to failqueryperkey - 1 do begin
          if TContains.contains(map, faildata[randomqueries[q]]) then fail;
          inc(q);
        end;
      end;
      qmXorShift: begin
        for j := 1 to queryperkey do begin
          q := xorshift mod (i + 1);
          //writeln(stderr, i, '<',q, ' ',data[q]);
          if TGetter.get(map, data[q]) <> @data[q] then fail;
          updateXorShift(xorshift);
        end;
        for j := 1 to failqueryperkey do begin
          q := xorshift mod failkeycount;
          //writeln(stderr, i, '>',faildata[q]); flush(stderr);
          if TContains.contains(map, faildata[q]) then fail;
          updateXorShift(xorshift);
        end;
      end;
    end;
  end;
  result := map;
end;


type
  TTestFPHashList = specialize TG_TestXXXX<
    contnrs.TFPHashList,
    specialize TG_CallAdd<TFPHashList>,
    specialize TG_CallGetFind<TFPHashList>,
    specialize TG_CallContainsGetNil<TFPHashList, specialize TG_CallGetFind<TFPHashList> >,
    pointer>;
  TTestFPHashTable = specialize TG_TestAddDefault<contnrs.TFPDataHashTable>;

type generic TG_StringHash<tstring> = class
  class function c(const a,b: tstring): boolean;
  class function rawhash(const s: tstring): SizeUInt; inline; static;
  class function hash(const s: tstring; n: SizeUInt): SizeUInt; inline; static;
  //equal(const AKey1, AKey2: TKey): Boolean;
end;
  TStringHash = specialize TG_StringHash<string>;
  TShortStringHash = specialize TG_StringHash<shortstring>;

type TMyFPGMap = class(specialize TFPGMap<string, pointer>)
  constructor create;
end;

constructor TMyFPGMap.create;
begin
  inherited;
  sorted := true;
end;

type
  TTestGHashMap = specialize TG_TestInsertDefaultContains<specialize THashmap<string, pointer, TStringHash>>;
  TSpezGmap = specialize TMap<string, pointer, TStringHash>;
  TTestGMap = specialize TG_TestXXXX<TSpezGmap,
      specialize TG_CallInsert<TSpezGmap>,
      specialize TG_CallGetDefault<TSpezGmap>,
      specialize TG_CallContainsGetNil<TSpezGmap,specialize TG_CallGetFind<TSpezGmap>>,
      pointer>;
  TTestGHashMapShortString = specialize TG_TestInsertDefaultContains<specialize THashmap<shortstring, pointer, TShortStringHash>>;
  TSpezGmapShortString = specialize TMap<shortstring, pointer, TShortStringHash>;
  TTestGMapShortString = specialize TG_TestXXXX<TSpezGmapShortString,
      specialize TG_CallInsert<TSpezGmapShortString>,
      specialize TG_CallGetDefault<TSpezGmapShortString>,
      specialize TG_CallContainsGetNil<TSpezGmapShortString,specialize TG_CallGetFind<TSpezGmapShortString>>,
      pointer>;
  TTestFPGMap = specialize TG_TestXXXX<
    TMyFPGMap,
    specialize TG_CallAdd<TMyFPGMap>,
    specialize TG_CallGetDefault<TMyFPGMap>,
    specialize TG_CallContainsIndexOf<TMyFPGMap>,
    pointer>;

  TStringListSorted = class(classes.TStringList)
    constructor create;
    function getValue(const key: string): pointer; inline;
  end;

  constructor TStringListSorted.create;
  begin
    inherited;
    sorted := true;
    CaseSensitive := true;
  end;

  function TStringListSorted.getValue(const key: string): pointer; inline;
  begin
    result := pointer(Objects[IndexOf(key)]);
  end;

type TTestStringList = specialize TG_TestXXXX<TStringListSorted,
    specialize TG_CallAddObject<TStringListSorted>,
    specialize TG_CallGetValue<TStringListSorted>,
    specialize TG_CallContainsIndexOf<TStringListSorted>,
    TObject>;

{$ifdef benchmarkIniFiles}
function testIniFiles: TObject;
var q, i, j: integer;
  map: IniFiles.TStringHash;
begin
  map := inifiles.TStringHash.Create;
  q := 0;
  for i := 0 to keycount - 1 do begin
    map.Add(data[i], i);
    for j := 0 to queryperkey - 1 do begin
      if map.ValueOf(data[randomqueries[q]]) <> randomqueries[q] then fail;
      inc(q);
    end;
  end;
  result := map;
end;
{$endif}


{$ifdef benchmarkLAZFGLHash}
type TTestLazFPGHashTable = specialize TG_TestXXXX<
  specialize TLazFPGHashTable<pointer>,
  specialize TG_CallAdd<specialize TLazFPGHashTable<pointer>>,
  specialize TG_CallGetDefault<specialize TLazFPGHashTable<pointer>>,
  specialize TG_CallContainsGetNil<specialize TLazFPGHashTable<pointer>, specialize TG_CallGetFind<specialize TLazFPGHashTable<pointer>>>,
  pointer
>;

{$endif}

{$ifdef benchmarkLAZXMLUtils}
type
TLaz2XMLHashTable = class(laz2_xmlutils.THashTable)
  constructor Create();
end;
TCallLazXMLHashTable = class
  class procedure add(map: TLaz2XMLHashTable; const key: string; value: pointer); static; inline;
  class function get(map: TLaz2XMLHashTable; const key: string): pointer; static; inline;
  class function contains(map: TLaz2XMLHashTable; const key: string): boolean; static; inline;
end;
TTestLazXMLHashTable = specialize TG_TestXXXX<
  TLaz2XMLHashTable,
  TCallLazXMLHashTable, TCallLazXMLHashTable, TCallLazXMLHashTable, TObject
>;
constructor TLaz2XMLHashTable.create;
begin
  inherited create(10000,false);
end;
class procedure TCallLazXMLHashTable.add(map: TLaz2XMLHashTable; const key: string; value: pointer);
begin
  map.FindOrAdd(pchar(key), length(key))^.data := TObject(value);
end;
class function TCallLazXMLHashTable.get(map: TLaz2XMLHashTable; const key: string): pointer;
var temp: PHashItem;
begin
  temp := map.Find(pchar(key), length(key));
  if temp <> nil then result := temp^.data else result := nil;
end;
class function TCallLazXMLHashTable.contains(map: TLaz2XMLHashTable; const key: string): boolean;
begin
  result := map.Find(pchar(key), length(key)) <> nil;
end;

{$endif}

{$ifdef benchmarkBEROsFLRE}
type TTestFLRE = specialize TG_TestXDefaultXCast<TFLRECacheHashMap, specialize TG_CallAddXCast<TFLRECacheHashMap, TFLRECacheHashMapData>, TFLRECacheHashMapData>;
{$endif}

{$ifdef BENCHMARKBEROSPASMP}
type TMyPasMPStringHashTable = class(pasmp.TPasMPStringHashTable)
  constructor create;
  function getValue(const key: string): pointer; inline;
end;
constructor TMyPasMPStringHashTable.create;
begin
  inherited create(sizeof(pointer));
end;
function TMyPasMPStringHashTable.getValue(const key: string): pointer;
begin
  if not GetKeyValue(key, result) then result := nil;
end;
type TTestPasMPStringHashTable = specialize TG_TestXXX<TMyPasMPStringHashTable, specialize TG_CallSetKeyValue<TMyPasMPStringHashTable>, specialize TG_CallGetValue<TMyPasMPStringHashTable>, pointer>;
{$endif}

{$ifdef BENCHMARKYAMERSHASHMAP}
type generic TMyGContnrsMap<T,Hasher> = class(specialize TGenHashMap<T, pointer>)
  function DefaultHashKey(const Key: T): Integer; override;
  function DefaultKeysEqual(const A, B: T): Boolean; override;
end;

function TMyGContnrsMap.DefaultHashKey(const Key: T): Integer;
begin
  Result:=Hasher.rawhash(key);
end;

function TMyGContnrsMap.DefaultKeysEqual(const A, B: T): Boolean;
begin
  result := a = b;
end;
type TTestGContnrs = specialize TG_TestInsertDefaultContains<specialize TMyGContnrsMap<string, TStringHash>>;
     TTestGContnrsShortString = specialize TG_TestInsertDefaultContains<specialize TMyGContnrsMap<shortstring, TShortStringHash>>;
{$endif}

{$ifdef BENCHMARKGENERICS}
type
  TTestGenericLinear = specialize    TG_TestAddDefaultContainsKey<specialize TOpenAddressingLP<string, pointer>>;
  TTestGenericQuadratic = specialize TG_TestAddDefaultContainsKey<specialize TOpenAddressingQP<string, pointer>>;
  TTestGenericDouble = specialize    TG_TestAddDefaultContainsKey<specialize TOpenAddressingDH<string, pointer>>;
  TTestGenericCuckooD2 = specialize  TG_TestAddDefaultContainsKey<specialize TCuckooD2<string, pointer>>;
  TTestGenericCuckooD4 = specialize  TG_TestAddDefaultContainsKey<specialize TCuckooD4<string, pointer>>;
  TTestGenericCuckooD6 = specialize  TG_TestAddDefaultContainsKey<specialize TCuckooD6<string, pointer>>;

  TTestGenericLinearShortString = specialize    TG_TestAddDefaultContainsKey<specialize TOpenAddressingLP<shortstring, pointer>>;
  TTestGenericQuadraticShortString = specialize TG_TestAddDefaultContainsKey<specialize TOpenAddressingQP<shortstring, pointer>>;
  TTestGenericDoubleShortString = specialize    TG_TestAddDefaultContainsKey<specialize TOpenAddressingDH<shortstring, pointer>>;
  TTestGenericCuckooD2ShortString = specialize  TG_TestAddDefaultContainsKey<specialize TCuckooD2<shortstring, pointer>>;
  TTestGenericCuckooD4ShortString = specialize  TG_TestAddDefaultContainsKey<specialize TCuckooD4<shortstring, pointer>>;
  TTestGenericCuckooD6ShortString = specialize  TG_TestAddDefaultContainsKey<specialize TCuckooD6<shortstring, pointer>>;
{$endif}

{$ifdef benchmarkBARRYKELLYsHashlist}
type TMyBKHashList = class(HashList.THashList)
  trait : hashlist.TCaseSensitiveTraits;
  constructor create;
  destructor destroy; override;
end;
constructor TMyBKHashList.create;
begin
  trait := hashlist.TCaseSensitiveTraits.Create;
  inherited create(trait, keycount div 2);
end;
destructor TMyBKHashList.destroy;
begin
  trait.free;
  inherited;
end;
type TTestBKHashList = specialize TG_TestAddDefault<TMyBKHashList>;
{
p := @data[i];
map.Add(data[i], p);
}
{$endif}

{$ifdef benchmarkCL4L}
type TMyStrHashMap = class(hashmap.TStrHashMap)
  constructor create;
  procedure insert(const key: string; value: pointer); inline;
end;
   TTestCL4LStrHashMap = class(specialize TG_TestXXX<TMyStrHashMap, specialize TG_CallInsert<TMyStrHashMap>, specialize TG_CallGetValue<TMyStrHashMap>, pointer>);

constructor TMyStrHashMap.create;
begin
  inherited create(keycount div 2, false);
end;
procedure TMyStrHashMap.insert(const key: string; value: pointer);
begin
  PutValue(key, tobject(value));
end;
{$endif}

{$ifdef benchmarkFundamentals}
type TTestFundamentalsPointerDictionaryA = specialize TG_TestAddDefault<TPointerDictionaryA>;
{$endif}

{$ifdef benchmarkLightContainers}
type TTestGenLightContainers = specialize TG_TestXXXX<specialize TLightStringMap<pointer>,
    specialize TG_CallAddObjectXCast<specialize TLightStringMap<pointer>, pointer>,
    specialize TG_CallGetLocate<specialize TLightStringMap<pointer>>,
    specialize TG_CallContainsLocate<specialize TLightStringMap<pointer>>,
    pointer>;

type TMyLightContainer = class
  lm: LightContainers.TLightMap;
  constructor create;
  procedure add(const k: string; value: pointer); inline;
  function get(const k: string): pointer; inline;
  destructor destroy; override;
  property items[const k: string]: pointer read get; default;

end;

constructor TMyLightContainer.create;
begin
  lm := LightMapStrCreate;
end;
procedure TMyLightContainer.add(const k: string; value: pointer); inline;
begin
  LightMapStrPutpair(lm, k, value);
end;
function TMyLightContainer.get(const k: string): pointer; inline;
begin
  result := LightMapStrLocate(lm, k);
end;
destructor TMyLightContainer.destroy;
begin
  LightMapStrDestroy(lm);
  inherited;
end;

type  TTestLightContainers = specialize TG_TestAddDefault<TMyLightContainer>;


{$endif}

{$ifdef benchmarkDeCAL}
type TMyDMap = class(DeCAL.DMap)
  procedure add(const s: string; v: pointer); reintroduce; inline;
  function getvalue(const s: string): pointer; inline;
end;
procedure TMyDMap.add(const s: string; v: pointer);
begin
  PutPair([s,v]);
end;
function TMyDMap.getvalue(const s: string): pointer; inline;
begin
  result := getPointer(locate([s]));
end;
type TTestDeCAL = class(specialize TG_TestXXX<TMyDMap, specialize TG_CallAdd<TMyDMap>, specialize TG_CallGetValue<TMyDMap>, pointer>);
{$endif}

{$ifdef benchmarkJUHAsStringHashMap}
type TTestJuhaStrHashMap = specialize TG_TestAddDefault<StrHashMap.TStringHashMap>;
{$endif}

{$ifdef benchmarkKEALONsCL4FPC}
type
  TMyKealonsHashMap = specialize HashMap<string, pointer, TStringHash>;
  TTestKealonsHashMap = specialize TG_TestXDefault<TMyKealonsHashMap>, specialize TG_CallDefault<TMyKealonsHashMap>>;
{$endif}

{$ifdef benchmarkBBHAMT}
type
  TMutableMapStringPointer = specialize TMutableMap<string, pointer, THAMTTypeInfo>;
  TImmutableMapStringPointer = class(specialize TImmutableMap<string, pointer, THAMTTypeInfo>)
    procedure fakeMutableInsert(const key: string; value: pointer);
  end;

  TImmutableMapAdder = class
    class procedure add(map: TImmutableMapStringPointer; const key: string; value: pointer); static; inline;
  end;
  class procedure TImmutableMapAdder.add(map: TImmutableMapStringPointer; const key: string; value: pointer); static; inline;
  begin
    map.fakeMutableInsert(key, value);
  end;
  procedure TImmutableMapStringPointer.fakeMutableInsert(const key: string; value: pointer);
  var c: TImmutableMapStringPointer;
    tempcount: SizeUInt;
    temproot: PHAMTNode;
  begin
    c := TImmutableMapStringPointer(insert(key, value));
    tempcount := fcount;
    temproot := froot;
    froot := c.froot;
    fcount := c.fcount;
    c.froot := temproot;
    c.fcount := tempcount;
    c.free;
  end;

type
  TTestBBHAMTMutable = specialize TG_TestXXXX<TMutableMapStringPointer, specialize TG_CallInsert<TMutableMapStringPointer>,  specialize TG_CallGetDefault<TMutableMapStringPointer>, specialize TG_CallContains<TMutableMapStringPointer>,  pointer>;
  TTestBBHAMTImmutable = specialize TG_TestXXXX<TImmutableMapStringPointer, TImmutableMapAdder,  specialize TG_CallGetDefault<TImmutableMapStringPointer>, specialize TG_CallContains<TImmutableMapStringPointer>,  pointer>;
{$endif}

{$ifdef benchmarkBBHashmap}
type
  TBBHashmap = class
    map: specialize TXQHashmapStr<pointer>;
    Constructor create;
    procedure add(const key: string; value: pointer); inline;
    function get(const key: string): pointer; inline;
    property defaultget[const key: string]: pointer read get write add ; default;
  end;
constructor TBBHashmap.create;
begin
  map.init;
end;

procedure TBBHashmap.add(const key: string; value: pointer);
begin
  map.include(key, value);
end;

function TBBHashmap.get(const key: string): pointer;
begin
  result := map[key];
end;
type
  TTestBBHashmap = specialize TG_TestXDefault<TBBHashmap, specialize TG_CallAdd<TBBHashmap>>;

{$endif}

{$ifdef benchmarkCustomMap}
type TTestCustomMap = specialize TG_TestXDefault<TCustomMap, specialize TG_CallAdd<TCustomMap>>;
{$endif}


class function TG_StringHash.c(const a, b: tstring): boolean;
begin
  result := a < b;
end;

class function TG_StringHash.rawhash(const s: tstring): SizeUInt;
var
  p,pmax : PChar;
begin
{$push}
{$Q-}
  Result:=0;
  p:=@s[1];
  pmax:=@s[length(s)+1];
  while (p<pmax) do
    begin
      Result:=LongWord(LongInt(Result shl 5) - LongInt(Result)) xor LongWord(P^);
      Inc(p);
    end;
{$pop}
end;

class function TG_StringHash.hash(const s: tstring; n: SizeUInt): SizeUInt; static; //as in contrnrs
begin
  result := rawhash(s) and (n - 1);
end;


{$ifdef referenceIsTheSixthCuckooOnTheSky}
type TReferenceHashmap = specialize TCuckooD6<string, pointer>;
  TReferenceHashmapAdd = specialize TG_CallAdd<TReferenceHashmap>;
  TReferenceHashmapContains = specialize TG_CallContainsKey<TReferenceHashmap>;
{$else}
type TReferenceHashmap = TFPHashList;
  TReferenceHashmapAdd = specialize TG_CallAdd<TReferenceHashmap>;
  TReferenceHashmapContains = specialize TG_CallContainsGetNil<TFPHashList, specialize TG_CallGetFind<TFPHashList> >;
{$endif}

type TKeyUniqueness = (kuNumber, kuFilter);
const KeyNumberCharacter = '!';
var
  s: string;
  i, j, basekeycount, maxkeycount: integer;
  totalkeycount: Integer = 0;
  referenceHashmap: TReferenceHashmap;
  referenceConflict: boolean;
  addkeycount, oldkeycount, oldfailkeycount: integer;

  dumpfile, cacheddata : textfile;

  cmdline: TCommandLineReader;
  temps, sourcefile, dumpdatafn, cacheddatafn: string;
  sources,sources2: tstringlist;
  keyUniqueness: TKeyUniqueness;
begin
  cmdline := TCommandLineReader.create;
  cmdline.declareString('sources', 'Source file');
  cmdline.declareString('cacheddata', 'Source file without duplicate lines', '');
  cmdline.declareInt('keycount', 'keycount', 0);
  cmdline.declareInt('basekeycount', 'basekeycount', 0);
  cmdline.declareInt('maxkeycount', 'maxkeycount', high(integer));
  cmdline.declareInt('keylen', 'keylen', 0);
  cmdline.declareInt('queriesperkey', 'queryperkey', 100);
  cmdline.declareInt('failqueriesperkey', 'failqueryperkey', 10);
  cmdline.declareInt('memlimit', 'mem limit (MB)', 1024);
  cmdline.declareInt('timelimit', 'time limit', 10*60*1000);
  cmdline.declareString('mode', ' list, single-run, multi-run', 'single-run');
  cmdline.declareString('querymode', 'random or xorshift', 'xorshift');
  cmdline.declareString('filter', ' Map to use', '');
  cmdline.declareString('dumpdata', 'Data inserted', '');
  cmdline.declareString('keyuniqueness', 'number, filter', 'number');

  timelimit := cmdline.readInt('timelimit');
  memlimit := int64(cmdline.readInt('memlimit')) * 1024 * 1024;

  sourcefile := cmdline.readString('sources');

  keycount := cmdline.readInt('keycount');
  keylen := cmdline.readInt('keylen');
  queryperkey := cmdline.readInt('queriesperkey');
  failqueryperkey := cmdline.readInt('failqueriesperkey');

  if sourcefile = '' then begin
    if keycount = 0 then keycount := 1000;
    if keylen = 0 then keylen := 15;
    sources := nil;
  end else begin
    sources := tstringlist.create;
    sources2 := tstringlist.create;
    for temps in strSplit(sourcefile, {$ifdef windows}';'{$else}':'{$endif}) do begin
      sources2.loadfromfile(temps);
      sources.Capacity := max(sources.Capacity,sources.count + sources2.count);
      for i := 0 to sources2.count - 1 do
        sources.add(sources2[i]);
    end;
    sources2.free;
    if not cmdline.existsProperty('keycount') then
      keycount := sources.count;
    writeln(stderr, 'Done loading sources');
  end;
  mapFilter:= cmdline.readString('filter');
  case cmdline.readString('mode') of
    'list': runMode := rmList;
    'dumpdata': runMode := rmDumpData;
    'multi-run': runmode := rmAddativeKeyCount;
    else {'single-run': }runmode := rmSingleRun;
  end;
  case cmdline.readString('querymode') of
    'random': queryMode := qmFPCRandomQueryList;
    else {'xorshift': }querymode := qmXorShift;
  end;
  case cmdline.readString('keyuniqueness') of
    'number': begin
      keyuniqueness := kuNumber;
      if sources <> nil then
        for i := 0 to sources.count - 1 do
          if pos(KeyNumberCharacter, sources[i]) > 0 then sources[i] := StringReplace(sources[i], KeyNumberCharacter, '_', [rfReplaceAll]);
    end;
    'filter': keyuniqueness := kuFilter;
    else raise exception.create('Invalid argument: keyuniqueness')
  end;
  dumpdatafn := cmdline.readString('dumpdata');
  if dumpdatafn <> '' then begin
    assignfile(dumpfile, dumpdatafn);
    rewrite(dumpfile);
  end;
  cacheddatafn := cmdline.readString('cacheddata');
  if cacheddatafn <> '' then begin
    assignfile(cacheddata, cacheddatafn);
    reset(cacheddata);
  end;

  basekeycount := cmdline.readInt('basekeycount');
  if basekeycount = 0 then basekeycount := keycount;
  maxkeycount := cmdline.readInt('maxkeycount');

  cmdline.free;



  data := nil;
  faildata := nil;

  if (cacheddatafn = '') and (keyUniqueness = kuFilter) then referenceHashmap := TReferenceHashmap.create
  else referenceHashmap := nil;
  repeat
    if failqueryperkey > 0 then failkeycount := max(100, keycount div 100)
    else failkeycount := 0;
    //if keycount > 2000 then break;

    referenceConflict := false; //for warnings
    s := '';

    if runMode = rmDumpData then oldkeycount := 0
    else oldkeycount := length(data);
    if length(data) <> keycount then SetLength(data, keycount);
    oldfailkeycount := length(faildata);
    SetLength(faildata, failkeycount);
    addkeycount := keycount - oldkeycount;
    //writeln(addkeycount, ' ', oldkeycount);
    if cacheddatafn = '' then begin
      if referenceHashmap <> nil then
        if (runMode <> rmDumpData) and (referenceHashmap.Capacity < keycount + failkeycount) then
          if  (referenceHashmap.Capacity < 1000000) then
            referenceHashmap.Capacity := (keycount + failkeycount) * 5
           else
             referenceHashmap.Capacity := keycount + failkeycount;
      for i := 0 to keycount + failkeycount - 1 - oldkeycount - oldfailkeycount do begin
        inc(totalkeycount);
        if sources <> nil then
          s := sources[i mod sources.count];
        case keyuniqueness of
          kuNumber: begin
            if sources <> nil then s := s + KeyNumberCharacter + inttostr(totalkeycount)
            else begin
              setlength(s, keylen);
              for j := 1 to keylen - 4 do
                s[j] := chr(Random(200)+32);
              pinteger(@s[length(s) - 4 + 1])^ := totalkeycount xor $12345678;
            end;
          end;
          kuFilter: begin
            repeat
              if sources = nil then begin
                setlength(s, keylen);
                for j := 1 to keylen do
                  s[j] := chr(Random(200)+32);
              end else if referenceConflict then
                s := s + chr(Random(200)+32);
              while length(s) < keylen do
                s := s + chr(Random(200)+32);
              referenceConflict := TReferenceHashmapContains.contains(referenceHashmap, s);
            until not referenceConflict;
          end;
        end;
        if i < addkeycount then begin
          data[i + oldkeycount] := s;
          if referenceHashmap <> nil then
            TReferenceHashmapAdd.add(referenceHashmap, s, @data[i]); //these added pointers will become invalid after resizing
        end else begin
          faildata[i - addkeycount + oldfailkeycount] := s;
          if referenceHashmap <> nil then
            TReferenceHashmapAdd.add(referenceHashmap, s, @faildata[i - addkeycount]);
        end;
      end;
    end else begin
      for i := 0 to keycount - oldkeycount - 1 do begin
        if eof(cacheddata) then begin writeln(stderr, 'data source exhausted'); flushall; halt; end;
        readln(cacheddata, data[i + oldkeycount]);
      end;
      for i := 0 to failkeycount - oldfailkeycount - 1 do begin
        if eof(cacheddata) then begin writeln(stderr, 'data source exhausted'); flushall; halt; end;
        readln(cacheddata, faildata[i + oldfailkeycount]);
      end;
      totalkeycount := length(data);
    end;


    if dumpdatafn <> '' then begin
      if runMode <> rmDumpData then rewrite(dumpfile);
      for i := 0 to keycount - 1 do
        writeln(dumpfile, data[i]);
      flush(dumpfile);
      writeln(stderr, 'key count: ', totalkeycount, ' ', sourcefile, ' keylen: ', keylen);
    end else begin
      writeln(stderr, 'key count: ', totalkeycount, ' ', sourcefile, ' keylen: ', keylen, ' read/write: ', queryperkey, ' fail/write: ', failqueryperkey);
    end;
    flushall;

    if queryMode = qmFPCRandomQueryList then begin
      SetLength(randomqueries, keycount * (queryperkey + failqueryperkey));
      for i := 0 to keycount - 1 do begin
        for j := 0 to queryperkey - 1 do
          randomqueries[i * (queryperkey + failqueryperkey) + j] := Random(i);
        for j := 0 to failqueryperkey - 1 do
          randomqueries[i * (queryperkey + failqueryperkey) + queryperkey + j] := Random(failkeycount);
      end;
    end;

    benchmark(mkHash, 'contnrs.TFPHashList', 'shortstring -> pointer', @TTestFPHashList.test);
    benchmark(mkHash, 'contnrs.TFPDataHashTable', 'string -> pointer', @TTestFPHashTable.test);
    benchmark(mkHash, 'ghashmap.THashMap', '* -> *', @TTestGHashMap.test);
    benchmark(mkHash, 'ghashmap.THashMap.shortstring', '* -> *', @TTestGHashMapShortString.test);
    benchmark(mkTree, 'gmap.TMap', '* -> *', @TTestGMap.test);
    benchmark(mkTree, 'gmap.TMap.shortstring', '* -> *', @TTestGMapShortString.test);
    benchmark(mkArray, 'fgl.TFPGMap (sorted)', '* -> *', @TTestFPGMap.test);
    benchmark(mkArray, 'classes.TStringList_(sorted)', 'string -> TObject', @TTestStringList.test);
    {$ifdef benchmarkIniFiles}benchmark(mkHash, 'inifiles.TStringHash', 'string -> integer', @testIniFiles);{$endif}

    {$ifdef benchmarkLAZFGLHash}benchmark(mkHash, 'lazfglhash.TLazFPGHashTable', 'string -> *', @TTestLazFPGHashTable.test);{$endif}
    {$ifdef benchmarkLAZXMLUTILS}benchmark(mkHash, 'laz2_xmlutils.THashTable', 'string -> TObject', @TTestLazXMLHashTable.test);{$endif}

    {$ifdef benchmarkGenerics}
    benchmark(mkHash, 'rtl-generics_linear', '* -> *', @TTestGenericLinear.test);
    {$ifdef benchmarkGenericsQuadraticProbing}benchmark(mkHash, 'rtl-generics_quadratic', '* -> *', @TTestGenericQuadratic.test);{$endif}
    benchmark(mkHash, 'rtl-generics_double', '* -> *', @TTestGenericDouble.test);
    benchmark(mkHash, 'rtl-generics_cuckoo2', '* -> *', @TTestGenericCuckooD2.test);
    benchmark(mkHash, 'rtl-generics_cuckoo4', '* -> *', @TTestGenericCuckooD4.test);
    benchmark(mkHash, 'rtl-generics_cuckoo6', '* -> *', @TTestGenericCuckooD6.test);

    benchmark(mkHash, 'rtl-generics_linear.shortstring', '* -> *', @TTestGenericLinearShortString.test);
    {$ifdef benchmarkGenericsQuadraticProbing}benchmark(mkHash, 'rtl-generics_quadratic.shortstring', '* -> *', @TTestGenericQuadraticShortString.test);{$endif}
    benchmark(mkHash, 'rtl-generics_double.shortstring', '* -> *', @TTestGenericDoubleShortString.test);
    benchmark(mkHash, 'rtl-generics_cuckoo2.shortstring', '* -> *', @TTestGenericCuckooD2ShortString.test);
    benchmark(mkHash, 'rtl-generics_cuckoo4.shortstring', '* -> *', @TTestGenericCuckooD4ShortString.test);
    benchmark(mkHash, 'rtl-generics_cuckoo6.shortstring', '* -> *', @TTestGenericCuckooD6ShortString.test);
    {$endif}
    {$ifdef benchmarkBEROsFLRE}benchmark(mkHash, 'Bero''s_TFLRECacheHashMap', 'string -> TFLRE', @TTestFLRE.test);{$endif}
    {$ifdef benchmarkBEROsPASMP}benchmark(mkParallelHash, 'Bero''s_TPasMPHashTable', '* -> *', @TTestPasMPStringHashTable.test);{$endif}
    {$ifdef benchmarkYAMERsHashmap}benchmark(mkHash, 'Yamer''s_TGenHashMap', '* -> *', @TTestGContnrs.test);{$endif}
    {$ifdef benchmarkYAMERsHashmap}benchmark(mkHash, 'Yamer''s_TGenHashMap.shortstring', '* -> *', @TTestGContnrsShortString.test);{$endif}
    {$ifdef benchmarkBARRYKELLYsHashlist}benchmark(mkHash, 'Barry_Kelly''s THashList (fixed size)', 'string -> pointer', @TTestBKHashList.test);{$endif}
    {$ifdef benchmarkCL4L}benchmark(mkHash, 'CL4L''s_TStrHashMap (fixed size)', 'string -> TObject', @TTestCL4LStrHashMap.test);{$endif}
    {$ifdef benchmarkFundamentals}benchmark(mkHash, 'fundamentals TPointerDictionaryA', 'string -> pointer', @TTestFundamentalsPointerDictionaryA.test);{$endif}
    {$ifdef benchmarkLightContainers}benchmark(mkHash, 'marcov''s lightcontainers', 'string -> pointer', @TTestLightContainers.test);
     benchmark(mkHash, 'marcov''s generic lightcontainers', '* -> *', @TTestGenLightContainers.test);{$endif}
    {$ifdef benchmarkDeCAL}benchmark(mkHash, 'hovadur''s DeCAL ', '* -> *', @TTestDeCAL.test);{$endif}
    {$ifdef benchmarkJUHAsStringHashMap}benchmark(mkHash, 'JUHA''s StringHashMap', 'string -> pointer', @TTestJuhaStrHashMap.test);{$endif}
    {$ifdef benchmarkKEALONsCL4FPC}benchmark(mkHash, 'kealon''s CL4fpc', '* -> *', @TTestKealonsHashMap.test);{$endif}
    {$ifdef benchmarkBBHAMT}benchmark(mkHash, 'BeniBela''s HAMT mutable', '* -> *', @TTestBBHAMTMutable.test);{$endif}
    {$ifdef benchmarkBBHAMT}benchmark(mkHash, 'BeniBela''s HAMT immutable', '* -> *', @TTestBBHAMTImmutable.test);{$endif}
    {$ifdef benchmarkBBHashmap}benchmark(mkHash, 'BeniBela''s_Hashmap', '* -> *', @TTestBBHashmap.test);{$endif}
    {$ifdef benchmarkCustomMap}benchmark(mkHash, 'custom', '?', @TTestCustomMap.test);{$endif}

    if runMode <> rmDumpData then begin
      keycount := keycount + basekeycount;
      if keycount > 10 * basekeycount then begin
        keycount := 2 * 10 * basekeycount;
        basekeycount := basekeycount * 10;
      end;
    end;
  until (runMode in [rmList, rmSingleRun]) or (keycount < 0) or (totalkeycount > maxkeycount);

  referenceHashmap.Free;
  sources.free;
end.

