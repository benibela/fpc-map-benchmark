program hashbenchmark;

{$mode objfpc}{$H+}
{$define UseCThreads}

{$define benchmarkGenerics}
//{$define benchmarkIniFiles} just a wrapper around TFPDataHashTable
{$define benchmarkBEROsFLRE}
{$define benchmarkBEROsPASMP}
{$define benchmarkYAMERsHashmap}
{$define benchmarkBARRYKELLYsHashlist}
{$define benchmarkCL4L}
{$define benchmarkFundamentals}
//{$define benchmarkLightContainers} are not compiling
{$define benchmarkDeCAL}
{$define benchmarkJUHAsStringHashMap}
//{$define benchmarkKEALONsCL4FPC} conflicts with benchmarkCL4L as you cannot access generic hashmap when unit hashmap is used (#30646)

uses
  //heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  bbutils, rcmdline,
  Classes,sysutils,math,contnrs,ghashmap
  {$ifdef benchmarkIniFiles},IniFiles{$endif},fgl,gmap,lazfglhash
  {$ifdef benchmarkGenerics}, Generics.Collections{$endif} //https://github.com/dathox/generics.collections
  {$ifdef benchmarkBEROsFLRE}, FLRE{$endif} //https://github.com/BeRo1985/flre/
  {$ifdef BENCHMARKBEROSPASMP}, PasMP{$endif} //https://github.com/BeRo1985/pasmp/
  {$ifdef benchmarkYAMERsHashmap}, gcontnrs{$endif} //http://yann.merignac.free.fr/
  {$ifdef benchmarkBARRYKELLYsHashlist},HashList{$endif} //no idea where it came from, but aminer used it there https://sites.google.com/site/aminer68/scalable-parallel-hashlist
  {$ifdef benchmarkCL4L},Hashmap{$endif} //https://github.com/CynicRus/CL4L
  {$ifdef benchmarkFundamentals},flcDataStructs{$endif}//https://github.com/fundamentalslib/fundamentals5
  {$ifdef benchmarkLightContainers},LightContainers{$endif} //http://www.stack.nl/~marcov/lightcontainers.zip
  {$ifdef benchmarkDeCAL}, DeCAL{$endif}//from https://bitbucket.org/hovadur/decal
  {$ifdef benchmarkJUHAsStringHashMap},StrHashMap{$endif}//from http://wiki.freepascal.org/StringHashMap
  {$ifdef benchmarkKEALONsCL4FPC},hashmaps{$endif} //https://sourceforge.net/projects/cl4fpc/
  { you can add units after this };

//set trees (usable as map with your own pair class): AvgLvlTree.TStringToStringTree, AVL_Tree

type TMapKind = (mkHash, mkParallelHash, mkTree, mkArray);
     TBenchmarkFunc = function: TObject;

procedure fail;
begin
  writeln(stderr, 'failed');
  halt;
end;

var data: array of string;
    randomqueries: array of integer;
    keycount, keylen, queryperkey: integer;
    timelimit, memlimit: int64;
    queryMode: (qmFPCRandomQueryList, qmXorShift);
    runMode: (rmList, rmSingleRun, rmAddativeKeyCount);
    mapFilter: string;

function benchmarkf(kind: TMapKind; name: string; p: TBenchmarkFunc): boolean;
const repcount = 5;
var
  r: Integer;
  tms, mean, meanmemory: double;
  timing: array[1..repcount] of double;
  m: TMemoryManager;
  memory: array[1..repcount] of double;
  heapstatus: THeapStatus;
  map: tobject;
begin
  result := false;
  GetMemoryManager(m);
  mean := 0;
  meanmemory := 0;
  for r := 1 to repcount do begin
    heapstatus := m.GetHeapStatus();
    tms := frac(now)*MSecsPerDay;
    map := p();
    timing[r] := frac(now)*MSecsPerDay - tms;
    memory[r] := m.GetHeapStatus().totalAllocated - heapstatus.totalAllocated;
    map.free;
    if timing[r] > timelimit then begin
      writeln(stderr, name, ' time limit exceeded: ', round(timing[r]), ' > ', timelimit);
      exit;
    end;
    if memory[r] > memlimit then begin
      writeln(stderr, name, ' memory limit exceeded: ', round(memory[r]), ' > ', memlimit);
      exit;
    end;
    mean += timing[r];
    meanmemory += memory[r];
  end;
  mean /= repcount;
  meanmemory /= repcount;
  writeln(name, ' ', keycount, ' ', round(mean), ' +- ', round(stddev(pdouble(@timing[1]), repcount)), ' ', round(meanmemory), ' +- ',  round(stddev(pdouble(@memory[1]), repcount)));
  result := true;
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
  generic TG_TestXXX<TMap, TAdder, TGetter, TCast> = class
    class function test: TObject; static;
  end;
  generic TG_TestXDefaultXCast<__TMap, TAdder, TCast> = class(specialize TG_TestXXX<__TMap, TAdder, specialize TG_CallGetDefault<__TMap>, TCast>);
  generic TG_TestXDefault<__TMap, TAdder> = class(specialize TG_TestXDefaultXCast<__TMap, TAdder, pointer>);
  generic TG_TestAddDefault<__TMap> = class(specialize TG_TestXDefault<__TMap, specialize TG_CallAdd<__TMap>>);
  generic TG_TestInsertDefault<__TMap> = class(specialize TG_TestXDefault<__TMap, specialize TG_CallInsert<__TMap>>);

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

class function TG_TestXXX.test(): TObject;
var q, i, j: integer;
  map: TMap;
  xorshift: cardinal;
begin
  map := TMap.create;
  q := 0;
  xorshift := 314159265;
  for i := 0 to keycount - 1 do begin
    TAdder.add(map, data[i], TCast(@data[i]));
    case queryMode of
      qmFPCRandomQueryList:
        for j := 0 to queryperkey - 1 do begin
          if TGetter.get(map, data[randomqueries[q]]) <> @data[randomqueries[q]] then fail;
          inc(q);
        end;
      qmXorShift:
        for j := 0 to queryperkey - 1 do begin
          q := xorshift mod (i + 1);
          if TGetter.get(map, data[q]) <> @data[q] then fail;
          xorshift := xorshift xor (xorshift shl 13);
          xorshift := xorshift xor (xorshift shr 17);
          xorshift := xorshift xor (xorshift shl 5);
        end;
    end;
  end;
  result := map;
end;


type
  TTestFPHashList = specialize TG_TestXXX<contnrs.TFPHashList, specialize TG_CallAdd<TFPHashList>, specialize TG_CallGetFind<TFPHashList>, pointer>;
  TTestFPHashTable = specialize TG_TestAddDefault<contnrs.TFPDataHashTable>;

type TStringHash = class
  class function c(const a,b: string): boolean;
  class function rawhash(const s: string): SizeUInt; inline; static;
  class function hash(const s: string; n: SizeUInt): SizeUInt; inline; static;
  //equal(const AKey1, AKey2: TKey): Boolean;
end;

type TMyFPGMap = class(specialize TFPGMap<string, pointer>)
  constructor create;
end;

constructor TMyFPGMap.create;
begin
  inherited;
  sorted := true;
end;

type
  TTestGHashMap = specialize TG_TestInsertDefault<specialize THashmap<string, pointer, TStringHash>>;
  TTestGMap = specialize TG_TestInsertDefault<specialize TMap<string, pointer, TStringHash>>;
  TTestFPGMap = specialize TG_TestAddDefault<TMyFPGMap>;

  TStringListSorted = class(classes.TStringList)
    constructor create;
    function getValue(const key: string): pointer; inline;
  end;

  constructor TStringListSorted.create;
  begin
    inherited;
    sorted := true;
  end;

  function TStringListSorted.getValue(const key: string): pointer; inline;
  begin
    result := pointer(Objects[IndexOf(key)]);
  end;

type TTestStringList = specialize TG_TestXXX<TStringListSorted, specialize TG_CallAddObject<TStringListSorted>, specialize TG_CallGetValue<TStringListSorted>, TObject>;

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


type TTestLazFPGHashTable = specialize TG_TestAddDefault<specialize TLazFPGHashTable<pointer>>;


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
  GetKeyValue(key, result);
end;
type TTestPasMPStringHashTable = specialize TG_TestXXX<TMyPasMPStringHashTable, specialize TG_CallSetKeyValue<TMyPasMPStringHashTable>, specialize TG_CallGetValue<TMyPasMPStringHashTable>, pointer>;
{$endif}

{$ifdef BENCHMARKYAMERSHASHMAP}
type TMyGContnrsMap = class(specialize TGenHashMap<string, pointer>)
  function DefaultHashKey(const Key: string): Integer; override;
  function DefaultKeysEqual(const A, B: string): Boolean; override;
end;

function TMyGContnrsMap.DefaultHashKey(const Key: string): Integer;
begin
  Result:=TStringHash.rawhash(key);
end;

function TMyGContnrsMap.DefaultKeysEqual(const A, B: string): Boolean;
begin
  result := a = b;
end;
type TTestGContnrs = specialize TG_TestInsertDefault<TMyGContnrsMap>;
{$endif}

{$ifdef BENCHMARKGENERICS}
type
  TTestGenericLinear = specialize    TG_TestAddDefault<specialize TOpenAddressingLP<string, pointer>>;
  TTestGenericQuadratic = specialize TG_TestAddDefault<specialize TOpenAddressingQP<string, pointer>>;
  TTestGenericDouble = specialize    TG_TestAddDefault<specialize TOpenAddressingDH<string, pointer>>;
  TTestGenericCuckooD2 = specialize  TG_TestAddDefault<specialize TCuckooD2<string, pointer>>;
  TTestGenericCuckooD4 = specialize  TG_TestAddDefault<specialize TCuckooD4<string, pointer>>;
  TTestGenericCuckooD6 = specialize  TG_TestAddDefault<specialize TCuckooD6<string, pointer>>;
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
type TTestLightContainers = specialize TG_TestAddDefault<TLightStringMap<pointer>>;
{$endif}

{$ifdef benchmarkDeCAL}
type TMyDMap = class(DeCAL.DMap)
  procedure add(const s: string; v: pointer); inline;
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

class function TStringHash.c(const a, b: string): boolean;
begin
  result := a < b;
end;

class function TStringHash.rawhash(const s: string): SizeUInt;
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

class function TStringHash.hash(const s: string; n: SizeUInt): SizeUInt; static; //as in contrnrs
begin
  result := rawhash(s) and (n - 1);
end;

var
  s: string;
  i, j, basekeycount: Integer;
  fphashlist: TFPHashList;
  oldptr: Pointer;

  cmdline: TCommandLineReader;
  temps, sourcefile: string;
  sources,sources2: tstringlist;
begin
  cmdline := TCommandLineReader.create;
  cmdline.declareString('sources', 'Source file');
  cmdline.declareInt('keycount', 'keycount', 0);
  cmdline.declareInt('keylen', 'keylen', 0);
  cmdline.declareInt('queriesperkey', 'queryperkey', 100);
  cmdline.declareInt('memlimit', 'mem limit (MB)', 1024);
  cmdline.declareInt('timelimit', 'time limit', 10*60*1000);
  cmdline.declareString('mode', ' list, single-run, multi-run', 'single-run');
  cmdline.declareString('querymode', 'randomlist or xorshift', 'xorshift');
  cmdline.declareString('filter', ' Map to use', '');

  timelimit := cmdline.readInt('timelimit');
  memlimit := int64(cmdline.readInt('memlimit')) * 1024 * 1024;

  sourcefile := cmdline.readString('sources');

  keycount := cmdline.readInt('keycount');
  keylen := cmdline.readInt('keylen');
  queryperkey := cmdline.readInt('queriesperkey');

  if sourcefile = '' then begin
    if keycount = 0 then keycount := 10000;
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
    'single-run': runmode := rmSingleRun;
    'multi-run': runmode := rmAddativeKeyCount;
  end;
  case cmdline.readString('querymode') of
    'randomlsit': queryMode := qmFPCRandomQueryList;
    'xorshift': querymode := qmXorShift;
  end;
  cmdline.free;

  basekeycount := keycount;

  repeat
    SetLength(data, keycount);
    fphashlist := TFPHashList.Create;
    fphashlist.Capacity := keycount*2;
    for i := 0 to keycount - 1 do begin
      if sources <> nil then begin
        oldptr := nil;
        s := sources[i mod sources.count];
      end;
      repeat
        if sources = nil then begin
          setlength(s, keylen);
          for j := 1 to keylen do
            s[j] := chr(Random(200)+32);
        end else if oldptr <> nil then
          s := s + chr(Random(200)+32);
        while length(s) < keylen do
          s := s + chr(Random(200)+32);
        oldptr := fphashlist.Find(s);
        if oldptr = nil then break;
        if PString(oldptr)^ <> s then fail;
      until oldptr = nil;
      data[i] := s;
      fphashlist.Add(s, @data[i]);
    end;
    fphashlist.Free;

    writeln(stderr, 'Data count: ', length(data), ' ', sourcefile, ' keylen: ', keylen, ' read/write: ', queryperkey);

    if queryMode = qmFPCRandomQueryList then begin
      SetLength(randomqueries, keycount * queryperkey);
      for i := 0 to keycount - 1 do
        for j := 0 to queryperkey - 1 do
          randomqueries[i * queryperkey + j] := Random(i);
    end;

    benchmark(mkHash, 'contnrs.TFPHashList', 'shortstring -> pointer', @TTestFPHashList.test);
    benchmark(mkHash, 'contnrs.TFPDataHashTable', 'string -> pointer', @TTestFPHashTable.test);
    benchmark(mkHash, 'ghashmap.THashMap', '* -> *', @TTestGHashMap.test);
    benchmark(mkTree, 'gmap.TMap', '* -> *', @TTestGMap.test);
    benchmark(mkArray, 'fgl.TFPGMap (sorted)', '* -> *', @TTestFPGMap.test);
    benchmark(mkArray, 'sysutils.TStringList_(sorted)', 'string -> TObject', @TTestStringList.test);
    {$ifdef benchmarkIniFiles}benchmark(mkHash, 'inifiles.TStringHash', 'string -> integer', @testIniFiles);{$endif}
    benchmark(mkHash, 'lazfglhash.TLazFPGHashTable', 'string -> *', @TTestLazFPGHashTable.test);


    {$ifdef benchmarkGenerics}
    benchmark(mkHash, 'rtl-generics_linear', '* -> *', @TTestGenericLinear.test);
    benchmark(mkHash, 'rtl-generics_quadratic', '* -> *', @TTestGenericQuadratic.test);
    benchmark(mkHash, 'rtl-generics_double', '* -> *', @TTestGenericDouble.test);
    benchmark(mkHash, 'rtl-generics_cuckoo2', '* -> *', @TTestGenericCuckooD2.test);
    benchmark(mkHash, 'rtl-generics_cuckoo4', '* -> *', @TTestGenericCuckooD4.test);
    benchmark(mkHash, 'rtl-generics_cuckoo6', '* -> *', @TTestGenericCuckooD6.test);
    {$endif}
    {$ifdef benchmarkBEROsFLRE}benchmark(mkHash, 'Bero''s_TFLRECacheHashMap', 'string -> TFLRE', @TTestFLRE.test);{$endif}
    {$ifdef benchmarkBEROsPASMP}benchmark(mkParallelHash, 'Bero''s_TPasMPHashTable', '* -> *', @TTestPasMPStringHashTable.test);{$endif}
    {$ifdef benchmarkYAMERsHashmap}benchmark(mkHash, 'Yamer''s_TGenHashMap', '* -> *', @TTestGContnrs.test);{$endif}
    {$ifdef benchmarkBARRYKELLYsHashlist}benchmark(mkHash, 'Barry_Kelly''s THashList (fixed size)', 'string -> pointer', @TTestBKHashList.test);{$endif}
    {$ifdef benchmarkCL4L}benchmark(mkHash, 'CL4L''s_TStrHashMap (fixed size)', 'string -> TObject', @TTestCL4LStrHashMap.test);{$endif}
    {$ifdef benchmarkFundamentals}benchmark(mkHash, 'fundamentals TPointerDictionaryA', 'string -> pointer', @TTestFundamentalsPointerDictionaryA.test);{$endif}
    {$ifdef benchmarkLightContainers}benchmark(mkHash, 'marcov''s generic lightcontainers', '* -> *', @TTestLightContainers.test);{$endif}
    {$ifdef benchmarkDeCAL}benchmark(mkHash, 'hovadur''s DeCAL ', '* -> *', @TTestDeCAL.test);{$endif}
    {$ifdef benchmarkJUHAsStringHashMap}benchmark(mkHash, 'JUHA''s StringHashMap', 'string -> pointer', @TTestJuhaStrHashMap.test);{$endif}
    {$ifdef benchmarkKEALONsCL4FPC}benchmark(mkHash, 'kealon''s CL4fpc', '* -> *', @TTestKealonsHashMap.test);{$endif}


    keycount := keycount + basekeycount;
    if keycount > 10 * basekeycount then begin
      keycount := 2 * 10 * basekeycount;
      basekeycount := basekeycount * 10;
    end;
  until (runMode <> rmAddativeKeyCount) or (keycount < 0);

  sources.free;
end.

