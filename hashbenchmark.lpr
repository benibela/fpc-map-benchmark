program hashbenchmark;

{$mode objfpc}{$H+}
{$define UseCThreads}

{$define benchmarkGenerics}
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
  Classes,sysutils,math,contnrs,ghashmap,IniFiles,fgl,gmap,lazfglhash
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
  raise exception.create('failed');
end;

var data: array of string;
    randomqueries: array of integer;
    keycount, keylen, queryperkey: integer;

procedure benchmark(kind: TMapKind; name: string; args: string; p: TBenchmarkFunc);
const repcount = 5;
var
  r: Integer;
  tms, mean: double;
  timing: array[1..repcount] of double;
begin
  name := StringReplace(name,' ','_',[rfReplaceAll]);
  if (kind = mkArray) and (keycount > 10000) then exit;
  if (kind = mkTree) and (keycount > 100000) then exit;
  mean := 0;
  for r := 1 to repcount do begin
    tms := frac(now)*MSecsPerDay;
    p().free;
    timing[r] := frac(now)*MSecsPerDay - tms;
    mean += timing[r];
  end;
  writeln(name, ' ', keycount, ' ', round(mean), ' +- ', round(stddev(pdouble(@timing[1]), repcount)));
end;

type generic TG_CallAddXCast<__TMap, TCast> = class
  class procedure add(map: __TMap; const key: string; value: TCast); static; inline;
  end;
  generic TG_CallAdd<TMap> = class(specialize TG_CallAddXCast<TMap, pointer>);
  generic TG_CallInsert<TMap> = class
    class procedure add(map: TMap; const key: string; value: pointer); static; inline;
  end;
  generic TG_GetValue<TMap> = class
    class function get(map: TMap; const key: string): pointer; static; inline;
  end;
  generic TG_GetDefault<TMap> = class
    class function get(map: TMap; const key: string): pointer; static; inline;
  end;
  generic TG_TestXXX<TMap, TAdder, TGetter, TCast> = class
    class function test: TObject; static;
  end;
  generic TG_TestXDefaultXCast<__TMap, TAdder, TCast> = class(specialize TG_TestXXX<__TMap, TAdder, specialize TG_GetDefault<__TMap>, TCast>);
  generic TG_TestXDefault<__TMap, TAdder> = class(specialize TG_TestXDefaultXCast<__TMap, TAdder, pointer>);
  generic TG_TestAddDefault<__TMap> = class(specialize TG_TestXDefault<__TMap, specialize TG_CallAdd<__TMap>>);
  generic TG_TestInsertDefault<__TMap> = class(specialize TG_TestXDefault<__TMap, specialize TG_CallInsert<__TMap>>);

class procedure TG_CallAddXCast.add(map: __TMap; const key: string; value: TCast); static; inline;
begin
  map.add(key, value);
end;
class procedure TG_CallInsert.add(map: TMap; const key: string; value: pointer); static; inline;
begin
  map.insert(key, value);
end;
class function TG_GetValue.get(map: TMap; const key: string): pointer; static; inline;
begin
  result := pointer(map.getvalue(key));
end;
class function TG_GetDefault.get(map: TMap; const key: string): pointer; static; inline;
begin
  result := pointer(map[key]);
end;

class function TG_TestXXX.test(): TObject;
var q, i, j: integer;
  map: TMap;
begin
  map := TMap.create;
  q := 0;
  for i := 0 to keycount - 1 do begin
    TAdder.add(map, data[i], TCast(@data[i]));
    for j := 0 to queryperkey - 1 do begin
      if TGetter.get(map, data[randomqueries[q]]) <> @data[randomqueries[q]] then fail;
      inc(q);
    end;
  end;
  result := map;
end;


function testfphashlist: TObject;
var q, i, j: integer;
  fphashlist: contnrs.TFPHashList;
begin
  fphashlist := TFPHashList.Create;
  q := 0;
  for i := 0 to keycount - 1 do begin
    fphashlist.Add(data[i], @data[i]);
    for j := 0 to queryperkey - 1 do begin
      if fphashlist.Find(data[randomqueries[q]]) <> @data[randomqueries[q]] then fail;
      inc(q);
    end;
  end;
  result := fphashlist;
end;

type TTestFPHashTable = specialize TG_TestAddDefault<TFPDataHashTable>;

type TStringHash = class
  class function c(const a,b: string): boolean;
  class function rawhash(const s: string): SizeUInt; inline; static;
  class function hash(const s: string; n: SizeUInt): SizeUInt; inline; static;
  //equal(const AKey1, AKey2: TKey): Boolean;
end;

type
  TTestGHashMap = specialize TG_TestInsertDefault<specialize THashmap<string, pointer, TStringHash>>;
  TTestGMap = specialize TG_TestInsertDefault<specialize TMap<string, pointer, TStringHash>>;
  TTestFPGMap = specialize TG_TestAddDefault<specialize TFPGMap<string, pointer>>;

function testStringList: TObject;
var q, i, j: integer;
  map: TStringList;
begin
  map := TStringList.Create;
  map.Sorted := true;
  q := 0;
  for i := 0 to keycount - 1 do begin
    map.AddObject(data[i], tobject(@data[i]));
    for j := 0 to queryperkey - 1 do begin
      if map.Objects[map.IndexOf(data[randomqueries[q]])] <> tobject(@data[randomqueries[q]]) then fail;
      inc(q);
    end;
  end;
  result := map;
end;

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




type TTestLazFPGHashTable = specialize TG_TestAddDefault<specialize TLazFPGHashTable<pointer>>;


{$ifdef benchmarkBEROsFLRE}
type TTestFLRE = specialize TG_TestXDefaultXCast<TFLRECacheHashMap, specialize TG_CallAddXCast<TFLRECacheHashMap, TFLRECacheHashMapData>, TFLRECacheHashMapData>;
{$endif}

{$ifdef BENCHMARKBEROSPASMP}
function testPASMP: TObject;
var q, i, j: integer;
  map: TPasMPStringHashTable;
  p: PAnsiString;
begin
  map := TPasMPStringHashTable.Create(sizeof(pointer));
  q := 0;
  for i := 0 to keycount - 1 do begin
    p := @data[i];
    map.SetKeyValue(data[i], p);
    for j := 0 to queryperkey - 1 do begin
      map.GetKeyValue(data[randomqueries[q]], p);
      if  p <> @data[randomqueries[q]] then fail;
      inc(q);
    end;
  end;
  result := map;
end;
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
function testBKHashList: TObject;
var q, i, j: integer;
  map: HashList.THashList;
  p: PAnsiString;
  trait: hashlist.TCaseSensitiveTraits;
begin
  trait := hashlist.TCaseSensitiveTraits.Create;
  map := thashlist.Create(trait, keycount);
  q := 0;
  for i := 0 to keycount - 1 do begin
    p := @data[i];
    map.Add(data[i], p);
    for j := 0 to queryperkey - 1 do begin
      if map.Data[data[randomqueries[q]]] <> @data[randomqueries[q]] then fail;
      inc(q);
    end;
  end;
  trait.free;
  result := map;
end;
{$endif}

{$ifdef benchmarkCL4L}
type TMyStrHashMap = class(TStrHashMap)
  constructor create;
  procedure insert(const key: string; value: pointer); inline;
end;
   TTestCL4LStrHashMap = class(specialize TG_TestXXX<TMyStrHashMap, specialize TG_CallInsert<TMyStrHashMap>, specialize TG_GetValue<TMyStrHashMap>, pointer>);

constructor TMyStrHashMap.create;
begin
  inherited create(10, false);
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
type TMyDMap = class(DMap)
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
type TTestDeCAL = class(specialize TG_TestXXX<TMyDMap, specialize TG_CallAdd<TMyDMap>, specialize TG_GetValue<TMyDMap>, pointer>);
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
  i, j: Integer;
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
  cmdline.declareInt('queryperkey', 'queryperkey', 10);

  sourcefile := cmdline.readString('sources');

  keycount := cmdline.readInt('keycount');
  keylen := cmdline.readInt('keylen');
  queryperkey := cmdline.readInt('queryperkey');

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
  cmdline.free;

  SetLength(data, keycount);
  fphashlist := TFPHashList.Create;
  fphashlist.Capacity := 5 * keycount;
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

  writeln(stderr, 'Data count: ', length(data));

  SetLength(randomqueries, keycount * queryperkey);
  for i := 0 to keycount - 1 do
    for j := 0 to queryperkey - 1 do
      randomqueries[i * queryperkey + j] := Random(i);


  benchmark(mkHash, 'contnrs.TFPHashList', 'shortstring -> pointer', @testfphashlist);
  benchmark(mkHash, 'contnrs.TFPDataHashTable', 'string -> pointer', @TTestFPHashTable.test);
  benchmark(mkHash, 'ghashmap.THashMap', '* -> *', @TTestGHashMap.test);
  benchmark(mkTree, 'gmap.TMap', '* -> *', @TTestGMap.test);
  benchmark(mkArray, 'fgl.TFPGMap', '* -> *', @TTestFPGMap.test);
  benchmark(mkArray, 'sysutils.TStringList_(sorted)', 'string -> TObject', @testStringList);
  benchmark(mkHash, 'inifiles.TStringHash', 'string -> integer', @testIniFiles);
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
  {$ifdef benchmarkBEROsPASMP}benchmark(mkParallelHash, 'Bero''s_TPasMPHashTable', '* -> *', @testPASMP);{$endif}
  {$ifdef benchmarkYAMERsHashmap}benchmark(mkHash, 'Yamer''s_TGenHashMap', '* -> *', @TTestGContnrs.test);{$endif}
  {$ifdef benchmarkBARRYKELLYsHashlist}benchmark(mkHash, 'Barry_Kelly''s_THashList_(fixed_size)', 'string -> pointer', @testBKHashList);{$endif}
  {$ifdef benchmarkCL4L}benchmark(mkHash, 'CL4L''s_TStrHashMap', 'string -> TObject', @TTestCL4LStrHashMap.test);{$endif}
  {$ifdef benchmarkFundamentals}benchmark(mkHash, 'fundamentals TPointerDictionaryA', 'string -> pointer', @TTestFundamentalsPointerDictionaryA.test);{$endif}
  {$ifdef benchmarkLightContainers}benchmark(mkHash, 'marcov''s generic lightcontainers', '* -> *', @TTestLightContainers.test);{$endif}
  {$ifdef benchmarkDeCAL}benchmark(mkHash, 'hovadur''s DeCAL ', '* -> *', @TTestDeCAL.test);{$endif}
  {$ifdef benchmarkJUHAsStringHashMap}benchmark(mkHash, 'JUHA''s StringHashMap', 'string -> pointer', @TTestJuhaStrHashMap.test);{$endif}
  {$ifdef benchmarkKEALONsCL4FPC}benchmark(mkHash, 'kealon''s CL4fpc', '* -> *', @TTestKealonsHashMap.test);{$endif}

  sources.free;
end.

