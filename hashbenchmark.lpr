program hashbenchmark;

{$mode objfpc}{$H+}
{$define UseCThreads}

//{$define benchmarkGenerics} does not compile
{$define benchmarkBEROsFLRE}
{$define benchmarkBEROsPASMP}
{$define benchmarkYAMERsHashmap}
{$define benchmarkBARRYKELLYsHashlist}

uses
  //heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,sysutils,math,contnrs,ghashmap,IniFiles,fgl,gmap,lazfglhash
  {$ifdef benchmarkGenerics}, Generics.Collections{$endif} //https://github.com/dathox/generics.collections
  {$ifdef benchmarkBEROsFLRE}, FLRE{$endif} //https://github.com/BeRo1985/flre/
  {$ifdef BENCHMARKBEROSPASMP}, PasMP{$endif} //https://github.com/BeRo1985/pasmp/
  {$ifdef benchmarkYAMERsHashmap}, gcontnrs{$endif} //http://yann.merignac.free.fr/
  {$ifdef benchmarkBARRYKELLYsHashlist},HashList{$endif} //no idea where it came from, but aminer used it there https://sites.google.com/site/aminer68/scalable-parallel-hashlist
  { you can add units after this };

//set trees (usable as map with your own pair class): AvgLvlTree.TStringToStringTree, AVL_Tree

type TMapKind = (mkHash, mkParallelHash, mkTree, mkArray);

procedure fail;
begin
  raise exception.create('failed');
end;

var data: array of string;
    randomqueries: array of integer;
    keycount, keylen, queryperkey: integer;

procedure benchmark(kind: TMapKind; name: string; args: string; p: TProcedure);
const repcount = 5;
var
  r: Integer;
  tms, mean: double;
  timing: array[1..repcount] of double;
begin
  if (kind = mkArray) and (keycount > 10000) then exit;
  if (kind = mkTree) and (keycount > 100000) then exit;
  mean := 0;
  for r := 1 to repcount do begin
    tms := frac(now)*MSecsPerDay;
    p();
    timing[r] := frac(now)*MSecsPerDay - tms;
    mean += timing[r];
  end;
  writeln(name, ' ', keycount, ' ', round(mean), ' +- ', round(stddev(pdouble(@timing[1]), repcount)));
end;

procedure testfphashlist;
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
  fphashlist.Free;
end;

procedure testfphashtable;
var q, i, j: integer;
  map: TFPObjectHashTable;
begin
  map := TFPObjectHashTable.Create(false);
  q := 0;
  for i := 0 to keycount - 1 do begin
    map.Add(data[i], tobject(@data[i]));
    for j := 0 to queryperkey - 1 do begin
      if map.Items[data[randomqueries[q]]] <> tobject(@data[randomqueries[q]]) then fail;
      inc(q);
    end;
  end;
  map.Free;
end;

type TStringHash = class
  class function c(const a,b: string): boolean;
  class function rawhash(const s: string): SizeUInt; inline; static;
  class function hash(const s: string; n: SizeUInt): SizeUInt; inline; static;
  //equal(const AKey1, AKey2: TKey): Boolean;
end;

type
  TMyGHMMap = specialize THashmap<string, pointer, TStringHash>;
procedure testGHashmap;
var q, i, j: integer;
  map: TMyGHMMap;
begin
  map := TMyGHMMap.Create;
  q := 0;
  for i := 0 to keycount - 1 do begin
    map.Items[data[i]] := @data[i];
    for j := 0 to queryperkey - 1 do begin
      if map.Items[data[randomqueries[q]]] <> @data[randomqueries[q]] then fail;
      inc(q);
    end;
  end;
  map.Free;
end;

type TMyGMap = specialize TMap<string, pointer, TStringHash>;
procedure testGmap;
var q, i, j: integer;
  map: TMyGMap;
begin
  map := TMyGMap.Create;
  q := 0;
  for i := 0 to keycount - 1 do begin
    map.Insert(data[i], @data[i]);
    for j := 0 to queryperkey - 1 do begin
      if map.GetValue(data[randomqueries[q]]) <> @data[randomqueries[q]] then fail;
      inc(q);
    end;
  end;
  map.Free;
end;

type TMyFPGMap = class(specialize TFPGMap<string, pointer>);
procedure testFPGmap;
var q, i, j: integer;
  map: TMyFPGMap;
begin
  map := TMyFPGMap.Create;
  q := 0;
  for i := 0 to keycount - 1 do begin
    map.AddOrSetData(data[i], @data[i]);
    for j := 0 to queryperkey - 1 do begin
      if map.KeyData[data[randomqueries[q]]] <> @data[randomqueries[q]] then fail;
      inc(q);
    end;
  end;
  map.Free;
end;

procedure testStringList;
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
  map.Free;
end;

procedure testIniFiles;
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
  map.Free;
end;

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



type TMyLazFPGHashTable = class(specialize TLazFPGHashTable<pointer>);
procedure testLazFPGHashTable;
var q, i, j: integer;
  map: TMyLazFPGHashTable;
begin
  map := TMyLazFPGHashTable.Create;
  q := 0;
  for i := 0 to keycount - 1 do begin
    map.Add(data[i], @data[i]);
    for j := 0 to queryperkey - 1 do begin
      if map.Items[data[randomqueries[q]]] <> @data[randomqueries[q]] then fail;
      inc(q);
    end;
  end;
  map.Free;
end;



{$ifdef benchmarkBEROsFLRE}
procedure testFLRE;
var q, i, j: integer;
  map: TFLRECacheHashMap;
begin
  map := TFLRECacheHashMap.Create;
  q := 0;
  for i := 0 to keycount - 1 do begin
    map.Add(data[i], TFLRECacheHashMapData(@data[i]));
    for j := 0 to queryperkey - 1 do begin
      if map.Values[data[randomqueries[q]]] <> TFLRECacheHashMapData(@data[randomqueries[q]]) then fail;
      inc(q);
    end;
  end;
  map.Free;
end;
{$endif}

{$ifdef BENCHMARKBEROSPASMP}
procedure testPASMP;
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
  map.Free;
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
procedure testGcontnrs;
var q, i, j: integer;
  map: TMyGContnrsMap;
begin
  map := TMyGContnrsMap.Create;
  q := 0;
  for i := 0 to keycount - 1 do begin
    map.Insert(data[i], @data[i]);
    for j := 0 to queryperkey - 1 do begin
      if map.GetItem(data[randomqueries[q]]) <> @data[randomqueries[q]] then fail;
      inc(q);
    end;
  end;
  map.Free;
end;
{$endif}

{$ifdef BENCHMARKGENERICS}
procedure testGcontnrs;
var q, i, j: integer;
  map: TDictionary;
begin
  map := TMyGContnrsMap.Create;
  q := 0;
  for i := 0 to keycount - 1 do begin
    map.Insert(data[i], @data[i]);
    for j := 0 to queryperkey - 1 do begin
      if map.GetItem(data[randomqueries[q]]) <> @data[randomqueries[q]] then fail;
      inc(q);
    end;
  end;
  map.Free;
end;
{$endif}

{$ifdef benchmarkBARRYKELLYsHashlist}
procedure testBKHashList;
var q, i, j: integer;
  map: HashList.THashList;
  p: PAnsiString;
  trait: TCaseSensitiveTraits;
begin
  trait := TCaseSensitiveTraits.Create;
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
  map.Free;
  trait.free;
end;
{$endif}


var
  s: string;
  i, j: Integer;
  fphashlist: TFPHashList;
  oldptr: Pointer;


begin
  keycount := 50000;
  keylen := 20;
  queryperkey := 3;

  SetLength(data, keycount);
  fphashlist := TFPHashList.Create;
  fphashlist.Capacity := 5 * keycount;
  for i := 0 to keycount - 1 do begin
    repeat
      setlength(s, keylen);
      for j := 1 to keylen do
        s[j] := chr(Random(200)+32);
      oldptr := fphashlist.Find(s);
      if oldptr = nil then break;
      if PString(oldptr)^ <> s then fail;
    until oldptr = nil;
    data[i] := s;
    fphashlist.Add(s, @data[i]);
  end;
  fphashlist.Free;

  SetLength(randomqueries, keycount * queryperkey);
  for i := 0 to keycount - 1 do
    for j := 0 to queryperkey - 1 do
      randomqueries[i * queryperkey + j] := Random(i);


  benchmark(mkHash, 'contnrs.TFPHashList', 'shortstring -> pointer', @testfphashlist);
  benchmark(mkHash, 'contnrs.TFPObjectHashTable', 'string -> TObject', @testfphashtable);
  benchmark(mkHash, 'ghashmap.THashMap', '* -> *', @testGHashmap);
  benchmark(mkTree, 'gmap.TMap', '* -> *', @testGmap);
  benchmark(mkArray, 'fgl.TFPGMap', '* -> *', @testFPGmap);
  benchmark(mkArray, 'sysutils.TStringList (strutils)', 'string -> TObject', @testStringList);
  benchmark(mkHash, 'inifiles.TStringHash', 'string -> integer', @testIniFiles);
  benchmark(mkHash, 'lazfglhash.TLazFPGHashTable', 'string -> *', @testLazFPGHashTable);


  {$ifdef benchmarkGenerics}todo{$endif}
  {$ifdef benchmarkBEROsFLRE}benchmark(mkHash, 'Bero''s_TFLRECacheHashMap', 'string -> TFLRE', @testFLRE);{$endif}
  {$ifdef benchmarkBEROsPASMP}benchmark(mkParallelHash, 'Bero''s_TPasMPHashTable', '* -> *', @testPASMP);{$endif}
  {$ifdef benchmarkYAMERsHashmap}benchmark(mkHash, 'Yamer''s_TGenHashMap', '* -> *', @testGcontnrs);{$endif}
  {$ifdef benchmarkBARRYKELLYsHashlist}benchmark(mkHash, 'Barry_Kelly''s_THashList', 'string -> pointer', @testBKHashList);{$endif}

end.

