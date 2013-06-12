unit uCalc;

interface
uses
  SysUtils, Variants, Types, Classes, IOUtils, Forms, XMLIntf, XMLDoc, StrUtils,
  Windows, DateUtils,
  uMPserv, uDB, uGameItems, uLogger, uQueue, uFactories;

type
  TWishListRec = record
    ID: cardinal;
    GameItem: TMGameItem;
    NeedQty,
    CurrentQty,
    HaveQty: integer;

    procedure Clear;
    function GetBalance: integer;
    function canUse: boolean;
  end;

  TWishListCalc = class
  private
    function WlAdd(name: string): integer; overload;
    function WlAdd(id: cardinal): integer; overload;
    function GetElmIndxByName(name: string): integer;
    function GetElmIndxByID(id: cardinal): integer;

    function GetCount: integer;
    function GetCanUseCount: integer;
    function GetCanUseElmCount: integer;
    function GetCanUseStr: string;
    function GetNeedCount: integer;
  public
    WList: array of TWishListRec;

    constructor Create;
    procedure Clear;

    procedure AddNeedList(lst: string);
    procedure AddCurrList(lst: string);

    procedure SortByBalance;
    procedure FillGiftsCount(world: TMWorld);

    function GetStat: string;

    property Count: integer read GetCount;
    property CanUseCount: integer read GetCanUseCount;
    property CanUseElmCount: integer read GetCanUseElmCount;
    property CanUseStr: string read GetCanUseStr;
    property NeedCount: integer read GetNeedCount;
  end;

implementation

{ TWishListRec }
function TWishListRec.canUse: boolean;
begin
  Result :=
    (NeedQty - CurrentQty <> 0) and
    (HaveQty > 0);
end;

procedure TWishListRec.Clear;
begin
  ID := 0;
  GameItem := nil;
  NeedQty := 0;
  CurrentQty := 0;
  HaveQty := 0;
end;

function TWishListRec.GetBalance: integer;
begin
  Result := NeedQty - CurrentQty - HaveQty;
end;

{ TWishListCalc }

procedure TWishListCalc.AddCurrList(lst: string);
var
  i: integer;
  indx: integer;
  sl: TStringList;
begin
  if lst = '' then exit;

  sl := TStringList.Create;
  sl.NameValueSeparator := ':';
  sl.Delimiter := ',';
  sl.DelimitedText := lst;

  for i := 0 to sl.Count - 1 do
  begin
    indx := GetElmIndxByID(StrToIntDef(sl.Names[i], -1));
    if indx < 0 then indx := WlAdd(sl.Names[i]);
    if indx < 0 then continue;

    WList[indx].CurrentQty := WList[indx].CurrentQty +
      StrToIntDef(sl.ValueFromIndex[i], 1);
  end;

  sl.Free;
end;

procedure TWishListCalc.AddNeedList(lst: string);
var
  i: integer;
  indx: integer;
  sl: TStringList;
begin
  if lst = '' then exit;

  sl := TStringList.Create;
  sl.NameValueSeparator := ':';
  sl.Delimiter := ',';
  sl.DelimitedText := lst;

  for i := 0 to sl.Count - 1 do
  begin
    indx := GetElmIndxByName(sl.Names[i]);
    if indx < 0 then indx := WlAdd(sl.Names[i]);
    if (indx < 0) or (indx >= length(WList)) then continue;

    WList[indx].NeedQty := WList[indx].NeedQty +
      StrToIntDef(sl.ValueFromIndex[i], 1);
  end;

  sl.Free;
end;

procedure TWishListCalc.Clear;
begin
  SetLength(WList, 0);
end;

constructor TWishListCalc.Create;
begin
  inherited;

  Clear;
end;

procedure TWishListCalc.FillGiftsCount(world: TMWorld);
var
  i: integer;
begin
  for i := 0 to length(WList) - 1 do
  begin
    WList[i].HaveQty := WList[i].HaveQty +
      world.GetAvailGiftCount(WList[i].ID);
  end;
end;

function TWishListCalc.GetCount: integer;
begin
  Result := length(WList);
end;

function TWishListCalc.GetElmIndxByID(id: cardinal): integer;
var
 i: integer;
begin
  Result := -1;
  for i := 0 to length(WList) - 1 do
    if WList[i].ID = id then
    begin
      Result := i;
      break;
    end;
end;

function TWishListCalc.GetElmIndxByName(name: string): integer;
var
 i: integer;
begin
  Result := -1;
  for i := 0 to length(WList) - 1 do
    if LowerCase(WList[i].GameItem.Name) = LowerCase(name) then
    begin
      Result := i;
      break;
    end;
end;

procedure TWishListCalc.SortByBalance;
var
 i,
 j: integer;
 t: TWishListRec;
begin
  for i := 0 to length(WList) - 1 do
    for j := i +1 to length(WList) - 1 do
      if WList[i].GetBalance < WList[j].GetBalance then
      begin
        t := WList[j];
        WList[j] := WList[i];
        WList[i] := t;
      end;
end;

function TWishListCalc.WlAdd(name: string): integer;
var
  GItem: TMGameItem;
begin
  Result := -1;
  GItem := TItemsFactory.GetInstance.GetGameItem(name);
  if (GItem = nil) or (GItem.ID = 0) then exit;
  Result := WlAdd(GItem.ID);
end;

function TWishListCalc.WlAdd(id: cardinal): integer;
var
  GItem: TMGameItem;
begin
  Result := -1;
  GItem := TItemsFactory.GetInstance.GetGameItem(id);
  if (GItem.ID = 0) or
     (GItem.ShopDept <> 'materials') or
     (GItem.GetAttr('exclude_from_need_materials').AsBoolean = true)
  then exit;

  SetLength(WList, length(WList) + 1);
  Result := length(WList) - 1;
  WList[Result].Clear;
  WList[Result].ID := GItem.ID;
  WList[Result].GameItem := GItem;
end;

function TWishListCalc.GetCanUseCount: integer;
var
 i: integer;
begin
  Result := 0;

  for i := 0 to length(WList) - 1 do
  if WList[i].canUse then
    Result := Result + WList[i].HaveQty;
end;

function TWishListCalc.GetCanUseElmCount: integer;
var
 i: integer;
begin
  Result := 0;

  for i := 0 to length(WList) - 1 do
  if WList[i].canUse then
    Result := Result + 1;
end;

function TWishListCalc.GetCanUseStr: string;
var
 i: integer;
begin
  Result := '';
  for i := 0 to length(WList) - 1 do
  if WList[i].canUse then
    Result := Result + WList[i].GameItem.RusName + ':' + IntToStr(WList[i].HaveQty) + ',';
  Result := Copy(Result, 1, length(Result) - 1);
end;

function TWishListCalc.GetNeedCount: integer;
var
 i: integer;
begin
  Result := 0;

  for i := 0 to length(WList) - 1 do
    if WList[i].GetBalance > 0 then
      Result := Result + WList[i].GetBalance;
end;

function TWishListCalc.GetStat: string;
var
 c: integer;
begin
  c := CanUseElmCount;
  Result :=
    'gifts count: ' + IntToStr(Count) + '(' +
    IntToStr(NeedCount) + ') ' +
    'canuse: ' + IntToStr(CanUseCount);
  if (c <> 0) and (c < 5)  then
    Result := Result + ' (' + CanUseStr + ')';

  if c <> 0 then Result := Result + '!!!';
end;


end.
