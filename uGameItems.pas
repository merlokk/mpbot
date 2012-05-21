unit uGameItems;

interface
uses
  Windows, SysUtils, Variants, Classes, StrUtils, superobject,
  uDefs, uQueue;

type
  TAttrType = (atNone, atBigInt, atInt, atBool, atVarchar, atFloat);
const
  StrAttrType: array[atNone .. atFloat] of string = (
    'ASVARCHAR',
    'ASBIGINT',
    'ASINT',
    'ASBOOL',
    'ASVARCHAR',
    'ASFLOAT');
type
  TMWorld = class;
  TMRoom = class;

  TAttrTransform = (attNone, attVOkv, attVOkr, attVOarray, attVOarrayKV);
  TAttr = record
    ID: integer;
    Name: string;
    AType: TAttrType;
    ATransform: TAttrTransform;
    Value: Variant;

    procedure Clear;
    function StrSetVal(s: string): boolean;
    function StrDBGetVal: string;

    function AsInt64: int64;
    function AsInteger: integer;
    function AsBoolean: boolean;
    function AsString: string;
    function AsFloat: Double;
  end;
  TAttrs = array of TAttr;

  TMGameItemType = (gitNone, gitPlace, gitFactory, gitBuilding, gitHouse);
  TMGameItem = class
  private
    FID: integer;
    FName,
    FShopDept,
    FSuperClass,
    FRusName: string;
    FAttr: TAttrs;

    function GetAttrIndx(id: integer): integer; overload;
    function GetAttrIndx(name: string): integer; overload;

    function GetIsFactory: boolean;
    function GetIsBuilding: boolean;
    function GetIsHouse: boolean;
    function GetItemType: TMGameItemType;

    function GetcanClean: boolean;
    function GetcanPick: boolean;
    function GetcanPut: boolean;
    function GetisBuildSite: boolean;
  public
    constructor Create;
    procedure Clear;

    procedure AddAttr(attr: TAttr);
    function GetAttrCount: integer;
    function GetAttrI(indx: integer): TAttr;
    function GetAttr(id: integer): TAttr; overload;
    function GetAttr(name: string): TAttr; overload;

    property ID: integer read FID write FID;
    property Name: string read FName write FName;
    property ShopDept: string read FShopDept write FShopDept;
    property SuperClass: string read FSuperClass write FSuperClass;
    property RusName: string read FRusName write FRusName;

    property canPick: boolean read GetcanPick;
    property canClean: boolean read  GetcanClean;
    property canPut: boolean read  GetcanPut;
    property isBuildSite: boolean read GetisBuildSite;
    property ItemType: TMGameItemType read GetItemType;
    property isFactory: boolean read GetIsFactory;
    property isBuilding: boolean read GetIsBuilding;
    property isHouse: boolean read GetIsHouse;
  end;

  TGiftRec = packed record
  private
    function GetGameItemID: cardinal;
  public
    Name: String;
    ID: Cardinal;
    Qty: integer;
    FromUser: cardinal;

    GameItem: TMGameItem;

    property GameItemID: cardinal read GetGameItemID;
    procedure Clear;
  end;

  TMField = class
  private
    FRoom: TMRoom;
    FGameItem: TMGameItem;
    FContractOutputItem: TMGameItem;
    FSerial: cardinal;
    function GetFieldType: TMGameItemType;
    procedure SetRoom(const Value: TMRoom);

  public
    isDeny: boolean;
    ID: int64;
    Name: string;
    State,
    x,
    y: integer;
    Rotated: boolean;
    ContractInput,
    ContractOutput,
    InputFill,
    OutputFill: string;
    ProcessEnd: cardinal;
    BuildingPosition: Int64;
    ExtraValue: cardinal;

    LastTickDT,
    LastUpdate: TDateTime;

    Qu: TActionQueue;

    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear; virtual;

    function GetProcessEndDT: TDateTime;

    property Serial: cardinal read FSerial write FSerial;
    property GameItem: TMGameItem read FGameItem write FGameItem;
    property ContractOutputItem: TMGameItem read FContractOutputItem write FContractOutputItem;

    procedure ClearTick;
    procedure Execute(canTick, canWork: boolean); virtual;

    property FieldType: TMGameItemType read GetFieldType;
    property Room: TMRoom read FRoom write SetRoom;
  end;

  TMFieldPlace = class (TMField)
  public
  end;

  TMFieldFactory = class (TMField)
  public
    PutKlass: string;
    PutGameItem: TMGameItem;

    procedure Clear; override;

    procedure ChangeState(NewState: integer);
    procedure Execute(canTick, canWork: boolean); override;
  end;

  TMFieldBuilding = class (TMField)
  public
    procedure ChangeState(NewState: integer);
    procedure Execute(canTick, canWork: boolean); override;
  end;

  TMFieldHouse = class (TMField)
  public
    procedure ChangeState(NewState: integer);
    procedure Execute(canTick, canWork: boolean); override;
  end;

  TBarnRec = packed record
    Name: String;
    ID: Cardinal;
    Qty: integer;

    GameItem: TMGameItem;

    procedure Clear;
  end;

  TWorldHeader = packed record
    NextTick: cardinal;
    RoomInformation,
    WishListStr: String;
    Tax: integer;
    PopulationMultiplier: extended;
    Population,
    MaxPopulation,
    Fuel,
    Gold,
    Coins: cardinal;
    ServerTime: int64;
    ResourceOptions,
    SessionKey: String;
    Auto: cardinal;
    Exp: int64;
    RoomID,
    RespectLevel,
    Level: integer;
    OwnerID: int64;
    Visitors: String;

    LastTick: cardinal;

    procedure Clear;
    function GetMaxPopulation: int64;
    function GetPopulation: int64;
    function GetFreePopulation: int64;
  end;

  TMRoom = class
  private
    FWorld: TMWorld;
    FID: integer;
    FAvailable: boolean;

    FItems: array of TMField;
    function GetFieldsCount: integer;
    procedure SetWorld(const Value: TMWorld);
  public
    Header: TWorldHeader;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function StartFieldsUpdate: cardinal;
    function GetField(AID: int64): TMField;
    procedure AddField(field: TMField);
    procedure EndFieldsUpdate(serial: cardinal);
    function GetFieldsCountByType(state: integer): integer;
    function StrFieldsStat: string;

    procedure FieldsClearTick;
    procedure FieldsExecute(MaxCount: integer; canTick, canWork: boolean); virtual;

    property ID: integer read FID write FID;
    property FieldsCount: integer read GetFieldsCount;
    property Avaliable: boolean read FAvailable write FAvailable;
    property World: TMWorld read FWorld write SetWorld;
  end;

  TMWorld = class
  private
    class var CInstance: TMWorld;
    class constructor ClassCreate;
  private
    FSWFRevision,
    FSrvRevision: string;
    FValid,
    FUserDisabled: boolean;

    FRooms: array of TMRoom;

    function GetSrvRevision6: string;
    function GetSWFRevisionAppID: string;
  public
    LastUpdate: TDateTime;
    LastHeader: TWorldHeader;

    Friends: TFriendRecArray;
    Barn: array of TBarnRec;
    RecvdGift: array of TGiftRec;
    AvailGift: array of TGiftRec;

    class function GetInstance: TMWorld;
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function CheckRoomInformation(roominfo: string): boolean;
    function GetRoom(RoomID: integer): TMRoom;
    function AddRoom(RoomID: integer): TMRoom;
    function GetRoomCount: integer;

    function StrGiftStat: String;

    property SWFRevision: string read FSWFRevision write FSWFRevision;
    property SWFRevisionAppID: string read GetSWFRevisionAppID;
    property SrvRevision: string read FSrvRevision write FSrvRevision;
    property SrvRevision6: string read GetSrvRevision6;
    property Valid: boolean read FValid write FValid;
    property UserDisabled: boolean read FUserDisabled write FUserDisabled;
  end;

implementation

{ TMGameItem }

procedure TMGameItem.AddAttr(attr: TAttr);
var
  indx: integer;
begin
  indx := GetAttrIndx(attr.ID);
  if indx < 0 then
  begin
    SetLength(FAttr, length(FAttr) + 1);
    FAttr[length(FAttr) - 1] := attr;
  end
  else
    FAttr[indx] := attr;
end;

procedure TMGameItem.Clear;
begin
  FID := 0;
  FName := '';
  FShopDept := '';
  FSuperClass := '';
  FRusName := '';
  SetLength(FAttr, 0);
end;

constructor TMGameItem.Create;
begin
  inherited;
  Clear;
end;

function TMGameItem.GetAttr(id: integer): TAttr;
var
  indx: integer;
begin
  Result.Clear;
  indx := GetAttrIndx(id);
  if indx < 0 then exit;
  Result := FAttr[indx];
end;

function TMGameItem.GetAttr(name: string): TAttr;
var
  indx: integer;
begin
  Result.Clear;
  indx := GetAttrIndx(name);
  if indx < 0 then exit;
  Result := FAttr[indx];
end;

function TMGameItem.GetAttrCount: integer;
begin
  Result := length(FAttr);
end;

function TMGameItem.GetAttrI(indx: integer): TAttr;
begin
  Result.Clear;
  if indx >= length(FAttr) then exit;
  Result := FAttr[indx];
end;

function TMGameItem.GetAttrIndx(name: string): integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to length(FAttr) - 1 do
    if FAttr[i].Name = name then
    begin
      Result := i;
      break;
    end;
end;

function TMGameItem.GetcanClean: boolean;
begin
  Result := GetAttr('clean').AsBoolean;
end;

function TMGameItem.GetcanPick: boolean;
begin
  Result := GetAttr('pick').AsBoolean;
end;

function TMGameItem.GetcanPut: boolean;
begin
  Result := GetAttr('put').AsBoolean;
end;

function TMGameItem.GetIsBuilding: boolean;
begin
  Result :=
    not isFactory and
    not GetAttr('pseudo_item').AsBoolean and
    not canPut and
    canClean;
end;

function TMGameItem.GetisBuildSite: boolean;
begin
  Result := GetAttr('buildsite').AsBoolean;
end;

function TMGameItem.GetIsFactory: boolean;
begin
  Result :=
    (length(GetAttr('contracts_ary').AsString) > 0) and
    canPut and
    canPick;
end;

function TMGameItem.GetIsHouse: boolean;
begin
  Result :=
    not isFactory and
    not GetAttr('pseudo_item').AsBoolean and
    not canPut and
    canPick;
end;

function TMGameItem.GetItemType: TMGameItemType;
begin
  Result := gitNone;
  if isFactory then
    begin
      Result := gitFactory;
      exit;
    end;

  if isBuilding then
    begin
      Result := gitBuilding;
      exit;
    end;

  if isHouse then
    begin
      Result := gitHouse;
      exit;
    end;
end;

function TMGameItem.GetAttrIndx(id: integer): integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to length(FAttr) - 1 do
    if FAttr[i].ID = id then
    begin
      Result := i;
      break;
    end;
end;

{ TAttr }

function TAttr.AsBoolean: boolean;
begin
  if (AType = atInt) or (AType = atBigInt) or (AType = atBool) then
    Result := Value
  else
    Result := false;
end;

function TAttr.AsFloat: Double;
begin
  if (AType = atInt) or (AType = atBigInt) or (AType = atFloat) then
    Result := Value
  else
    Result := 0;
end;

function TAttr.AsInt64: int64;
begin
  if (AType = atInt) or (AType = atBigInt) then
    Result := Value
  else
    Result := 0;
end;

function TAttr.AsInteger: integer;
begin
  if (AType = atInt) then
    Result := Value
  else
    Result := 0;
end;

function TAttr.AsString: string;
begin
  case AType of
    atBigInt: Result := VarToStr(Value);
    atInt: Result := VarToStr(Value);
    atBool: Result := VarToStr(Value);
    atVarchar: Result := VarToStr(Value);
    atFloat: Result := ReplaceStr(VarToStr(Value), ',', '.');
  else
    Result := '';
  end;
end;

procedure TAttr.Clear;
begin
  ID := 0;
  Name := '';
  AType := atNone;
  ATransform := attNone;
  Value := Null;
end;

function TAttr.StrDBGetVal: string;
begin
  Result := '';
  case AType of
    atBigInt: Result := VarToStr(Value);
    atInt: Result := VarToStr(Value);
    atBool: Result := IntToStr(integer(Value = true));
    atVarchar: Result := '''' + VarToStr(Value) + '''';
    atFloat: Result := ReplaceStr(VarToStr(Value), ',', '.');
  else
  end;
end;

function TAttr.StrSetVal(s: string): boolean;
begin
  Result := false;
  Value := Null;
  try
    case ATransform of
      attVOkv:
        begin
          s := ReplaceStr(s, '[', '');
          s := ReplaceStr(s, ']', '');
          s := Trim(s);
        end;
      attVOkr:
        begin
          s := ReplaceStr(s, '{', '');
          s := ReplaceStr(s, '}', '');
          s := Trim(s);
        end;
      attVOarray:
        if Pos('Array()', s) > 0 then s := '';
      attVOarrayKV:
        begin
          if Pos('Array()', s) > 0 then s := '';

          s := ReplaceStr(s, '[', '');
          s := ReplaceStr(s, ']', '');
          s := Trim(s);
        end;
    end;

    case AType of
      atBigInt: Value := StrToInt64Def(s, 0);
      atInt: Value := StrToIntDef(s, 0);
      atBool: Value := (s = 'true');
      atVarchar: Value := s;
      atFloat: Value := StrToFloatDef(ReplaceStr(s, '.', ','), 0);
    else
    end;
    Result := not VarIsNull(Value);
  except
  end;
end;

{ TMWorld }

function TMWorld.AddRoom(RoomID: integer): TMRoom;
begin
  Result := GetRoom(RoomID);
  if Result = nil then
  begin
    Result := TMRoom.Create;
    Result.World := Self;
    Result.ID := RoomID;

    SetLength(FRooms, length(FRooms) + 1);
    FRooms[length(FRooms) - 1] := Result;
  end;
end;

function TMWorld.CheckRoomInformation(roominfo: string): boolean;
var
 obj,
 robj,
 onode: ISuperObject;
 roomn,
 i: integer;
 oname: string;
 room: TMRoom;
begin
  Result := false;
  try
    obj := TSuperObject.ParseString(PWideChar(roominfo), true);
    if obj = nil then exit;

    robj := obj.O['rooms'];
    if robj = nil then exit;

    for i := 0 to robj.AsObject.GetNames.AsArray.Length - 1 do
    begin
      oname := robj.AsObject.GetNames.AsArray[i].AsString;
      roomn := StrToIntDef(
        Copy(oname, Pos('_', oname) + 1, length(oname)) , -1);
      if (oname = '') or (roomn < 0) then continue;

      room := AddRoom(roomn);
      if room = nil then continue;

      onode := robj.O[oname];
      if onode = nil then continue;

      room.Avaliable := onode.B['available'];
    end;

    Result := true;
  except
  end;
end;

class constructor TMWorld.ClassCreate;
begin
  CInstance := nil;
end;

procedure TMWorld.Clear;
var
  i: Integer;
begin
  FSWFRevision := '';
  FSrvRevision := '';
  FValid := false;
  FUserDisabled := false;
  LastHeader.Clear;
  SetLength(Friends, 0);
  SetLength(Barn, 0);
  SetLength(RecvdGift, 0);
  SetLength(AvailGift, 0);

  for i := 0 to length(FRooms) - 1 do
  try
    FRooms[i].Free;
  except
  end;
  SetLength(FRooms, 0);
end;

constructor TMWorld.Create;
begin
  inherited;

  Clear;
end;

destructor TMWorld.Destroy;
begin
  Clear;

  inherited;
end;

class function TMWorld.GetInstance: TMWorld;
begin
  if CInstance = nil then CInstance := TMWorld.Create;

  Result := CInstance;
end;

function TMWorld.GetRoom(RoomID: integer): TMRoom;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to length(FRooms) - 1 do
    if FRooms[i].ID = RoomID then
    begin
      Result := FRooms[i];
      break;
    end;
end;

function TMWorld.GetRoomCount: integer;
begin
  Result := length(FRooms);
end;

function TMWorld.GetSrvRevision6: string;
begin
  Result := Copy(FSrvRevision, 1, 6);
end;

function TMWorld.GetSWFRevisionAppID: string;
begin
  Result := '';
  if length(FSWFRevision) < 19 then exit;
  Result := Copy(FSWFRevision, length(FSWFRevision) - 18, 19);
end;

function TMWorld.StrGiftStat: String;
var
 i: integer;
 avail,
 received: integer;
begin
  avail := 0;
  received := 0;
  for i := 0 to length(AvailGift) - 1 do
    avail := avail + AvailGift[i].qty;
  for i := 0 to length(RecvdGift) - 1 do
    received := received + RecvdGift[i].qty;

  if Self.valid then
    Result := 'aval=' + IntToStr(avail) +
      ' received=' + IntToStr(received)
  else
    Result := 'invalid data';
end;

{ TWorldHeader }

procedure TWorldHeader.Clear;
begin
  NextTick := 0;
  RoomInformation := '';
  WishListStr := '';
  Tax := 0;
  PopulationMultiplier := 1;
  population := 0;
  MaxPopulation := 0;
  Fuel := 0;
  Gold := 0;
  Coins := 0;
  ServerTime := 0;
  ResourceOptions := '';
  SessionKey := '';
  Auto := 0;
  Exp := 0;
  RoomID := -1;
  RespectLevel := 0;
  Level := 0;
  OwnerID := 0;
  Visitors := '';

  LastTick := 0;
end;

function TWorldHeader.GetFreePopulation: int64;
begin
  Result := GetMaxPopulation - GetPopulation;
end;

function TWorldHeader.GetMaxPopulation: int64;
begin
  Result := int64(Trunc(MaxPopulation * PopulationMultiplier));
end;

function TWorldHeader.GetPopulation: int64;
begin
  Result := int64(Population);
end;

{ TMRoom }

procedure TMRoom.AddField(field: TMField);
begin
  field.Room := Self;
  SetLength(FItems, length(FItems) + 1);
  FItems[length(FItems) - 1] := field;
end;

procedure TMRoom.Clear;
var
  i: integer;
begin
  FAvailable := false;
  Header.Clear;

  for i := 0 to length(FItems) - 1 do
  try
    FItems[i].Free;
  except
  end;
  SetLength(FItems, 0);
end;

constructor TMRoom.Create;
begin
  inherited;

  Clear;
end;

destructor TMRoom.Destroy;
begin
  Clear;

  inherited;
end;

procedure TMRoom.EndFieldsUpdate(serial: cardinal);
var
  i,
  j: Integer;
begin
  for i := 0 to length(FItems) - 1 do
    if FItems[i].Serial <> serial then
    begin
      FItems[i].Free;
      FItems[i] := nil;
    end
    else
      FItems[i].LastUpdate := Now;

  for i := length(FItems) - 1 downto 0 do
    if FItems[i] = nil then
    begin
      for j := i to length(FItems) - 2 do
        FItems[j] := FItems[j + 1];
      SetLength(FItems, length(FItems) - 1);
    end;
end;

procedure TMRoom.FieldsClearTick;
var
  i: Integer;
begin
  for i := 0 to length(FItems) - 1 do
    FItems[i].ClearTick;
end;

procedure TMRoom.FieldsExecute(MaxCount: integer; canTick, canWork: boolean);
var
  i: Integer;
  Qu: TActionQueue;
begin
  Qu := TActionQueue.GetInstance;
  for i := 0 to length(FItems) - 1 do
  begin
    if (MaxCount <> 0) and (MaxCount <= Qu.Count)
    then break;

    FItems[i].Execute(canTick, canWork);
  end;
end;

function TMRoom.StrFieldsStat: string;
begin
  Result := 'BUILD/STANDBY/WORK/ABAND/DIRTY/EXPIR ' +
    IntToStr(GetFieldsCountByType(1)) + '/' +
    IntToStr(GetFieldsCountByType(2)) + '/' +
    IntToStr(GetFieldsCountByType(3)) + '/' +
    IntToStr(GetFieldsCountByType(4)) + '/' +
    IntToStr(GetFieldsCountByType(5)) + '/' +
    IntToStr(GetFieldsCountByType(6));
end;

function TMRoom.GetField(AID: int64): TMField;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to length(FItems) - 1 do
    if FItems[i].ID = AID then
    begin
      Result := FItems[i];
      break;
    end;
end;

function TMRoom.GetFieldsCountByType(state: integer): integer;
var
 i: integer;
begin
  Result := 0;
  if not FAvailable then exit;

  for i := 0 to length(FItems) - 1 do
    if FItems[i].State = state then
      Result := Result + 1;
end;

function TMRoom.GetFieldsCount: integer;
begin
  Result := length(FItems);
end;

procedure TMRoom.SetWorld(const Value: TMWorld);
begin
  FWorld := Value;
end;

function TMRoom.StartFieldsUpdate: cardinal;
var
  i: Integer;
begin
  Result := GetTickCount;
  for i := 0 to length(FItems) - 1 do
    FItems[i].Serial := 0;
end;

{ TBarnRec }

procedure TBarnRec.Clear;
begin
  Name := '';
  ID := 0;
  Qty := 0;
  GameItem := nil;
end;

{ TGiftRec }

procedure TGiftRec.Clear;
begin
  Name := '';
  ID := 0;
  Qty := 0;
  FromUser := 0;

  GameItem := nil;
end;

function TGiftRec.GetGameItemID: cardinal;
begin
  Result := 0;
  if GameItem = nil then exit;
  Result := GameItem.ID;
end;

{ TMField }

procedure TMField.Clear;
begin
  FGameItem := nil;
  FContractOutputItem := nil;
  FSerial := 0;

  isDeny := false;
  ID := 0;
  Name := '';
  State := 0;
  x := 0;
  y := 0;
  Rotated := false;
  ContractInput := '';
  ContractOutput := '';
  InputFill := '';
  OutputFill := '';
  ProcessEnd := 0;
  BuildingPosition := 0;
  ExtraValue := 0;

  LastTickDT := 0;
  LastUpdate := 0;
end;

procedure TMField.ClearTick;
begin
  LastTickDT := 0;
end;

constructor TMField.Create;
begin
  inherited;

  Qu := TActionQueue.GetInstance;
  FRoom := nil;
  Clear;
end;

destructor TMField.Destroy;
begin

  inherited;
end;

procedure TMField.Execute(canTick, canWork: boolean);
begin

end;

function TMField.GetFieldType: TMGameItemType;
begin
  if GameItem = nil then
    Result := gitNone
  else
    Result := GameItem.GetItemType;
end;

function TMField.GetProcessEndDT: TDateTime;
begin
  if ProcessEnd <> 0 then
    Result :=
      LastUpdate +
      ProcessEnd / (SecsPerDay * 1000)  // ms in a day
  else
    Result := 0;
end;

procedure TMField.SetRoom(const Value: TMRoom);
begin
  FRoom := Value;
end;

{ TMFieldBuilding }

procedure TMFieldBuilding.ChangeState(NewState: integer);
begin
  if State = NewState then exit;

  if NewState = STATE_DIRTY then
  begin
    State := STATE_DIRTY;
  end;

  if NewState = STATE_ABANDONED then
  begin
    ProcessEnd := FGameItem.GetAttr('abandoned_length').AsInteger;
    LastUpdate := Now;

    State := STATE_ABANDONED;
  end;
end;

procedure TMFieldBuilding.Execute(canTick, canWork: boolean);
var
  elm: TActionQueueElm;
begin
  inherited;

  if isDeny then exit;

  if canTick then
  try
    if (State = STATE_ABANDONED) and
       (GetProcessEndDT < Now) and
       (GetProcessEndDT > Now - 10)
    then
    begin
      elm := Qu.Add(Room.ID, ID, faTick);
      elm.ActionDT := GetProcessEndDT;

      ChangeState(STATE_DIRTY);
    end;
  except
  end;

  if canWork then
  try
    if (State = STATE_DIRTY) and
       (GameItem.canClean)
    then
    begin
      Qu.Add(Room.ID, ID, faClean,
        GameItem.GetAttr('extra_exp').AsInteger);

      ChangeState(STATE_ABANDONED);
    end;
  except
  end;
end;

{ TMFieldHouse }

procedure TMFieldHouse.ChangeState(NewState: integer);
begin
  if State = NewState then exit;

  if NewState = STATE_STANDBY then
  begin
    State := STATE_STANDBY;
  end;

  if NewState = STATE_WORK then
  begin
    ProcessEnd := FGameItem.GetAttr('stage_length').AsInteger;
    LastUpdate := Now;

    State := STATE_WORK;
  end;
end;

procedure TMFieldHouse.Execute(canTick, canWork: boolean);
var
  elm: TActionQueueElm;
  aff: string;
  ippl: integer;
begin
  inherited;

  if isDeny then exit;

  if canTick then
  try
    if (State = STATE_WORK) and
       (GetProcessEndDT < Now) and
       (GetProcessEndDT > Now - 10)
    then
    begin
      elm := Qu.Add(Room.ID, ID, faTick);
      elm.ActionDT := GetProcessEndDT;

      ChangeState(STATE_STANDBY);
    end;
  except
  end;

  if canWork then
  try
    if (State = STATE_STANDBY) and
       (GameItem.canPick) and
       (Room.Header.GetFreePopulation > 2000)
    then
    begin
      elm := Qu.Add(Room.ID, ID, faPick,
        GameItem.GetAttr('extra_exp').AsInteger);
      aff := '';
      // TODO: calc affected
      ippl := GameItem.GetAttr('population_increase').AsInteger;
      if aff <> '' then
      begin
        elm.AddAttr('affect_items', aff);
        ippl := Trunc(ippl * 1.5 + 0.5);
      end;

      Room.Header.Population := cardinal(
        integer(Room.Header.Population) +
        ippl);

      ChangeState(STATE_WORK);
    end;
  except
  end;
end;

{ TMFieldFactory }

procedure TMFieldFactory.ChangeState(NewState: integer);
begin
  if State = NewState then exit;

  if NewState = STATE_ABANDONED then
  begin
    State := STATE_ABANDONED;
  end;

  if NewState = STATE_WORK then
  begin
    if PutGameItem <> nil then
      ContractOutput := IntToStr(PutGameItem.ID);

    State := STATE_WORK;
  end;

  if NewState = STATE_DIRTY then
  begin
    State := STATE_DIRTY;
  end;

  if NewState = STATE_STANDBY then
  begin
    ContractOutput := '';
    OutputFill := '';

    State := STATE_STANDBY;
  end;
end;

procedure TMFieldFactory.Clear;
begin
  inherited;
  PutKlass := '';
  PutGameItem := nil;
end;

procedure TMFieldFactory.Execute(canTick, canWork: boolean);
var
  elm: TActionQueueElm;
  xp: integer;
begin
  inherited;

  if isDeny then exit;

  if canTick then
  try
    if (State = STATE_WORK) and
       (GetProcessEndDT < Now) and
       (GetProcessEndDT > Now - 10)
    then
    begin
      elm := Qu.Add(Room.ID, ID, faTick);
      elm.ActionDT := GetProcessEndDT;

      ChangeState(STATE_ABANDONED);
    end;
  except
  end;

  if canWork then
  try
    if (State = STATE_ABANDONED) and
       ((ContractOutput <> '') or
        (OutputFill <> '')) and
       (GameItem.canPick)
    then
    begin   // moneyout+ xp+
      xp := 2;
      if FContractOutputItem <> nil then
        xp := FContractOutputItem.GetAttr('exp').AsInteger;

      Qu.Add(Room.ID, ID, faPick, xp);

      ChangeState(STATE_STANDBY);
    end;

    if (State = STATE_STANDBY) and
       (GameItem.canPut) and
       (PutKlass <> '')
    then
    begin // moneyin-
      elm := Qu.Add(Room.ID, ID, faPut, 0);
      elm.AddAttr('klass', PutKlass);

{???
    FClFields[length(FClFields) - 1].Affected :=
      CalcFactoryAffecting(st, st.fields[i]);
??? }
      ChangeState(STATE_WORK);
    end;

    // Expired
    if (State = STATE_EXPIRED) and
       (GameItem.canPick)
    then
    begin // moneyin/2 +
      Qu.Add(Room.ID, ID, faPick, 0);

      ChangeState(STATE_DIRTY);
    end;

    if (State = STATE_DIRTY) and
       (GameItem.canClean)
    then
    begin // money- xp+
      Qu.Add(Room.ID, ID, faClean, 2);

      ChangeState(STATE_STANDBY);
    end;
  except
  end;
end;

end.
