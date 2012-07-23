unit uGameItems;

interface
uses
  Windows, SysUtils, Variants, Classes, StrUtils, superobject,
  DateUtils,
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

  PPL_GUARD_SPACE = 2000;   // population guard space
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
    function GetMaterialQty: string;
    function GetHeight: integer;
    function GetRadius: integer;
    function GetWidth: integer;
    function GetcanHelp: boolean;
    function GetAllStates: string;
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

    function GetAllStatesParam(Operation, ItemName: string; ParamName: string = 'min_quantity'): string;
    function GetAllStatesParamInt(Operation, ItemName: string; ParamName: string = 'min_quantity'): integer;

    property canPick: boolean read GetcanPick;
    property canClean: boolean read  GetcanClean;
    property canPut: boolean read  GetcanPut;
    property canHelp: boolean read  GetcanHelp;
    property isBuildSite: boolean read GetisBuildSite;
    property ItemType: TMGameItemType read GetItemType;
    property isFactory: boolean read GetIsFactory;
    property isBuilding: boolean read GetIsBuilding;
    property isHouse: boolean read GetIsHouse;
    property MaterialQty: string read GetMaterialQty;
    property Height: integer read GetHeight;
    property Width: integer read GetWidth;
    property Radius: integer read GetRadius;
    property AllStates: string read GetAllStates;
  end;

  TGiftRec = packed record
  private
    function GetGameItemID: cardinal;
  public
    Name: String;
    ID: Cardinal;
    Qty: integer;
    FromUser: cardinal;
    Score: Extended;

    GameItem: TMGameItem;

    property GameItemID: cardinal read GetGameItemID;
    procedure Clear;
  end;
  TGiftRecs = array of TGiftRec;

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
    function GetActionDT(canTick, canWork: boolean): TDateTime; virtual;
    function inAreaOf(it: TMField): boolean;

    property Serial: cardinal read FSerial write FSerial;
    property GameItem: TMGameItem read FGameItem write FGameItem;
    property ContractOutputItem: TMGameItem read FContractOutputItem write FContractOutputItem;

    procedure ClearTick;
    procedure Execute(canTick, canWork: boolean); virtual;
    function CanHelp: boolean;
    procedure CalcHelp(FriendID: int64); virtual;

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

    function GetActionDT(canTick, canWork: boolean): TDateTime; override;
    procedure ChangeState(NewState: integer);
    procedure Execute(canTick, canWork: boolean); override;
    procedure CalcHelp(FriendID: int64); override;
  end;

  TMFieldBuilding = class (TMField)
  public
    function GetActionDT(canTick, canWork: boolean): TDateTime; override;
    procedure ChangeState(NewState: integer);
    procedure Execute(canTick, canWork: boolean); override;
  end;

  TMFieldHouse = class (TMField)
  private
    function CalcHouseAffected: string;
  public
    function GetActionDT(canTick, canWork: boolean): TDateTime; override;
    procedure ChangeState(NewState: integer);
    procedure Execute(canTick, canWork: boolean); override;
  end;

  TMFieldFactoryWithHelp = class (TMFieldFactory)
  private
    FLastSendHelp: TDateTime;
    FLastFriendGroup: integer;

    function GetSlotsCount(JSONPath: string = 'slots.count'): integer;
  public
    ExecContract: TExecContractRec;

    constructor Create; override;
    procedure Clear; override;
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
    RoomsResources: string;
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
    RoomResourcesArray: TNameValSArr;

    HelpPoints,
    NotPlayDays: integer;
    OwnerRoomInformation: string;

    LastTick: cardinal;

    procedure Clear;
    function GetMaxPopulation: int64;
    function GetPopulation: int64;
    function GetFreePopulation: int64;
    function GetRoomResource(name: string): string;
    function GetVisitorsCount(FieldID: int64): integer;
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
    function GetField(AID: int64): TMField; overload;
    function GetField(LeftPartOfName: string): TMField; overload;
    function GetFieldI(indx: integer): TMField;
    function GetFieldIndxByPos(x, y: integer): integer;
    procedure AddField(field: TMField);
    procedure EndFieldsUpdate(serial: cardinal);
    function GetFieldsCountByType(state: integer): integer;
    function StrFieldsStat: string;

    procedure FieldsClearTick;
    procedure FieldsExecute(MaxCount: integer; canTick, canWork: boolean); virtual;
    function FieldsExecuteCount(canTick, canWork: boolean; FromDate, ToDate: TDateTime): integer;
    function FieldGrafFill(var gr: TFieldGraf): boolean;
    procedure FieldsHelp(MaxCount: integer; UserID: int64); virtual;

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
    function GetOwnerID: int64;
  public
    LastUpdate,
    LastRoomChange: TDateTime;
    LastHeader: TWorldHeader;

    Friends: TFriendRecArray;
    Barn: array of TBarnRec;
    RecvdGift: TGiftRecs;
    AvailGift: TGiftRecs;

    class function GetInstance: TMWorld;
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function CheckRoomInformation(roominfo: string): boolean;
    function GetRoom(RoomID: integer): TMRoom;
    function AddRoom(RoomID: integer): TMRoom;
    function GetRoomCount: integer;
    function GetRoomResource(RoomID: integer; ResName: string): string;

    function MakeGift(gift: TSendGiftRec): boolean;
    function CanGift(uid: int64): boolean;
    function GetNextGift(var gift: TSendGiftRec): boolean;
    function GetAvailGiftCount(GameItemID: cardinal): integer;
    function GetBarnCount(GameItemID: cardinal): integer;

    function StrGiftStat: String;
    function StrXPStat: string;

    property SWFRevision: string read FSWFRevision write FSWFRevision;
    property SWFRevisionAppID: string read GetSWFRevisionAppID;
    property SrvRevision: string read FSrvRevision write FSrvRevision;
    property SrvRevision6: string read GetSrvRevision6;
    property Valid: boolean read FValid write FValid;
    property UserDisabled: boolean read FUserDisabled write FUserDisabled;
    property OwnerID: int64 read GetOwnerID;
  end;

implementation
uses uDB, uFactories, uTactic;

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

function TMGameItem.GetAllStates: string;
begin
  Result := GetAttr('all_states').AsString;
end;

function TMGameItem.GetAllStatesParam(Operation, ItemName,
  ParamName: string): string;
var
  i: integer;
  vAllStates: string;
  itemObj,
  itemsObj,
  operObj,
  dropObj,
  obj: ISuperObject;
begin
  Result := '';
  try
    vAllStates := AllStates;
    obj := TSuperObject.ParseString(PWideChar(vAllStates), false);
    if obj = nil then exit;

    dropObj := nil;
    for i := 0 to obj.AsArray.Length - 1 do
    begin
      dropObj := obj.AsArray.O[i].AsObject['drop'];
      if dropObj <> nil then break;
    end;
    if (dropObj = nil) or (dropObj.AsObject = nil) then exit;
    operObj := dropObj.O[Operation];
    if (operObj = nil) or (operObj.AsArray = nil) then exit;

    itemsObj := nil;
    for i := 0 to operObj.AsArray.Length - 1 do
    begin
      itemsObj := operObj.AsArray.O[i].AsObject['items'];
      if itemsObj <> nil then break;
    end;
    if (itemsObj = nil) or (itemsObj.AsArray = nil) then exit;

    itemObj := nil;
    for i := 0 to itemsObj.AsArray.Length - 1 do
    begin
      itemObj := itemsObj.AsArray.O[i].AsObject[ItemName];
      if itemObj <> nil then break;
    end;
    if (itemObj = nil) or (itemObj.AsObject = nil) then exit;

    Result := itemObj.O[ParamName].AsString;
  except
  end;
end;

function TMGameItem.GetAllStatesParamInt(Operation, ItemName,
  ParamName: string): integer;
begin
  Result := StrToIntDef(GetAllStatesParam(Operation, ItemName, ParamName), 0);
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

function TMGameItem.GetcanHelp: boolean;
begin
  Result := GetAttr('help').AsBoolean;
end;

function TMGameItem.GetcanPick: boolean;
begin
  Result := GetAttr('pick').AsBoolean;
end;

function TMGameItem.GetcanPut: boolean;
begin
  Result := GetAttr('put').AsBoolean;
end;

function TMGameItem.GetHeight: integer;
begin
  Result := GetAttr('height').AsInteger;
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

function TMGameItem.GetMaterialQty: string;
begin
  Result := GetAttr('materials_quantity_obj').AsString;
end;

function TMGameItem.GetRadius: integer;
begin
  Result := GetAttr('radius').AsInteger;
end;

function TMGameItem.GetWidth: integer;
begin
  Result := GetAttr('width').AsInteger;
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

function TMWorld.CanGift(uid: int64): boolean;
var
 i: integer;
begin
  Result := false;
  if not valid then exit;
  if uid = OwnerID then exit;

  for i := 0 to length(Friends) - 1 do
    if Friends[i].ID = uid then
    begin
      Result := not Friends[i].HaveGift;
      break;
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
  LastUpdate := 0;
  LastRoomChange := 0;
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

function TMWorld.GetAvailGiftCount(GameItemID: cardinal): integer;
var
 i: integer;
begin
  Result := 0;
  if not valid then exit;

  for i := 0 to length(RecvdGift) - 1 do
    if (RecvdGift[i].GetGameItemID = GameItemID) then
      Result := Result + RecvdGift[i].Qty;
end;

function TMWorld.GetBarnCount(GameItemID: cardinal): integer;
var
 i: integer;
begin
  Result := 0;
  if not valid then exit;

  for i := 0 to length(barn) - 1 do
    if (Barn[i].id = GameItemID) then
      Result := Result + Barn[i].Qty;
end;

function TMWorld.GetNextGift(var gift: TSendGiftRec): boolean;
var
 i: integer;
 q: extended;
begin
  Result := false;
  if not valid then exit;

  q := 1000;
  gift.GameItemID := 0;
  gift.ID := 0;

  for i := 0 to length(AvailGift) - 1 do
    if (AvailGift[i].qty > 0) and
       (AvailGift[i].Score < q) then
    begin
      gift.GameItemID := AvailGift[i].GameItemID;
      gift.ID := AvailGift[i].ID;
      q := AvailGift[i].Score;
    end;
end;

class function TMWorld.GetInstance: TMWorld;
begin
  if CInstance = nil then CInstance := TMWorld.Create;

  Result := CInstance;
end;

function TMWorld.GetOwnerID: int64;
var
  room: TMRoom;
begin
  Result := 0;
  room := GetRoom(0);
  if room <> nil then
    Result := room.Header.OwnerID;
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

function TMWorld.GetRoomResource(RoomID: integer; ResName: string): string;
var
  room: TMRoom;
begin
  Result := '';
  room := GetRoom(RoomID);
  if room = nil then exit;

  Result := room.Header.GetRoomResource(ResName);
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

function TMWorld.MakeGift(gift: TSendGiftRec): boolean;
var
 i,
 j,
 frid: integer;
begin
  Result := false;
  if not valid then exit;

  frid := -1;
  for i := 0 to length(friends) - 1 do
    if friends[i].id = gift.UserID then
    begin
      if Friends[i].HaveGift then exit;
      frid := i;
      break;
    end;

  if frid < 0 then exit;

  for j := 0 to length(AvailGift) - 1 do
    if (AvailGift[j].ID = gift.ID) and
       (AvailGift[j].Qty > 0) then
    begin
      AvailGift[j].Qty := AvailGift[j].Qty - 1;
      Friends[frid].HaveGift := true;

      Result := true;
      break;
    end;
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

function TMWorld.StrXPStat: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to length(FRooms) - 1 do
    Result := Result + IntToStr(FRooms[i].Header.Exp) + ', ';

  Result := Copy(Result, 1, length(Result) - 2);
end;

{ TWorldHeader }

procedure TWorldHeader.Clear;
begin
  NextTick := 0;
  RoomInformation := '';
  WishListStr := '';
  RoomsResources := '';
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
  SetLength(RoomResourcesArray, 0);

  HelpPoints := 0;
  NotPlayDays := 100;
  OwnerRoomInformation := '';

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

function TWorldHeader.GetRoomResource(name: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to length(RoomResourcesArray) - 1 do
    if RoomResourcesArray[i].Name = name then
    begin
      Result := RoomResourcesArray[i].Value;
      break;
    end;
end;

function TWorldHeader.GetVisitorsCount(FieldID: int64): integer;
var
 obj: ISuperObject;
begin
  Result := 0;
  try
    obj := TSuperObject.ParseString(PWideChar(visitors), false);
    if (obj <> nil) and (obj.A[IntToStr(FieldID)] <> nil) then
      Result := obj.A[IntToStr(FieldID)].Length;
  except
    Result := 0;
  end;
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

function TMRoom.FieldGrafFill(var gr: TFieldGraf): boolean;
var
  i: Integer;
  cdt: TDateTime;
begin
  Result := false;
  try
    for i := 0 to 12 do gr[i] := 0;

    cdt := Now;
    gr[0] := FieldsExecuteCount(true, true, 0, cdt);
    for i := 1 to 12 do
      gr[i] := FieldsExecuteCount(true, true, cdt + 5 * OneMinute * (i-1), cdt + 5 * OneMinute * i);

    Result := true;
  except
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

function TMRoom.FieldsExecuteCount(canTick, canWork: boolean; FromDate,
  ToDate: TDateTime): integer;
var
  i: Integer;
  dt: TDateTime;
begin
  Result := 0;
  for i := 0 to length(FItems) - 1 do
  begin
    dt := FItems[i].GetActionDT(canTick, canWork);
    // No action needed
    if dt < Now - 10 then continue;
    // action in open intervals. each of dates may eq 0.
    if (dt > FromDate) and
       ((ToDate < Now - 10) or (dt < ToDate))
    then
      Result := Result + 1;
  end;
end;

procedure TMRoom.FieldsHelp(MaxCount: integer; UserID: int64);
var
  i: Integer;
  Qu: TActionQueue;
begin
  Qu := TActionQueue.GetInstance;
  for i := 0 to length(FItems) - 1 do
  begin
    if (MaxCount <> 0) and (MaxCount <= Qu.Count)
    then break;

    FItems[i].CalcHelp(UserID);
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

function TMRoom.GetField(LeftPartOfName: string): TMField;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to length(FItems) - 1 do
    if Pos(LeftPartOfName, FItems[i].Name) = 1 then
    begin
      Result := FItems[i];
      break;
    end;
end;

function TMRoom.GetFieldI(indx: integer): TMField;
begin
  Result := nil;
  if (indx < 0) or (indx >= length(FItems)) then exit;

  Result := FItems[indx];
end;

function TMRoom.GetFieldIndxByPos(x, y: integer): integer;
var
 i: integer;
begin
  Result := -1;

  for i := 0 to length(FItems) - 1 do
  if (FItems[i].x = x) and
     (FItems[i].y = y) then
  begin
    Result := i;
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
  Score := 0;

  GameItem := nil;
end;

function TGiftRec.GetGameItemID: cardinal;
begin
  Result := 0;
  if GameItem = nil then exit;
  Result := GameItem.ID;
end;

{ TMField }

procedure TMField.CalcHelp(FriendID: int64);
var
  elm: TActionQueueElm;
begin
  if not CanHelp then exit;
  try
    // build help
    if (GameItem <> nil) and
       (GameItem.canHelp) and
       (State = STATE_BUILD) then
    begin
      elm := Qu.Add(Room.ID, ID, Name, faHelp, 3);
      elm.AddAttr('klass', Name);
      elm.AddAttr('friend_id', FriendID);
    end;

    // buildable_help (rails, central port, ...etc)
    if (GameItem <> nil) and
       (GameItem.isBuildSite) and
       (GameItem.GetAttr('buildable_help').AsBoolean) and
       (State = STATE_STANDBY) then
    begin
      elm := Qu.Add(Room.ID, ID, Name, faHelp, 3);
      elm.AddAttr('klass', Name);
      elm.AddAttr('friend_id', FriendID);
    end;
  except
  end;
end;

function TMField.CanHelp: boolean;
begin
  Result := true;

  //  picnic objects disabled
  if pos('picnic', Name) > 0 then
  begin
    Result := false;
    exit;
  end;

  //  bigbon objects disabled
  if pos('bigbon', Name) > 0 then
  begin
    Result := false;
    exit;
  end;

  //  double rails after bridge disabled
  if (x = 24) and (y < 49) and
     (FRoom.GetFieldIndxByPos(24, 55) < 0)
  then
  begin
    Result := false;
    exit;
  end;
end;

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

function TMField.GetActionDT(canTick, canWork: boolean): TDateTime;
begin
  Result := 0;
  // 0: no action
  // now-1: work action
  // else: tick action
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

function FieldInArea(itMain, itSlave: TMField): boolean;
var
  hMain,
  wMain,
  hSlave,
  wSlave,
  rMain: integer;
begin
  Result := false;
  if (itMain.GameItem = nil) or (itSlave.GameItem = nil) then exit;

  if not itMain.Rotated then
  begin
    hMain := itMain.GameItem.Height;
    wMain := itMain.GameItem.Width;
  end
  else
  begin
    hMain := itMain.GameItem.Width;
    wMain := itMain.GameItem.Height;
  end;

  if not itSlave.Rotated then
  begin
    hSlave := itSlave.GameItem.Height;
    wSlave := itSlave.GameItem.Width;
  end
  else
  begin
    hSlave := itSlave.GameItem.Width;
    wSlave := itSlave.GameItem.Height;
  end;

  rMain := itMain.GameItem.Radius;

  Result :=
    (itSlave.x >= itMain.x - rMain) and
    (itSlave.x + (wSlave - 1) <= itMain.x + rMain + (wMain - 1)) and
    (itSlave.y >= itMain.y - rMain) and
    (itSlave.y + (hSlave - 1) <= itMain.y + rMain + (hMain - 1));
end;

function TMField.inAreaOf(it: TMField): boolean;
begin
  Result := FieldInArea(it, Self);
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
      elm := Qu.Add(Room.ID, ID, Name, faTick);
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
      Qu.Add(Room.ID, ID, Name, faClean,
        GameItem.GetAttr('extra_exp').AsInteger);

      ChangeState(STATE_ABANDONED);
    end;
  except
  end;
end;

function TMFieldBuilding.GetActionDT(canTick, canWork: boolean): TDateTime;
begin
  Result := inherited;

  if canTick and
     (State = STATE_ABANDONED)
  then
    Result := GetProcessEndDT;

  if canWork and
     (State = STATE_DIRTY)
  then
    Result := Now - 1;
end;

{ TMFieldHouse }

function TMFieldHouse.CalcHouseAffected: string;
var
 i: integer;
begin
  Result := '';

  if not GameItem.GetAttr('buff_population').AsBoolean then exit;

  for i := 0 to length(FRoom.FItems) - 1 do
    if (FRoom.FItems[i].GameItem <> nil) and
       (FRoom.FItems[i].GameItem.GetAttr('actions').AsInteger > 0) and
       (FRoom.FItems[i].ID <> ID) and
       (FRoom.FItems[i].GameItem.SuperClass = 'excavation_buff_base') and
       (StrToIntDef(FRoom.FItems[i].InputFill, 0) > 0) and
       (inAreaOf(FRoom.FItems[i]))
    then
    begin
      Result := IntToStr(FRoom.FItems[i].ID);
      FRoom.FItems[i].InputFill := IntToStr(
        StrToIntDef(FRoom.FItems[i].InputFill, 0) - 1);

      break;
    end;
end;

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
      elm := Qu.Add(Room.ID, ID, Name, faTick);
      elm.ActionDT := GetProcessEndDT;

      ChangeState(STATE_STANDBY);
    end;
  except
  end;

  if canWork then
  try
    if (State = STATE_STANDBY) and
       (GameItem.canPick) and
       (Room.Header.GetFreePopulation > PPL_GUARD_SPACE)
    then
    begin
      elm := Qu.Add(Room.ID, ID, Name, faPick,
        GameItem.GetAttr('extra_exp').AsInteger);

      //  house affected from excavation
      aff := CalcHouseAffected;

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

function TMFieldHouse.GetActionDT(canTick, canWork: boolean): TDateTime;
begin
  Result := inherited;

  if FRoom.Header.GetFreePopulation <
     PPL_GUARD_SPACE + GameItem.GetAttr('population_increase').AsInteger
  then exit;

  if canTick and
     (State = STATE_WORK)
  then
    Result := GetProcessEndDT;

  if canWork and
     (State = STATE_STANDBY)
  then
    Result := Now - 1;
end;

{ TMFieldFactory }

{  // help build
  for i := 0 to length(fields) - 1 do
  if (fields[i].State = STATE_BUILD) and
     (CanHelp(fields[i])) then
  begin
    SetLength(FClFields, length(FClFields) + 1);
    FClFields[length(FClFields) - 1] := fields[i];
    FClFields[length(FClFields) - 1].Cleaned := false;
    FClFields[length(FClFields) - 1].Func := FN_HELP;
    FClFields[length(FClFields) - 1].PutKlass :=
      fields[i].Name;

    fields[i].State := STATE_CMDCOMPLETED;
  end;

  // buildable_help (rails, central port, ...etc)
  for i := 0 to length(fields) - 1 do
  if (fields[i].State = STATE_STANDBY) and
     (fields[i].item.isBuildSite) and
     (fields[i].item.BuildableHelp) and
     (CanHelp(fields[i]))
  then
  begin
    SetLength(FClFields, length(FClFields) + 1);
    FClFields[length(FClFields) - 1] := fields[i];
    FClFields[length(FClFields) - 1].Cleaned := false;
    FClFields[length(FClFields) - 1].Func := FN_HELP;
    FClFields[length(FClFields) - 1].PutKlass :=
      fields[i].Name;

    fields[i].State := STATE_CMDCOMPLETED;
  end;

  // help contract
  for i := 0 to length(fields) - 1 do
  if (fields[i].State = STATE_WORK) and
     (fields[i].item.canHelp) and
     (fields[i].item.canPick) and
     (fields[i].item.canPut) and
     (fields[i].item.isFactory) and
     (CanHelp(fields[i]))
  then
  begin
    SetLength(FClFields, length(FClFields) + 1);
    FClFields[length(FClFields) - 1] := fields[i];
    FClFields[length(FClFields) - 1].Cleaned := false;
    FClFields[length(FClFields) - 1].Func := FN_HELP;
    FClFields[length(FClFields) - 1].PutKlass :=
      fields[i].Name;

    fields[i].State := STATE_CMDCOMPLETED;
  end;

  // help expired
  for i := 0 to length(fields) - 1 do
  if (fields[i].State = STATE_EXPIRED) and
     (fields[i].item.canHelp) and
     (CanHelp(fields[i]))
  then
  begin
    SetLength(FClFields, length(FClFields) + 1);
    FClFields[length(FClFields) - 1] := fields[i];
    FClFields[length(FClFields) - 1].Cleaned := false;
    FClFields[length(FClFields) - 1].Func := FN_HELP;
    FClFields[length(FClFields) - 1].PutKlass :=
      fields[i].Name;

    fields[i].State := STATE_CMDCOMPLETED;
  end;
}

procedure TMFieldFactory.CalcHelp(FriendID: int64);
var
  elm: TActionQueueElm;
begin
  inherited;
  if not CanHelp then exit;
  try
    // help contract
    if (GameItem <> nil) and
       (GameItem.canHelp) and
       (State = STATE_WORK) then
    begin
      elm := Qu.Add(Room.ID, ID, Name, faHelp, 3);
      elm.AddAttr('klass', Name);
      elm.AddAttr('friend_id', FriendID);
    end;

    // help expired contract
    if (GameItem <> nil) and
       (GameItem.canHelp) and
       (State = STATE_EXPIRED) then
    begin
      elm := Qu.Add(Room.ID, ID, Name, faHelp, 3);
      elm.AddAttr('klass', Name);
      elm.AddAttr('friend_id', FriendID);
    end;
  except
  end;
end;

procedure TMFieldFactory.ChangeState(NewState: integer);
begin
  if State = NewState then exit;

  if NewState = STATE_ABANDONED then
  begin
    ContractOutput := ContractInput;
    ContractInput := '';
    OutputFill := InputFill;
    InputFill := '';

    State := STATE_ABANDONED;
  end;

  if NewState = STATE_WORK then
  begin
    if PutGameItem <> nil then
    begin
      ContractOutput := IntToStr(PutGameItem.ID);
      ProcessEnd := PutGameItem.GetAttr('stage_length').AsInteger;
      if ProcessEnd = 0 then
        ProcessEnd := 1 * 60 * 60 * 1000;
    end
    else
      ProcessEnd := 1 * 60 * 60 * 1000;  // 1 hour - default
    LastUpdate := Now;

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
      elm := Qu.Add(Room.ID, ID, Name, faTick);
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

      Qu.Add(Room.ID, ID, Name, faPick, xp);

      ChangeState(STATE_STANDBY);
    end;

    if (State = STATE_STANDBY) and
       (GameItem.canPut) and
       (PutKlass <> '')
    then
    begin // moneyin-
      elm := Qu.Add(Room.ID, ID, Name, faPut, 0);
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
      Qu.Add(Room.ID, ID, Name, faPick, 0);

      ChangeState(STATE_DIRTY);
    end;

    if (State = STATE_DIRTY) and
       (GameItem.canClean)
    then
    begin // money- xp+
      Qu.Add(Room.ID, ID, Name, faClean, 2);

      ChangeState(STATE_STANDBY);
    end;
  except
  end;
end;

function TMFieldFactory.GetActionDT(canTick, canWork: boolean): TDateTime;
begin
  Result := inherited;

  if canTick and
     (State = STATE_WORK)
  then
    Result := GetProcessEndDT;

  if canWork and
     (State = STATE_ABANDONED) and
     ((ContractOutput <> '') or
      (OutputFill <> '')
     ) or
     ((State = STATE_STANDBY) and
       (PutKlass <> '')
     ) or
     (State = STATE_DIRTY) or
     (State = STATE_EXPIRED)
  then
    Result := Now - 1;
end;

{ TMFieldFactoryCustom }

procedure TMFieldFactoryWithHelp.Clear;
begin
  inherited;

  ExecContract.Clear;
end;

constructor TMFieldFactoryWithHelp.Create;
begin
  inherited;

  FLastSendHelp := 0;
  FLastFriendGroup := 0;
end;

function TMFieldFactoryWithHelp.GetSlotsCount(JSONPath: string): integer;
var
  obj: ISuperObject;
  item: TMGameItem;
begin
  Result := 0;

  try
    // some scripting )
    JSONPath := ReplaceStr(JSONPath, '<name>', Name);

    //  get contract help slots count
    item := TItemsFactory.GetInstance.GetGameItem(StrToIntDef(ContractInput, 0));
    if item = nil then exit;

    obj := TSuperObject.ParseString(PWideChar(item.GetAttr('extra_params').AsString), false);
    if obj = nil then exit;

    if obj.S['type'] = 'slots' then
      Result := obj.I[JSONPath];
  except
  end;
end;

procedure TMFieldFactoryWithHelp.Execute(canTick, canWork: boolean);
var
  cnt: integer;
  elm: TActionQueueElm;
begin
  inherited;

  if canWork then
  begin
    // clear sendhelp state if not working
    if (State = STATE_STANDBY) then
    begin
      FLastSendHelp := 0;
    end;

    //  send message
    if (State = STATE_WORK) and
       (ExecContract.HelpName <> '') and
       (not isDeny) and
       (FLastSendHelp + 60 * OneMinute < Now)  //  send per 60 min
    then
    begin
      cnt := GetSlotsCount(ExecContract.HelpSlotsLink);
      if (cnt > 0) and (cnt > FRoom.Header.GetVisitorsCount(ID)) then
      begin
        elm := Qu.Add(Room.ID, ID, Name, faSendRequest, 0);
        elm.AddAttr('name', ExecContract.HelpName);
        elm.AddAttr('msg', ExecContract.HelpMsg);
        elm.AddAttr('notify', TMPdatabase.GetInstance.GetGroupedAppFriends(FLastFriendGroup, 100));
        FLastSendHelp := now;
      end;
    end;
  end;
end;

end.
