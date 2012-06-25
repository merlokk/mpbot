unit uDefs;

interface
uses
  SysUtils, DateUtils, Variants, Classes;

type
  TFieldAction = (faNone, faClean, faPick, faPut, faTick,
    faRemoveWishList, faAddWishList, faHelp, faSendGift, faMassSendGift,
    faLast);

const
  STATE_BUILD = 1;
  STATE_STANDBY = 2;
  STATE_WORK = 3;
  STATE_ABANDONED = 4;
  STATE_DIRTY = 5;
  STATE_EXPIRED = 6;
  STATE_CMDCOMPLETED = 1000;

  FN_CLEAN = 1;
  FN_PICK = 2;
  FN_PUT = 3;
  FN_TICK = 4;
  FN_REMOVEWL = 5;
  FN_ADDWL = 6;
  FN_HELP = 7;
  FN_STR: array [1..7] of string =
    ('clean', 'pick', 'put', 'tick',
     'remove_from_wish_list', 'add_to_wish_list', 'help');

  FA_STR: array [faNone..faLast] of string =
    ('', 'clean', 'pick', 'put', 'tick',
     'remove_from_wish_list', 'add_to_wish_list', 'help',
     'send_gift', 'mass_send_gift',
     '');
  FA_STR_STAT: array [faNone..faLast] of string =
    ('', 'cln', 'pick', 'put', 'tick',
     'remwl', 'addwl', 'hlp', 'gift', 'mgift',
     '');

  GUARD_TIME = 60 / SecsPerDay; // 60 sec

type
  TNameValRec = record
    Name: string;
    Value: integer;
  end;
  TNameValArr = array of TNameValRec;

  TNameValSRec = record
    Name: string;
    Value: string;
  end;
  TNameValSArr = array of TNameValSRec;

  TFieldGraf = array [0..12] of integer;

  WantListRec = packed record
    GiftID: cardinal;
    Score: extended;
  end;

  ExecContractRec = packed record
    name,
    klass,
    affected_items: string;
  end;

  TSendGiftRec = packed record
    ID: int64;
    GameItemID: integer;
    UserID: int64;
    procedure Clear;
  end;

  TFriendRec = packed record
    ID: int64;
    RusName: String;
    Level: integer;
    WishList,
    HelpItems,
    RoomInfo: String;
    RewardPoints: Extended;
    HaveGift: boolean;
    HelpPointsCnt: integer;
    NextVisitDT: TDateTime;

    procedure Clear;
  end;
  TFriendRecArray = array of TFriendRec;

  VKFriendRec = packed record
    id: int64;
    FirstName,
    LastName: String;
    Sex: integer;

    procedure Clear;
  end;

function GetRandomStr:String;
function GetBaseRandom(base:Int64; deviation: cardinal):Int64;
function GetBaseRandomStr(base:Int64; deviation: cardinal):String;
procedure AddCmdLine(var res: string; cnt: integer; cmd: string; data:string); overload;
procedure AddCmdLine(var res: string; cnt: integer; cmd: string; data:int64); overload;
function GetFloatFormat: TFormatSettings;

// variants
function VarToIntDef(v: variant; default: integer): integer;
function VarToInt64Def(v: variant; default: int64): int64;
function VarToBoolDef(v: variant; default: boolean): boolean;

implementation

var
  FFloatFormat: TFormatSettings;

function GetRandomStr:String;
begin
  Result := FormatFloat('0.0000000000000000', Random, GetFloatFormat);
end;

function GetBaseRandom(base:Int64; deviation: cardinal):Int64;
begin
  Result := base - deviation div 2 + Random(deviation);
end;

function GetBaseRandomStr(base:Int64; deviation: cardinal):String;
begin
  Result := IntToStr(GetBaseRandom(base, deviation));
end;

procedure AddCmdLine(var res: string; cnt: integer; cmd: string; data:string); overload;
begin
  res := res + 'cached[' + IntToStr(cnt) +
         '][' + cmd + ']:' + data + #$0D#$0A;
end;

procedure AddCmdLine(var res: string; cnt: integer; cmd: string; data:int64); overload;
begin
  AddCmdLine(res, cnt, cmd, IntToStr(data));
end;

function GetFloatFormat: TFormatSettings;
begin
  Result := FFloatFormat;
end;

{ VKFriendRec }

procedure VKFriendRec.Clear;
begin
  id := 0;
  FirstName := '';
  LastName := '';
  Sex := 0;
end;

// variants
function VarToIntDef(v: variant; default: integer): integer;
begin
  Result := StrToIntDef(VarToStr(v), default);
end;

function VarToInt64Def(v: variant; default: int64): int64;
begin
  Result := StrToInt64Def(VarToStr(v), default);
end;

function VarToBoolDef(v: variant; default: boolean): boolean;
begin
  Result := default;
  if LowerCase(VarToStr(v)) = 'true' then Result := true;
  if LowerCase(VarToStr(v)) = 'false' then Result := false;
  if VarToStr(v) = '1' then Result := true;
  if VarToStr(v) = '0' then Result := false;
end;

{ FriendRec }

procedure TFriendRec.Clear;
begin
  ID := 0;
  RusName := '';
  Level := 0;
  WishList := '';
  HelpItems := '';
  RoomInfo := '';
  RewardPoints := 0;
  HaveGift := true;
  HelpPointsCnt := 0;
  NextVisitDT := Now + 10;
end;

{ TSendGiftRec }

procedure TSendGiftRec.Clear;
begin
  ID := 0;
  GameItemID := 0;
  UserID := 0;
end;

initialization
  Randomize;
  FFloatFormat := TFormatSettings.Create;
  FFloatFormat.DecimalSeparator := '.';
end.
