unit uDB;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Forms, math, Types,
  FIBDatabase, pFIBDatabase, FIBQuery, pFIBQuery, pFIBProps, uDefs,
  StrUtils, Generics.Collections, Generics.Defaults,
  uGameItems, uLogger;

type
  TMPdatabase = class
  protected
    class var FInstance: TMPdatabase;
    class constructor ClassCreate;
  private
    FConnected: boolean;
    DBFloatFormat: TFormatSettings;

    FIBDatabase: TpFIBDatabase;
    FIBQuery: TpFIBQuery;
    FIBQueryCurs: TpFIBQuery;
    FIBTransaction: TpFIBTransaction;

    FWantListLoaded: boolean;
//    FRewardList: array of WantListRec;
    FRewardLoaded: boolean;

    FExecContractList: array of TExecContractRec;
    FExecContractListLoaded: TDateTime;

    FGameFriend: array of integer;
    FFieldDenyLoaded: TDateTime;
    FFieldDeny: array of string;

    function UpdateExecContractList: boolean;
//    function FillRewardList: boolean;
    function UpdateFieldDenyList: boolean;
  public
    class function GetInstance: TMPdatabase; static;

    constructor Create;
    destructor Destory;

    property Connected: boolean read FConnected;
    function Connect: boolean;
    procedure Commit;

    function SetParam(ParamName: string; ParamValue: Integer): boolean; overload;
    function SetParam(ParamName, ParamValue: string): boolean; overload;
    function SetParam(ParamGroup: integer; ParamName: string; ParamValue: integer): boolean; overload;
    function SetParam(ParamGroup: integer; ParamName: string; ParamValue: string): boolean; overload;

    function GetParam(ParamGroup: integer; ParamName: string; Default: string = ''): string;
    function GetParamGroups: TNameValArr;

    function GetRewardPoints(userid: int64): Extended;
    procedure SubtractReward(gift: TSendGiftRec; SubKoef: extended = 1);

    function FriendsDisable: boolean;
    function FriendUpdate(friend: TFriendRec): boolean;
    function FriendsUpdate(friends: TFriendRecArray): boolean;
    procedure FillGameFriends(lst: string);
    function FillGameFriendsFromDB: boolean;
    procedure UpdateFriendHelp(RoomID: integer; FriendID: int64; HelpCount: integer; Date: TDateTime);
    function CanHelpFriend(RoomID: integer; FriendID: int64): boolean;
    function AddFriendActivityPoints(FriendID: int64): boolean;
    function GetGroupedAppFriends(var GroupNum: integer; GroupCount: integer): string;

    function RecvdGiftsDisable: boolean;
    function RecvdGiftUpdate(gift: TGiftRec): boolean;
    function RecvdGiftsUpdate(gifts: TGiftRecs): boolean;
    function AvailGiftsDisable: boolean;
    function AvailGiftUpdate(gift: TGiftRec): boolean;
    function AvailGiftsUpdate(gifts: TGiftRecs): boolean;
    function GetGiftReward(GameItemID: int64): extended;
    function FillGiftsScore(var gifts: TGiftRecs): boolean;

    function GetPriorityBuildList: TStringDynArray;

    function MakeGifts: TpFIBQuery;
    function GetGiftFriendsList(MaxLevel: integer): TpFIBQuery;

    function CalcRewardPoints(OwnerUserID: int64): boolean;

    function GetGameItemId(name: String): cardinal;
    function GetGameItemName(id: cardinal): string;
    function GetGameItem(id: integer): TMGameItem; overload;
    function GetGameItem(name: string): TMGameItem; overload;

    function GetItemAttributes: TAttrs;
    function ClearGameItemsDB: boolean;
    function AddGameItem(item: TMGameItem): boolean;

    function GetMiningItems:TNameValArr;

//    function GetItemWorkXP(field: FieldRec): integer;
//    function GetContractXP(contractclass: string): integer;

    function FieldIsDeny(Name: string): boolean;
//    function isFieldDenyForWhishList(field: FieldRec): boolean;

    function GetExecContract(FactoryName: string): TExecContractRec;
//    function GetCAffectedItems(item: GameItemRec): string;

    function GetTodayUsedGiftsCnt: integer;
  end;

implementation

{ TMPdatabase }

function BToStr(b: boolean): string;
begin
  Result := IntToStr(integer(b))
end;

function TMPdatabase.AddFriendActivityPoints(FriendID: int64): boolean;
begin
  Result := false;
  if (not Connected) or (FriendID = 0) then exit;

  try
    FIBQuery.SQL.Text :=
      'update friends set activity = coalesce(activity + 1, 1) where id =' +
      IntToStr(FriendID);
    FIBQuery.ExecQuery;

    Result := true;
  except
  end;
end;

function TMPdatabase.AddGameItem(item: TMGameItem): boolean;
var
  i: Integer;
  Attr: TAttr;
begin
  Result := false;
  try
    FIBQuery.SQL.Text :=
      'insert into GAME_ITEMS (ID, NAME, SHOP_DEPARTMENT, ' +
      'SUPER_CLASS, RUS_NAME ' +
      ') values (' +
      IntToStr(item.ID) + ',''' +
      item.Name + ''',''' +
      item.ShopDept + ''',''' +
      item.SuperClass + ''',''' +
      item.RusName + ''')';
    FIBQuery.ExecQuery;

    for i := 0 to item.GetAttrCount - 1 do
    begin
      Attr := item.GetAttrI(i);
      FIBQuery.SQL.Text :=
        'insert into GAME_ITEMS_ATTR (GAME_ITEMS_ID, ATTRIBUTES_ID, ' +
        StrAttrType[Attr.AType] +
        ') values (' +
        IntToStr(item.ID) + ',' +
        IntToStr(Attr.ID) + ',' +
        Attr.StrDBGetVal + ')';
      FIBQuery.ExecQuery;
    end;

    Result := true;
  except
  end;
end;

function TMPdatabase.CalcRewardPoints(OwnerUserID: int64): boolean;
begin
  Result := false;
  if not Connected then exit;

  try
    FIBQuery.SQL.Text :=
      'execute procedure CALC_REWARD_POINTS(' +
      IntToStr(OwnerUserID) + ')';
    FIBQuery.ExecQuery;
    FIBTransaction.Commit;

    Result := true;
  except
  end;
end;

function TMPdatabase.CanHelpFriend(RoomID: integer; FriendID: int64): boolean;
begin
  Result := false;
  if not Connected then exit;

  try
    FIBQuery.SQL.Text :=
      'execute procedure CAN_HELP_FRIEND(' +
      IntToStr(RoomID) + ',' +
      IntToStr(FriendID) + ')';
    FIBQuery.ExecQuery;

    Result := FIBQuery.ParamValue('RES');
  except
  end;
end;

class constructor TMPdatabase.ClassCreate;
begin
  FInstance := nil;
end;

function TMPdatabase.ClearGameItemsDB: boolean;
begin
  Result := false;
  try
    FIBQuery.SQL.Text := 'delete from GAME_ITEMS_ATTR';
    FIBQuery.ExecQuery;
    FIBQuery.SQL.Text := 'delete from GAME_ITEMS';
    FIBQuery.ExecQuery;
    FIBTransaction.Commit;
    Result := true;
  except
  end;
end;

procedure TMPdatabase.Commit;
begin
  if FIBTransaction.Active then FIBTransaction.Commit;
end;

function TMPdatabase.Connect: boolean;
var
  FileName: string;
begin
  Result := false;
  if Connected then exit;

  FileName := ExtractFilePath(Application.ExeName) + 'mpolis.fdb';
  if FileExists(FileName) then
    FIBDatabase.DBName := FileName
  else
    FIBDatabase.DBName := 'C:\Projects\!chrome\MPolis2\mpolis.fdb';
  FIBDatabase.DBParams.Values['user_name'] := 'SYSDBA';
  FIBDatabase.DBParams.Values['password'] := 'masterkey';
  FIBDatabase.DBParams.Values['lc_ctype'] := 'utf-8';
  FIBDatabase.SQLDialect := 3;
  FIBDatabase.WaitForRestoreConnect := 100;
  FIBDatabase.Open(true);

  FConnected := FIBDatabase.Connected;
  Result := FConnected;
end;

constructor TMPdatabase.Create;
begin
  inherited Create;

  DBFloatFormat := TFormatSettings.Create;
  DBFloatFormat.DecimalSeparator := '.';

  SetLength(FExecContractList, 0);
  SetLength(FGameFriend, 0);

  FWantListLoaded := false;
  FRewardLoaded := false;
  FExecContractListLoaded := 0;
  FFieldDenyLoaded := 0;

  FIBDatabase := TpFIBDatabase.Create(nil);

  FIBTransaction := TpFIBTransaction.Create(nil);
  FIBDatabase.DefaultTransaction := FIBTransaction;
  FIBDatabase.DefaultUpdateTransaction := FIBTransaction;

  FIBQuery := TpFIBQuery.Create(nil);
  FIBQuery.Database := FIBDatabase;
  FIBQuery.Options := FIBQuery.Options + [qoStartTransaction];
  FIBQuery.GoToFirstRecordOnExecute := false;

  FIBQueryCurs := TpFIBQuery.Create(nil);
  FIBQueryCurs.Database := FIBDatabase;
  FIBQueryCurs.Options := FIBQuery.Options + [qoStartTransaction];
  FIBQueryCurs.GoToFirstRecordOnExecute := true;
end;

destructor TMPdatabase.Destory;
begin
  FIBDatabase.Free;
  FIBQuery.Free;
  FIBTransaction.Free;

  inherited;
end;
  {
function TMPdatabase.FillRewardList: boolean;
begin
  Result := false;
  try
    FIBQuery.Close;
    FIBQuery.SQL.Text :=
      'select * from giftrewards order by rewardpoints desc';
    FIBQuery.ExecQuery;

    SetLength(FRewardList, 0);

    while not FIBQuery.Eof do
    begin
      if FIBQuery.FieldByName('gift_id').AsInt64 <> 0 then
      begin
        SetLength(FRewardList, length(FRewardList) + 1);
        FRewardList[length(FRewardList) - 1].GiftID :=
          FIBQuery.FieldByName('gift_id').AsInt64;
        FRewardList[length(FRewardList) - 1].Score :=
          FIBQuery.FieldByName('rewardpoints').AsFloat;
      end;

      FIBQuery.Next;
    end;
    FIBQuery.Close;

    FRewardLoaded := true;
    Result := true;
  except
  end;
end;

 }

function TMPdatabase.FriendsDisable: boolean;
begin
  Result := false;
  if not Connected then exit;

  try
    FIBQuery.SQL.Text := 'update FRIENDS set DELETED = 1';
    FIBQuery.ExecQuery;
    FIBTransaction.Commit;

    Result := true;
  except
  end;
end;

function TMPdatabase.FriendsUpdate(friends: TFriendRecArray): boolean;
var
  i: Integer;
begin
  Result := true;
  for i := 0 to length(friends) - 1 do
  try
    Result := Result and FriendUpdate(friends[i]);
  except
    break;
  end;
  FIBTransaction.Commit;
end;

function TMPdatabase.FriendUpdate(friend: TFriendRec): boolean;
begin
  Result := false;
  if not Connected then exit;
  try
    FIBQuery.SQL.Text :=
      'execute procedure FRIEND_UPDATE(' + IntToStr(friend.ID) +
      ',' + IntToStr(friend.Level) +
      ',''' + friend.WishList + ''')';
    FIBQuery.ExecQuery;
    Result := true;
  except
  end;
end;


{
function TMPdatabase.GetCAffectedItems(item: GameItemRec): string;
var
 i: integer;
begin
  Result := '';
  if not Connected then exit;
  if not FExecContractListLoaded then
    FillExecContractList;

  for i := 0 to Length(FExecContractList) - 1 do
    if pos(FExecContractList[i].name, item.ItemName) = 1 then
    begin
      Result := FExecContractList[i].affected_items;
      exit;
    end;
end;

function TMPdatabase.GetContractXP(contractclass: string): integer;
var
  item: GameItemRec;
  cc: cardinal;
begin
  Result := 0;
  cc := StrToIntDef(contractclass, 0);
  if cc = 0 then exit;

  item := GetItemDetailsByClass(
    GetNameByClassId(cc));
  if (item.ShopDept <> 'contracts') and
     (item.ID <> cc)
  then exit;

  Result := item.XP;
end;
       }
function TMPdatabase.GetGiftFriendsList(MaxLevel: integer): TpFIBQuery;
begin
  Result := nil;
  if not Connected then exit;

  try
    //  рассыпаем ненужные подарки начиная с мелких и в игре
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text :=
      'select fr.* ' +
      'from friends fr ' +
      'where level <=' + IntToStr(MaxLevel) + ' ' +
      'order by ingame desc, fr.reward_points, fr.level';
    FIBQueryCurs.ExecQuery;
    Result := FIBQueryCurs;
  except
    AddLog('GetGiftFriendsList execution exception', 0);
  end;
end;

function TMPdatabase.GetGameItem(name: string): TMGameItem;
begin
  Result := GetGameItem(GetGameItemId(name));
end;

function TMPdatabase.GetGameItemId(name: String): cardinal;
begin
  Result := 0;
  if not Connected then exit;

  FIBQuery.SQL.Text := 'execute procedure GET_ITEM_ID(''' + name + ''')';
  FIBQuery.ExecQuery;
  Result := FIBQuery.ParamValue('id');
end;

function TMPdatabase.GetGroupedAppFriends(var GroupNum: integer; GroupCount: integer): string;
var
  i,
  indx,
  ti,
  cnt,
  frcount: integer;
  friends: array of integer;
begin
  Result := '';

  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text :=
      'select count(1) cnt from friends where ingame=1';
    FIBQueryCurs.ExecQuery;
    frcount := FIBQueryCurs.FieldByName('cnt').AsInteger;
    FIBQueryCurs.Close;

    if GroupNum >= Trunc(frcount / GroupCount + 0.99999) then
      GroupNum := 0;

    SetLength(friends, 0);
    FIBQueryCurs.SQL.Text :=
      'select * from friends where ingame=1 ' +
      'order by trunc((activity + 9) / 10) desc, ' +
      'level desc, id';
    FIBQueryCurs.ExecQuery;

    cnt := 0;
    while not FIBQueryCurs.Eof do
    begin
      if (GroupNum + 1) * GroupCount <= cnt then break;

      if GroupNum * GroupCount <= cnt then
      begin
        SetLength(friends, length(friends) + 1);
        friends[length(friends) - 1] :=
          FIBQueryCurs.FieldByName('ID').AsInteger;
      end;

      cnt := cnt + 1;
      FIBQueryCurs.Next;
    end;
    FIBQueryCurs.Close;

    if (length(friends) < GroupCount) and
       (frcount > GroupCount) then
    begin
      FIBQueryCurs.ExecQuery;
      while not FIBQueryCurs.Eof do
      begin
        SetLength(friends, length(friends) + 1);
        friends[length(friends) - 1] :=
          FIBQueryCurs.FieldByName('ID').AsInteger;

        if length(friends) >= GroupCount then break;
        FIBQueryCurs.Next;
      end;
      FIBQueryCurs.Close;
    end;

    for i := 0 to length(friends) - 1 do
    begin
      indx := random(length(friends) - 1);
      ti := friends[indx];
      friends[indx] := friends[i];
      friends[i] := ti;
    end;

    for i := 0 to length(friends) - 1 do
      Result := Result + IntToStr(friends[i]) + ',';

    Result := Copy(Result, 1, length(Result) - 1);
  except
  end;
end;

class function TMPdatabase.GetInstance: TMPdatabase;
begin
  if not Assigned(FInstance) then
    FInstance := TMPdatabase.Create;
  Result := FInstance;
end;

function TMPdatabase.GetItemAttributes: TAttrs;
begin
  SetLength(Result, 0);
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from ATTRIBUTES';
    FIBQueryCurs.ExecQuery;

    while not FIBQueryCurs.Eof do
    begin
      SetLength(Result, length(Result) + 1);
      Result[length(Result) - 1].ID :=
        FIBQueryCurs.FieldByName('ID').AsInteger;
      Result[length(Result) - 1].Name :=
        FIBQueryCurs.FieldByName('NAME').AsString;
      Result[length(Result) - 1].AType :=
        TAttrType(FIBQueryCurs.FieldByName('ATTR_TYPE_ID').AsInteger);
      Result[length(Result) - 1].ATransform :=
        TAttrTransform(FIBQueryCurs.FieldByName('ATTR_TRANSFORM_ID').AsInteger);

      FIBQueryCurs.Next;
    end;
    FIBQueryCurs.Close;
  except
    SetLength(Result, 0);
  end;
end;

function TMPdatabase.GetMiningItems: TNameValArr;
begin
  SetLength(Result, 1);
  Result[0].Name := 'iron_ore';
  Result[0].Value := 30;
end;

function TMPdatabase.GetGameItem(id: integer): TMGameItem;
var
  Attr: TAttr;
begin
  Result := nil;
  if not Connected then exit;

  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from GAME_ITEMS where ID=' + IntToStr(id) + ' ';
    FIBQueryCurs.ExecQuery;

    if FIBQueryCurs.FieldByName('ID').AsString <> IntToStr(id) then exit;

    Result := TMGameItem.Create;
    Result.ID := FIBQueryCurs.FieldByName('ID').AsInt64;
    Result.Name := FIBQueryCurs.FieldByName('NAME').AsString;
    Result.ShopDept := FIBQueryCurs.FieldByName('SHOP_DEPARTMENT').AsString;
    Result.SuperClass := FIBQueryCurs.FieldByName('SUPER_CLASS').AsString;
    Result.RusName := Utf8ToAnsi(RawByteString(FIBQueryCurs.FieldByName('RUS_NAME').AsString));

    FIBQueryCurs.Close;

    FIBQueryCurs.SQL.Text :=
      'select * from GAME_ITEMS_ATTR a, attributes ar  where ' +
      'a.attributes_id = ar.id and a.GAME_ITEMS_ID=' + IntToStr(id);
    FIBQueryCurs.ExecQuery;
    while not FIBQueryCurs.Eof do
    begin
      Attr.Clear;

      Attr.ID := FIBQueryCurs.FieldByName('ATTRIBUTES_ID').AsInteger;
      Attr.Name := FIBQueryCurs.FieldByName('NAME').AsString;
      Attr.AType := TAttrType(FIBQueryCurs.FieldByName('ATTR_TYPE_ID').AsInteger);
      Attr.ATransform := TAttrTransform(FIBQueryCurs.FieldByName('ATTR_TRANSFORM_ID').AsInteger);

      case Attr.AType of
        atBigInt: Attr.Value := FIBQueryCurs.FieldByName('ASBIGINT').AsInt64;
        atInt: Attr.Value := FIBQueryCurs.FieldByName('ASINT').AsInteger;
        atBool: Attr.Value := FIBQueryCurs.FieldByName('ASBOOL').AsBoolean;
        atVarchar: Attr.Value := FIBQueryCurs.FieldByName('ASVARCHAR').AsString;
        atFloat: Attr.Value := FIBQueryCurs.FieldByName('ASFLOAT').AsDouble;
      else
        Attr.Value := Null;
      end;

      if Attr.ID <> 0 then Result.AddAttr(Attr);

      FIBQueryCurs.Next;
    end;
    FIBQueryCurs.Close;
  except
  end;
end;
   {
function TMPdatabase.GetItemWorkXP(field: FieldRec): integer;
begin
  case field.Func of
    FN_PUT: Result := 0; // put contract
    FN_PICK:             // pick contract or peoples
      if field.item.isFactory then
        Result := GetContractXP(field.GetPrcContactID)    // contract XP
      else
        Result := field.item.ExtraXP;
  else
    Result := field.item.ExtraXP;
  end;
end;
    }
function TMPdatabase.GetGameItemName(id: cardinal): string;
begin
  Result := '';
  if not Connected then exit;

  FIBQuery.SQL.Text := 'execute procedure GET_ITEM_NAME(' + IntToStr(id) + ')';
  FIBQuery.ExecQuery;
  Result := FIBQuery.ParamValue('name');
end;

function TMPdatabase.GetGiftReward(GameItemID: int64): extended;
begin
  Result := 0;
  if not Connected then exit;

  FIBQuery.SQL.Text := 'execute procedure GIFT_REWARD(' + IntToStr(GameItemID) + ')';
  FIBQuery.ExecQuery;
  Result := FIBQuery.ParamValue('reward');
end;

function TMPdatabase.FillGiftsScore(var gifts: TGiftRecs): boolean;
var
  i: Integer;
begin
  for i := 0 to length(gifts) - 1 do
    gifts[i].Score := GetGiftReward(gifts[i].GameItemID);
  Result := true;
end;

function TMPdatabase.GetPriorityBuildList: TStringDynArray;
begin
  SetLength(Result, 0);
  try
    FIBQuery.Close;
    FIBQuery.SQL.Text :=
      'select * from PRIORITY_BUILD order by PRIORITY desc';
    FIBQuery.ExecQuery;

    while not FIBQuery.Eof do
    begin
      if FIBQuery.FieldByName('NAME').AsString <> '' then
      begin
        SetLength(Result, length(Result) + 1);
        Result[length(Result) - 1] :=
          FIBQuery.FieldByName('NAME').AsString;
      end;

      FIBQuery.Next;
    end;
    FIBQuery.Close;
  except
    SetLength(Result, 0);
  end;
end;

function TMPdatabase.GetExecContract(FactoryName: string): TExecContractRec;
var
 i: integer;
begin
  Result.Clear;
  if not Connected then exit;
  UpdateExecContractList;

  for i := 0 to Length(FExecContractList) - 1 do
    if pos(FExecContractList[i].Name, FactoryName) = 1 then
    begin
      Result := FExecContractList[i];
      exit;
    end;
end;

function TMPdatabase.GetRewardPoints(userid: int64): Extended;
begin
  Result := 0;
  if not Connected then exit;

  FIBQuery.SQL.Text := 'execute procedure GET_FRIEND_REWARD_POINTS(' + IntToStr(userid) + ')';
  FIBQuery.ExecQuery;
  Result := FIBQuery.ParamValue('reward');
end;

function TMPdatabase.GetTodayUsedGiftsCnt: integer;
begin
  Result := 0;
  if not Connected then exit;

  FIBQuery.SQL.Text :=
    'execute procedure GET_TODAY_USED_GIFTS';
  FIBQuery.ExecQuery;
  Result := FIBQuery.ParamValue('cnt');
end;

function TMPdatabase.AvailGiftsDisable: boolean;
begin
  Result := false;
  if not Connected then exit;

  try
    FIBQuery.SQL.Text := 'delete from GIFTS_AVAIL';
    FIBQuery.ExecQuery;
    FIBTransaction.Commit;

    Result := true;
  except
  end;
end;

function TMPdatabase.AvailGiftUpdate(gift: TGiftRec): boolean;
begin
  Result := false;
  if not Connected then exit;

  try
    FIBQuery.SQL.Text :=
      'update or insert into GIFTS_AVAIL (id, game_items_id, qty) values(' +
      IntToStr(gift.ID) + ', ' +
      IntToStr(gift.GameItemID) + ', ' +
      IntToStr(gift.Qty) + ') matching (id)';
    FIBQuery.ExecQuery;

    Result := true;
  except
  end;
end;

function TMPdatabase.AvailGiftsUpdate(gifts: TGiftRecs): boolean;
var
  i: Integer;
begin
  Result := true;

  AvailGiftsDisable;
  for i := 0 to length(gifts) - 1 do
  try
    Result := Result and AvailGiftUpdate(gifts[i]);
  except
    break;
  end;

  if FIBTransaction.Active then FIBTransaction.Commit;
end;

function TMPdatabase.RecvdGiftsDisable: boolean;
begin
  Result := false;
  if not Connected then exit;

  try
    FIBQuery.SQL.Text := 'update GIFTS_REC set STATE = 1';
    FIBQuery.ExecQuery;
    FIBTransaction.Commit;

    Result := true;
  except
  end;
end;

function TMPdatabase.RecvdGiftsUpdate(gifts: TGiftRecs): boolean;
var
  i: Integer;
begin
  Result := true;

  RecvdGiftsDisable;
  for i := 0 to length(gifts) - 1 do
  try
    Result := Result and RecvdGiftUpdate(gifts[i]);
  except
    break;
  end;

  FIBTransaction.Commit;
end;

function TMPdatabase.RecvdGiftUpdate(gift: TGiftRec): boolean;
begin
  Result := false;
  if not Connected then exit;

  try
    FIBQuery.SQL.Text :=
      'update or insert into GIFTS_REC(ID, GAME_ITEMS_ID, QTY, FROM_FRIENDS_ID)' +
      ' values (' +
      IntToStr(gift.ID) + ', ' +
      IntToStr(gift.GameItemID) + ', ' +
      IntToStr(gift.Qty) + ', ' +
      IntToStr(gift.FromUser) + ')' +
      ' matching (ID)';
    FIBQuery.ExecQuery;

    Result := true;
  except
  end;
end;

function TMPdatabase.SetParam(ParamGroup: integer; ParamName: string;
  ParamValue: integer): boolean;
begin
  Result := SetParam(ParamGroup, ParamName, IntToStr(ParamValue));
end;

function TMPdatabase.SetParam(ParamName: string; ParamValue: Integer): boolean;
begin
  Result := SetParam(-1, ParamName, IntToStr(ParamValue));
end;

function TMPdatabase.FieldIsDeny(Name: string): boolean;
var
  i: integer;
begin
  Result := false;
  UpdateFieldDenyList;

  for i := 0 to length(FFieldDeny) - 1 do
    if Pos(FFieldDeny[i], Name) = 1 then
    begin
      Result := true;
      break;
    end;
end;
     {
function TMPdatabase.isFieldDenyForWhishList(field: FieldRec): boolean;
begin
  Result := false;

  //  bugs from database
  if (field.item.isBuildSite) and
     (field.item.Produce = '') and
     (field.InputFill = '')
  then
  begin
    Result := true;
    exit;
  end;
end;
}

function TMPdatabase.MakeGifts: TpFIBQuery;
begin
  Result := nil;
  if not Connected then exit;

  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text :=
      'select fr.*, gi.name, wi.game_items_id, ga.id as gift_un_id ' +
      'from friends fr, wishlist wi, gifts_avail ga, game_items gi ' +
      'where ' +
      '(fr.ingame = 1) and ' +
      '(fr.id = wi.friends_id) and ' +
      '(wi.game_items_id = gi.id) and ' +
      '(wi.game_items_id = ga.game_items_id) ' +
      'order by fr.reward_points desc, fr.level desc ';

    FIBQueryCurs.ExecQuery;
    Result := FIBQueryCurs;
  except
    AddLog('MakeGifts execution exception', 0);
  end;
end;

procedure TMPdatabase.UpdateFriendHelp(RoomID: integer; FriendID: int64; HelpCount: integer; Date: TDateTime);
begin
  if not Connected then exit;

  FIBQuery.SQL.Text := 'update or insert into friends_help (FRIENDS_ID, ROOM_ID, HELP_COUNT, HELP_DT) values (' +
    IntToStr(FriendID) + ',' + IntToStr(RoomID) + ',' +
    IntToStr(HelpCount) + ',' +
    '''' + FormatDateTime('DD.MM.YYYY HH:NN:SS', Date) + ''') matching(FRIENDS_ID, ROOM_ID)';
  FIBQuery.ExecQuery;
  Commit;
end;

procedure TMPdatabase.SubtractReward(gift: TSendGiftRec; SubKoef: extended);
var
 rew: extended;
begin
  if not Connected then exit;

  try
    rew := GetGiftReward(gift.GameItemID) * SubKoef;
    FIBQuery.SQL.Text :=
      'update FRIENDS set ' +
      'REWARD_POINTS = REWARD_POINTS - ' + FormatFloat('0.00', rew, DBFloatFormat) + ',' +
      'HINT= ''subtracted=' + FormatFloat('0.00', rew, DBFloatFormat) + ' gi=' + IntToStr(gift.GameItemID) + '''' +
      ' where ID=' + IntToStr(gift.UserID);
    FIBQuery.ExecQuery;
  except
  end;
end;


function TMPdatabase.UpdateExecContractList: boolean;
begin
  Result := false;
  // list up to date)))
  if FExecContractListLoaded + 30 / MinsPerDay > Now then
  begin
    Result := true;
    exit;
  end;

  // fill list
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from EXEC_CONTRACTS';
    FIBQueryCurs.ExecQuery;

    SetLength(FExecContractList, 0);

    while not FIBQueryCurs.Eof do
    begin
      SetLength(FExecContractList, length(FExecContractList) + 1);
      FExecContractList[length(FExecContractList) - 1].Name :=
        trim(FIBQueryCurs.FieldByName('FACTORY_NAME').AsString);
      FExecContractList[length(FExecContractList) - 1].Klass :=
        trim(FIBQueryCurs.FieldByName('CONTRACT_NAME').AsString);
      FExecContractList[length(FExecContractList) - 1].AffectedItems :=
        trim(FIBQueryCurs.FieldByName('AFFECTED_ITEMS').AsString);
      FExecContractList[length(FExecContractList) - 1].HelpName :=
        trim(FIBQueryCurs.FieldByName('HELP_NAME').AsString);
      FExecContractList[length(FExecContractList) - 1].HelpSlotsLink :=
        trim(FIBQueryCurs.FieldByName('HELP_SLOTS_LINK').AsString);
      FExecContractList[length(FExecContractList) - 1].HelpMsg :=
        trim(FIBQueryCurs.FieldByName('HELP_MSG').AsString);
      FExecContractList[length(FExecContractList) - 1].TacticID :=
        FIBQueryCurs.FieldByName('TACTIC_ID').AsInteger;

      FIBQueryCurs.Next;
    end;
    FIBQueryCurs.Close;

    FExecContractListLoaded := Now;
    Result := true;
  except
  end;
end;

function TMPdatabase.UpdateFieldDenyList: boolean;
begin
  Result := false;

  // list up to date)))
  if FFieldDenyLoaded + 30 / MinsPerDay > Now then
  begin
    Result := true;
    exit;
  end;

  // fill list
  try
    FIBQuery.Close;
    FIBQuery.SQL.Text :=
      'select * from deny_fields';
    FIBQuery.ExecQuery;

    SetLength(FFieldDeny, 0);

    while not FIBQuery.Eof do
    begin
      if FIBQuery.FieldByName('name').AsString <> '' then
      begin
        SetLength(FFieldDeny, length(FFieldDeny) + 1);
        FFieldDeny[length(FFieldDeny) - 1] :=
          FIBQuery.FieldByName('name').AsString;
      end;

      FIBQuery.Next;
    end;
    FIBQuery.Close;

    FFieldDenyLoaded := Now;
    Result := true;
  except
  end;
end;

procedure TMPdatabase.FillGameFriends(lst: string);
var
 i: integer;
 sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Delimiter := ',';
  sl.DelimitedText := lst;

  FIBQuery.SQL.Text := 'update friends set ingame = 0';
  FIBQuery.ExecQuery;

  for i := 0 to sl.Count - 1 do
   if StrToIntDef(sl[i], 0) <> 0 then
   begin
    SetLength(FGameFriend, length(FGameFriend) + 1);
    FGameFriend[length(FGameFriend) - 1] :=
      StrToIntDef(sl[i], 0);

    FIBQuery.SQL.Text :=
      'update friends set ingame = 1 where id = ' + sl[i];
    FIBQuery.ExecQuery;
   end;
  sl.Free;

  Commit;

  FillGameFriendsFromDB;
end;

function TMPdatabase.FillGameFriendsFromDB: boolean;
begin
  Result := false;
  SetLength(FGameFriend, 0);
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from friends where ingame=1';
    FIBQueryCurs.ExecQuery;

    while not FIBQueryCurs.Eof do
    begin
      SetLength(FGameFriend, length(FGameFriend) + 1);
      FGameFriend[length(FGameFriend) - 1] :=
        FIBQueryCurs.FieldByName('ID').AsInt64;

      FIBQueryCurs.Next;
    end;
    FIBQueryCurs.Close;

    Result := true;
  except
  end;
end;

function TMPdatabase.SetParam(ParamName, ParamValue: string): boolean;
begin
  Result := SetParam(-1, ParamName, ParamValue);
end;

function TMPdatabase.SetParam(ParamGroup: integer; ParamName: string;
  ParamValue: string): boolean;
var
  sparam: string;
begin
  Result := false;
  if not FConnected then exit;
  try
    if ParamGroup < 0 then
      sparam := 'NULL'
    else
      sparam := IntToStr(ParamGroup);

    FIBQuery.SQL.Text := 'update or insert into params (id, name, param_group_id, ATTR_TYPE_ID, val) values ' +
      '(gen_id(gen_params_id,1), ''' + ParamName + ''', ' + sparam +', 4, ''' + ParamValue + ''') matching (name, param_group_id)';
    FIBQuery.ExecQuery;

    Commit;
    Result := true;
  except
  end;
end;

function TMPdatabase.GetParam(ParamGroup: integer; ParamName: string; Default: string = ''): string;
var
  cond: string;
begin
  Result := Default;
  if not FConnected then exit;
  try
    if ParamGroup < 0 then
      cond := 'PARAM_GROUP_ID is NULL'
    else
      cond := 'PARAM_GROUP_ID=' + IntToStr(ParamGroup);

    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text :=
      'select * from params where (NAME=''' + ParamName + ''') and' +
        '(' + cond + ')';
    FIBQueryCurs.ExecQuery;

    Result := FIBQueryCurs.FieldByName('VAL').AsString;

    FIBQueryCurs.Close;
  except
    Result := Default;
  end;
end;


function TMPdatabase.GetParamGroups: TNameValArr;
begin
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from PARAM_GROUP';
    FIBQueryCurs.ExecQuery;

    SetLength(Result, 0);

    while not FIBQueryCurs.Eof do
    begin
      SetLength(Result, length(Result) + 1);
      Result[length(Result) - 1].Value :=
        FIBQueryCurs.FieldByName('ID').AsInteger;
      Result[length(Result) - 1].Name :=
        FIBQueryCurs.FieldByName('NAME').AsString;

      FIBQueryCurs.Next;
    end;
    FIBQueryCurs.Close;
  except
  end;
end;

end.
