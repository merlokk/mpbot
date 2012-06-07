unit uDB;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Forms, math, Types,
  FIBDatabase, pFIBDatabase, FIBQuery, pFIBQuery, pFIBProps, uDefs,
  StrUtils, Generics.Collections, Generics.Defaults,
  uGameItems;

type
  TMPdatabase = class
  protected
    class var FInstance: TMPdatabase;
    class constructor ClassCreate;
  private
    FConnected: boolean;

    FIBDatabase: TpFIBDatabase;
    FIBQuery: TpFIBQuery;
    FIBQueryCurs: TpFIBQuery;
    FIBTransaction: TpFIBTransaction;

//    FWantList: array of WantListRec;
    FWantListLoaded: boolean;
//    FRewardList: array of WantListRec;
    FRewardLoaded: boolean;

    FExecContractList: array of ExecContractRec;
    FExecContractListLoaded: TDateTime;

    FGameFriend: array of integer;
    FFieldDenyLoaded: TDateTime;
    FFieldDeny: array of string;

    function UpdateExecContractList: boolean;
//    function FillWantList: boolean;
//    function FillRewardList: boolean;
    function UpdateFieldDenyList: boolean;
  public
    class function GetInstance: TMPdatabase; static;

    constructor Create;
    destructor Destory;

    property Connected: boolean read FConnected;
    function Connect: boolean;
    procedure Commit;

    function SetParam(ParamName, ParamValue: string): boolean;
    function GetParam(ParamName: string; Default: string = ''): string;

    function GetRewardPoints(userid: int64): Extended;
//    procedure SubtractReward(gift: SendGiftRec);

    function FriendsDisable: boolean;
    function FriendUpdate(friend: TFriendRec): boolean;
    function FriendsUpdate(friends: TFriendRecArray): boolean;
    procedure FillGameFriends(lst: string);
    function FillGameFriendsFromDB: boolean;
//    procedure SetFriendNextHelp(FriendID: int64; Date: TDateTime);
//    function CanHelpFriend(FriendID: int64): boolean;
    function AddFriendActivityPoints(FriendID: int64): boolean;
//    function GetGroupedAppFriends(var GroupNum: integer; GroupCount: integer): string;

    function RecvdGiftsDisable: boolean;
    function RecvdGiftUpdate(gift: TGiftRec): boolean;
    function RecvdGiftsUpdate(gifts: TGiftRecs): boolean;
//    function GiftsAvailDisable: boolean;
//    function GiftsAvailUpdate(gift: GiftRec): boolean;
{
    function GetGiftScore(gift_id: cardinal): extended;
    function QueryWantList: boolean;
}
    function GetPriorityBuildList: TStringDynArray;

{    function MakeGifts: TpFIBQuery;
    function GetFriendsList: TpFIBQuery;
}
    function CalcRewardPoints(OwnerUserID: int64): boolean;

    function GetGameItemId(name: String): cardinal;
    function GetGameItemName(id: cardinal): string;
    function GetGameItem(id: integer): TMGameItem; overload;
    function GetGameItem(name: string): TMGameItem; overload;

    function GetItemAttributes: TAttrs;
    function ClearGameItemsDB: boolean;
    function AddGameItem(item: TMGameItem): boolean;

//    function GetItemWorkXP(field: FieldRec): integer;
//    function GetContractXP(contractclass: string): integer;

    function FieldIsDeny(Name: string): boolean;
//    function isFieldDenyForWhishList(field: FieldRec): boolean;

//    function CanPut(item: GameItemRec): boolean;
    function GetCPutKlass(FactoryName: string): string;
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
    {
function TMPdatabase.CanHelpFriend(FriendID: int64): boolean;
begin
  Result := false;
  if not Connected then exit;

  try
    FIBQuery.SQL.Text :=
      'execute procedure CAN_HELP_FRIEND(' +
      IntToStr(FriendID) + ')';
    FIBQuery.ExecQuery;

    Result := FIBQuery.ParamValue('RES');
  except
  end;
end;

function TMPdatabase.CanPut(item: GameItemRec): boolean;
var
 i: integer;
begin
  Result := false;
  if not Connected then exit;
  if not FExecContractListLoaded then
    FillExecContractList;

  for i := 0 to Length(FExecContractList) - 1 do
    if pos(FExecContractList[i].name, item.ItemName) = 1 then
    begin
      Result := true;
      exit;
    end;
end; }

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
begin
  Result := false;
  if Connected then exit;

//  FIBDatabase.DBName := ExtractFilePath(Application.ExeName) + 'mpolis.fdb';
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

function TMPdatabase.FillWantList: boolean;
begin
  Result := false;
  try
    FIBQuery.Close;
    FIBQuery.SQL.Text :=
      'select gift_id, count(gift_id) cnt from whishlist group by gift_id having count(gift_id) > 1 order by cnt desc';
    FIBQuery.ExecQuery;

    SetLength(FWantList, 0);

    while not FIBQuery.Eof do
    begin
      SetLength(FWantList, length(FWantList) + 1);
      FWantList[length(FWantList) - 1].GiftID :=
        FIBQuery.FieldByName('gift_id').AsInt64;
      FWantList[length(FWantList) - 1].Score :=
        FIBQuery.FieldByName('cnt').AsInteger;

      FIBQuery.Next;
    end;
    FIBQuery.Close;

    FWantListLoaded := true;
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

function TMPdatabase.GetFriendsList: TpFIBQuery;
begin
  Result := nil;
  if not Connected then exit;

  try
    FIBQueryCurs.SQL.Text :=
      'select fr.* ' +
      'from friends fr ' +
      'order by ingame desc, fr.reward_points desc, fr.level desc';
    FIBQueryCurs.ExecQuery;
    Result := FIBQueryCurs;
  except
  end;
end;
  }
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
   {
function TMPdatabase.GetGiftScore(gift_id: cardinal): extended;
var
 i: integer;
begin
  Result := -1;
  //  if gift in my wanted list ==> reward -10

  // if in wanted list all the friends ==> maximize
  for i := 0 to length(FWantList) - 1 do
    if FWantList[i].GiftID = gift_id then
    begin
      Result := max(Result, FWantList[i].Score);
      break;
    end;
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
      'level desc';
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
end;        }

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

function TMPdatabase.GetCPutKlass(FactoryName: string): string;
var
 i: integer;
begin
  Result := '';
  if not Connected then exit;
  UpdateExecContractList;

  for i := 0 to Length(FExecContractList) - 1 do
    if pos(FExecContractList[i].name, FactoryName) = 1 then
    begin
      Result := FExecContractList[i].klass;
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
 {
function TMPdatabase.GiftsAvailDisable: boolean;
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

function TMPdatabase.GiftsAvailUpdate(gift: GiftRec): boolean;
begin
  Result := false;
  if not Connected then exit;

  try
    FIBQuery.SQL.Text :=
      'execute procedure UPDATE_GIFTS_AVAIL(' +
      IntToStr(gift.id) + ', ' +
      IntToStr(gift.globalid) + ', ' +
      IntToStr(gift.qty) + ')';
    FIBQuery.ExecQuery;

    Result := true;
  except
  end;
end;           }

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

function TMPdatabase.MakeGifts: TpFIBQuery;
begin
  Result := nil;
  if not Connected then exit;

  try
    FIBQueryCurs.SQL.Text :=
      'select sq.*, gr.rewardpoints ' +
      'from ' +
      '(select fr.*, gi.name, wi.gift_id, ga.id as gift_un_id ' +
      'from friends fr,whishlist wi, gifts_avail ga, gifts gi ' +
      'where ' +
      '(fr.ingame = 1) and ' +
      '(fr.id = wi.user_id) and ' +
      '(wi.gift_id = gi.gift_id) and ' +
      '(wi.gift_id = ga.class_id) ' +
      'order by fr.reward_points desc, fr.level desc) sq ' +
      'left join giftrewards gr on sq.gift_id = gr.gift_id';
    FIBQueryCurs.ExecQuery;
    Result := FIBQueryCurs;
  except
  end;

end;

function TMPdatabase.QueryWantList: boolean;
begin
  if not FWantListLoaded then FillWantList;
  Result := FWantListLoaded;
end;

procedure TMPdatabase.SetFriendNextHelp(FriendID: int64; Date: TDateTime);
begin
  if not Connected then exit;

  FIBQuery.SQL.Text := 'update friends set NEXT_HELP_DT = ''' +
    FormatDateTime('DD.MM.YYYY', Date) + ''' where ID=' + IntToStr(FriendID);
  FIBQuery.ExecQuery;
  Commit;
end;

procedure TMPdatabase.SubtractReward(gift: SendGiftRec);
var
 i: integer;
 rew: extended;
begin
  if not Connected then exit;
  if not FRewardLoaded then FillRewardList;

  rew := 0;
  for i := 0 to length(FRewardList) - 1 do
    if FRewardList[i].GiftID = gift.globalid then
    begin
      rew := FRewardList[i].Score;
      break;
    end;

  if Round(rew * 5) = 0 then
   for i := 0 to length(FWantList) - 1 do
    if FWantList[i].GiftID = gift.globalid then
    begin
      rew := FWantList[i].Score / 15;
      if rew < 1 then rew := 1;
      break;
    end;

  if Round(rew * 5) <> 0 then
  try
    FIBQuery.SQL.Text :=
      'execute procedure SUB_FRIEND_REWARD_POINTS(' +
        IntToStr(gift.to_user) + ',' +
        FormatFloat('0.00', rew, FFloatFormat) + ')';
    FIBQuery.ExecQuery;
  except
  end;
end;

}
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
      FExecContractList[length(FExecContractList) - 1].name :=
        trim(FIBQueryCurs.FieldByName('FACTORY_NAME').AsString);
      FExecContractList[length(FExecContractList) - 1].klass :=
        trim(FIBQueryCurs.FieldByName('CONTRACT_NAME').AsString);
      FExecContractList[length(FExecContractList) - 1].affected_items :=
        trim(FIBQueryCurs.FieldByName('AFFECTED_ITEMS').AsString);

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
  Result := false;
  if not FConnected then exit;
  try
    FIBQuery.SQL.Text := 'update or insert into params (id, name, ATTR_TYPE_ID, val) values ' +
      '(gen_id(gen_params_id,1), ''' + ParamName + ''', 4, ''' + ParamValue + ''') matching (name)';
    FIBQuery.ExecQuery;

    Commit;
    Result := true;
  except
  end;
end;

function TMPdatabase.GetParam(ParamName: string; Default: string = ''): string;
begin
  Result := Default;
  if not FConnected then exit;
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from params where NAME=''' + ParamName + ''' ';
    FIBQueryCurs.ExecQuery;

    Result := FIBQueryCurs.FieldByName('VAL').AsString;

    FIBQueryCurs.Close;
  except
    Result := Default;
  end;
end;


end.
