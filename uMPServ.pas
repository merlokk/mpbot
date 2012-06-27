unit uMPserv;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  clHttpRequest, clGZip, clCookies, clConnection, clMultiDC, clSingleDC,
  clDownLoader, clTcpClient, clHttp,  Dialogs, StdCtrls, XMLIntf, XMLDoc,
  StrUtils,
  uFactories, uDB, uVK, uDefs, uGameItems, uLogger, uQueue;

type
  TMPServer = class
  private
   const
    CMaxVersion: integer = 99;
   var
    clCookieManager: TclCookieManager;
    clGZip: TclGZip;
    clHttpRequest,
    clHttpRequest2: TclHttpRequest;
    clHttp: TclHttp;

    FOwnerID: string;

    Fres: TStringList;
    frn: integer;

    FCurrUserStatVer,
    FCurrCheckPerformVer: integer;

    FServerVer,
    FUserStatVer,
    FUserStatFPVer,
    FCheckAndPerformVer: integer;
    FVerFP: string;
    FRevision,
    FAuthKey,
    FSessionKey,
    FAppFriends: String;
    FCurrRoomID: integer;
    FTimerms: cardinal;

    FGetRevisionTick: cardinal;
    FServerURL: string;

    class var FInst: TMPServer;
    class constructor ClassCreate;
    function GetPlayedTime: String;
    function GetNextUserStatVer: integer;
    function GetNextUserStatVerStr: string;
    function GetNextCheckPerformVer: integer;
    function GetNextCheckPerformVerStr: string;

    function SendPost(vURL: string; vHttpRequest: TclHttpRequest;
      sl: TstringList): boolean;
    function SendGet(vURL: string; vHttpRequest: TclHttpRequest;
      sl: TstringList): boolean;
    procedure FillFormData(vHttpRequest: TclHttpRequest; data: string);

    function SrvGetUserStatFirst(RoomID: integer): String;
    function SrvGetUserStat(RoomID: integer; data: string): String;
    function SrvCheckAndPerform(Qu: TActionQueue): String;
    function SrvCheckReqResult(data: string): boolean;
  public
    class function GetInstance: TMPServer;
    constructor Create;

    procedure Clear;

    function GetRevision(World: TMWorld): boolean;
    function GetCityXML(sl: TStringList): boolean;
    function LoadFile(URL, FileName: string): integer;
    function GotRevisionTime: cardinal;
    function GetUserStat(World: TMWorld; RoomID: integer; StartNewSession: boolean = false): boolean;
    function GetUserStatFriend(OwnerID, FriendId: int64; data: String): String;
    function CheckAndPerform(World: TMWorld; Qu: TActionQueue): boolean;

    property OwnerID: string read FOwnerID write FOwnerID;
    property CurrRoomID: integer read FCurrRoomID;

    property UserStatVer: integer read FUserStatVer write FUserStatVer;
    property UserStatFPVer: integer read FUserStatFPVer write FUserStatFPVer;
    property CheckAndPerformVer: integer read FCheckAndPerformVer write FCheckAndPerformVer;
    property VerFP: string read FVerFP write FVerFP;

    property Revision: String read FRevision write FRevision;
    property AuthKey: String read FAuthKey write FAuthKey;
    property SessionKey: String read FSessionKey write FSessionKey;

    property AppFriends: String read FAppFriends write FAppFriends;
  end;

  TWorldHeaderHelper = record helper for TWorldHeader
    function ProcessResponseHeader(root: IXMLNode): boolean;
    procedure ProcessRoomResources(data: string; root: IXMLNode);
  end;

  TMWorldHelper = class helper for TMWorld
    function LoadFromXML(RoomID: integer; data: string): boolean;
    function LoadFriends(node: IXMLNode): boolean;
    function LoadGifts(node: IXMLNode): boolean;
    function LoadBarn(node: IXMLNode): boolean;
  end;

  TMRoomHelper = class helper for TMRoom
    function LoadFields(node: IXMLNode): boolean;
  end;

  TBarnRecHelper = record helper for TBarnRec
    function Load(node: IXMLNode): boolean;
  end;

  TGiftRecHelper = record helper for TGiftRec
    function Load(node: IXMLNode): boolean;
  end;

  TFriendRecHelper = record helper for TFriendRec
    function Load(node: IXMLNode): boolean;
  end;

  TMFieldHelper = class helper for TMField
    function Load(node: IXMLNode): boolean;
  end;

implementation

{ TMPServer }

function TMPServer.CheckAndPerform(World: TMWorld; Qu: TActionQueue): boolean;
var
  data: string;
begin
  Result := false;
  try
    if FSessionKey = '' then exit;

    data := SrvCheckAndPerform(Qu);
    SrvCheckReqResult(data);
    if data = '' then exit;

    World.LoadFromXML(FCurrRoomID, data);
    FSessionKey := World.LastHeader.SessionKey;

    Result := true;
    AddLog('server_time=' + IntToStr(World.LastHeader.ServerTime) +
      ' q: ' + Qu.StrStat +
      ' session_key=' + World.LastHeader.SessionKey, 4);
  except
  end;
end;

procedure TMPServer.FillFormData(vHttpRequest: TclHttpRequest; data: string);
var
  st1: string;
  i: Integer;
  sl: TStringList;
  st2: string;
begin
  sl := TStringList.Create;
  sl.Text := data;
  for i := 0 to sl.Count - 1 do
    if Pos(':', sl[i]) > 0 then
    begin
      st1 := Copy(sl[i], 1, Pos(':', sl[i]) - 1);
      st2 := Copy(sl[i], Pos(':', sl[i]) + 1, length(sl[i]));

      // unit clHttpRequest, procedure InitStaticVars;
      // Chars = '+&*%<>"#{}|\^~[]''?!=/:$;_,.';
      // не все было URLencoded!!!
      vHttpRequest.AddFormField(st1, st2);
    end;
  sl.Free;
end;

class constructor TMPServer.ClassCreate;
begin
  FInst := nil;
end;

procedure TMPServer.Clear;
begin
  FServerURL := 'http://88.212.222.164/city_server_vk_prod/';

  FOwnerID := '';
  FServerVer := 0;
  FUserStatVer := 1;
  FUserStatFPVer := 1;
  FCheckAndPerformVer := 2;
  FVerFP := '';

  FCurrUserStatVer := 0;
  FCurrCheckPerformVer := 0;

  FCurrRoomID := 0;
  FGetRevisionTick := 0;

  FSessionKey := '';

  frn := 0;
  FTimerms := GetTickCount;

  clHttp.Close;
end;

constructor TMPServer.Create;
begin
  inherited;

  Fres := TStringList.Create;

  clGZip := TclGZip.Create(Nil);
  clGZip.BatchSize := 32769;

  clHttpRequest := TclHttpRequest.Create(Nil);
  clHttpRequest.BatchSize := 32769;
  clHttpRequest.Header.AcceptEncoding := 'gzip,deflate,sdch';
  clHttpRequest.Header.AcceptCharSet := 'windows-1251,utf-8;q=0.7,*;q=0.3';
  clHttpRequest.Header.AcceptLanguage := 'ru-RU,ru;q=0.8,en-US;q=0.6,en;q=0.4';
  clHttpRequest.Header.Referer := 'http://cs305104.vkontakte.ru/u6148904/ef16d2ea6d5e19.zip';
  clHttpRequest.Header.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.79 Safari/535.11';

  clHttpRequest2 := TclHttpRequest.Create(Nil);
  clHttpRequest2.BatchSize := 32769;
  clHttpRequest2.Assign(clHttpRequest);

  clHttp := TclHttp.Create(Nil);
  clHttp.BatchSize := 32769;
  clHTTP.Request := clHttpRequest;
  clHttp.KeepConnection := true;
  clHttp.KeepAlive := 300;

  clCookieManager := TclCookieManager.Create(Nil);

  Clear;
end;

function TMPServer.GetCityXML(sl: TStringList): boolean;
begin
  Result := SendGet(
    'http://mb.static.socialquantum.ru/assets_vk_city_prod/i18n/ru/city.xml?r=' +
      FRevision,
    nil,
    sl);
end;

class function TMPServer.GetInstance: TMPServer;
begin
  if FInst = nil then FInst := TMPServer.Create;
  Result := FInst;
end;

function TMPServer.GetNextCheckPerformVer: integer;
begin
  if FCurrCheckPerformVer = 0 then
    FCurrCheckPerformVer := FCheckAndPerformVer;

  FCurrCheckPerformVer := FCurrCheckPerformVer + GetBaseRandom(4, 2);
  if FCurrCheckPerformVer > CMaxVersion then
    FCurrCheckPerformVer := FCheckAndPerformVer;
  Result := FCurrCheckPerformVer;
end;

function TMPServer.GetNextUserStatVer: integer;
begin
  if FCurrUserStatVer = 0 then
    FCurrUserStatVer := FUserStatVer;

  FCurrUserStatVer := FCurrUserStatVer + GetBaseRandom(4, 2);
  if FCurrUserStatVer > CMaxVersion then
    FCurrUserStatVer := FUserStatVer;
  Result := FCurrUserStatVer;
end;

function TMPServer.GetPlayedTime: String;
begin
  Result := IntToStr(GetTickCount - FTimerms);
  FTimerms := GetTickCount;
end;

function TMPServer.GetRevision(World: TMWorld): boolean;
var
 i: integer;
 s: string;
begin
  Result := false;
  for i := 0 to 2 do
    try
      FGetRevisionTick := GetTickCount;

      clHttp.Get(
        FServerURL + 'REVISION.txt?rand=' + GetRandomStr,
        Fres);

      if Fres.Count > 0 then s := Fres[0];
      if s <> '' then
      begin
        World.SrvRevision := s;
        FRevision := World.SrvRevision6;
        Result := true;

        World.SWFRevision := s;
        if Fres.Count > 1 then
        begin
          s := ReplaceStr(Fres[1], ',', '');
          if s <> '' then
            World.SWFRevision := s;
        end;


        break;
      end;
    except
    end;
  AddLog('MP revision=' + World.SrvRevision + ' ok=' + BoolToStr(Result, true));
end;

function TMPServer.SrvCheckAndPerform(Qu: TActionQueue): String;
begin
  Result := '';
  try
    clHttpRequest2.Assign(clHttpRequest);
    clHttpRequest2.Header.ContentType := 'application/x-www-form-urlencoded';
    clHttpRequest2.AddFormField('client_performance_stats', '{"first_request":11.343,"iframe":8.871,"render":11.337,"daily":110}');
    clHttpRequest2.AddFormField('avg_mem', GetBaseRandomStr(304844800, 100000));
    clHttpRequest2.AddFormField('revision', FRevision);
    clHttpRequest2.AddFormField('rand', GetRandomStr);
    clHttpRequest2.AddFormField('version', GetNextCheckPerformVerStr);
    clHttpRequest2.AddFormField('avg_fps', GetBaseRandomStr(8, 4));
    clHttpRequest2.AddFormField('user_id', FOwnerID);
    clHttpRequest2.AddFormField('room_id', IntToStr(FCurrRoomID));
    clHttpRequest2.AddFormField('time_pl', GetPlayedTime); // time from last request in ms
    clHttpRequest2.AddFormField('serv_ver', IntToStr(FServerVer));
    clHttpRequest2.AddFormField('auth_key', FAuthKey);
    clHttpRequest2.AddFormField('session_key', FSessionKey);

    Qu.FillFormData(clHttpRequest2);

    // номер запроса!!!
    frn := frn + 1;

    SendPost(
      'http://88.212.222.164/city_server_vk_prod/check_and_perform?uid=' + FOwnerID +
      '&crev=' + FRevision + '&fp=' + FVerFP + '&rn=' + IntToStr(Frn),
      clHttpRequest2, Fres);
    Result := Fres.Text;

    AddLog('server exec check_and_perform. elm=' + IntToStr(Qu.Count) +
      ' room=' + IntToStr(FCurrRoomID) +
      '. rn=' + IntToStr(frn) +
      '. res len=' + IntToStr(length(Result)));
    AddLogFile('!server', 'check_and_perform.xml', Fres);
  except
  end;
end;

function TMPServer.SrvCheckReqResult(data: string): boolean;
var
 doc: IXMLDocument;
 root,
 nod: IXMLNode;
 i: integer;
begin
  Result := false;
  try
    doc := LoadXMLData(data);
    if doc = nil then exit;

    root := doc.DocumentElement;
    if root = nil then
    begin
      AddLog('server error: empty response');
      exit;
    end;

    // logging messages
    nod := root.ChildNodes.FindNode('messages');
    if nod <> nil then
      for i := 0 to nod.ChildNodes.Count - 1 do
        AddLog('server message: ' +
           nod.ChildNodes[i].NodeName + ' ' +
          'fr:' + VarToStr(nod.ChildNodes[i].Attributes['friend_name']) +
          'code:' + VarToStr(nod.ChildNodes[i].Attributes['code'])
        , 5);

    // is OK
    if root.ChildNodes.FindNode('ok') <> nil then
    begin
      Result := true;
      exit;
    end;

    //  error reporting
    nod := root.ChildNodes.FindNode('errors');
    if nod <> nil then
    begin
      if nod.ChildNodes.Count = 0 then
        AddLog('server error: ' + nod.Text, 0)
      else
      begin
        AddLog('server errors: ' + IntToStr(nod.ChildNodes.Count));
        for i := 0 to nod.ChildNodes.Count - 1 do
          AddLog(
            VarToStr(nod.ChildNodes[i].Attributes['command'])+
            ': ' + nod.ChildNodes[i].Text
          , 0);
      end;
    end;

    nod := root.ChildNodes.FindNode('error');
    if nod <> nil then
      AddLog('server error: ' + nod.Text, 0);
  except
  end;
end;

function TMPServer.SrvGetUserStat(RoomID: integer; data: string): String;
begin
  clHttpRequest2.Assign(clHttpRequest);
  clHttpRequest2.AddFormField('client_performance_stats', '{"first_request":380.784}');
  clHttpRequest2.AddFormField('avg_mem', GetBaseRandomStr(197341184, 100000));
  clHttpRequest2.AddFormField('version', GetNextUserStatVerStr);
  clHttpRequest2.AddFormField('user_id', FOwnerID);
  clHttpRequest2.AddFormField('room_id', IntToStr(FCurrRoomID));
  clHttpRequest2.AddFormField('time_pl', GetPlayedTime);
  clHttpRequest2.AddFormField('avg_fps', GetBaseRandomStr(10, 5));
  clHttpRequest2.AddFormField('serv_ver', IntToStr(FServerVer));
  clHttpRequest2.AddFormField('auth_key', FAuthKey);

  if FCurrRoomID <> RoomID then
  begin
    clHttpRequest2.AddFormField('view_room_id', IntToStr(RoomID));
    clHttpRequest2.AddFormField('change_room', 'true');
    FCurrRoomID := RoomID;
  end;

  clHttpRequest2.AddFormField('session_key', FSessionKey);
  clHttpRequest2.AddFormField('revision', FRevision);
  clHttpRequest2.AddFormField('rand', GetRandomStr);

  FillFormData(clHttpRequest2, data);

  frn := frn + 1;

  SendPost(
    FServerURL + 'get_user_stat?uid=' + FOwnerID + '&crev=' +
    FRevision + '&fp=' + FVerFP + '&rn=' + IntToStr(Frn),
    clHttpRequest2,
    Fres);

  Result := Fres.Text;

  AddLog('server exec get_user_stat next. room=' + IntToStr(RoomID) +
    '. rn=' + IntToStr(frn) +
    '. res len=' + IntToStr(length(Result)));
  AddLogFile('!server', 'get_user_statN.xml', Fres);
end;

function TMPServer.SrvGetUserStatFirst(RoomID: integer): String;
begin
  frn := 0;

  FTimerms := GetTickCount;
  FTimerms := FTimerms + 1500 + Trunc(Random(1000));

  clHttpRequest2.Assign(clHttpRequest);
  clHttpRequest2.AddFormField('avg_mem', GetBaseRandomStr(197341184, 100000));
  clHttpRequest2.AddFormField('from_ie', '1');
  clHttpRequest2.AddFormField('version', GetNextUserStatVerStr);
  clHttpRequest2.AddFormField('adv_id', 'profile');
  clHttpRequest2.AddFormField('user_id', FOwnerID);
  clHttpRequest2.AddFormField('room_id', IntToStr(RoomID));
  clHttpRequest2.AddFormField('time_pl', GetPlayedTime);
  clHttpRequest2.AddFormField('avg_fps', GetBaseRandomStr(10, 5));
  clHttpRequest2.AddFormField('user_first_name', 'Олег');
  clHttpRequest2.AddFormField('first_request', 'true');
  clHttpRequest2.AddFormField('serv_ver', IntToStr(FServerVer));
  clHttpRequest2.AddFormField('user_sex', '2');
  clHttpRequest2.AddFormField('auth_key', FAuthKey);
  clHttpRequest2.AddFormField('session_key', 'null');
  clHttpRequest2.AddFormField('user_city', 'Харьков');
  clHttpRequest2.AddFormField('friends', FAppFriends);
  clHttpRequest2.AddFormField('user_last_name', 'Моисеенко');
  clHttpRequest2.AddFormField('user_birthdate', 'Thu Jul 10 1975');
  clHttpRequest2.AddFormField('revision', FRevision);
  clHttpRequest2.AddFormField('fp_ver', IntToStr(FUserStatFPVer));  //  change
  clHttpRequest2.AddFormField('rand', GetRandomStr);

  SendPost(
    FServerURL + 'get_user_stat?uid=' + FOwnerID + '&crev=' +
    FRevision + '&fp=' + FVerFP + '&rn=' + IntToStr(Frn),
    clHttpRequest2,
    Fres);

  Result := Fres.Text;
  FCurrRoomID := RoomID;

  AddLog('server exec get_user_stat first. room=' + IntToStr(RoomID) +
    '. rn=' + IntToStr(frn) +
    '. res len=' + IntToStr(length(Result)));
  AddLogFile('!server', 'get_user_statF.xml', Fres);
end;

function TMPServer.GetUserStat(World: TMWorld; RoomID: integer; StartNewSession: boolean): boolean;
var
  data: string;
begin
  Result := false;
  try
    if StartNewSession then FSessionKey := '';
    if FSessionKey = '' then
      data := SrvGetUserStatFirst(RoomID)
    else
      data := SrvGetUserStat(RoomID, '');
    SrvCheckReqResult(data);
    if data = '' then exit;

    World.LoadFromXML(RoomID, data);
    FSessionKey := World.LastHeader.SessionKey;

    World.CheckRoomInformation(World.LastHeader.RoomInformation);
    World.LastRoomChange := Now;

    Result := true;
    AddLog('server_time=' + IntToStr(World.LastHeader.ServerTime) +
      ' next_tick=' + IntToStr(World.LastHeader.NextTick) +
      ' session_key=' + World.LastHeader.SessionKey, 4);
  except
  end;
end;

function TMPServer.GetUserStatFriend(OwnerID, FriendId: int64; data: String): String;
begin
//  FTimerms := GetTickCount;
//  FTimerms := FTimerms + 1500 + Trunc(Random(1000));

  clHttpRequest2.Assign(clHttpRequest);
  clHttpRequest2.AddFormField('client_performance_stats', '{"first_request":380.784}');
  clHttpRequest2.AddFormField('avg_mem', GetBaseRandomStr(197341184, 100000));
  clHttpRequest2.AddFormField('version', GetNextUserStatVerStr);
  clHttpRequest2.AddFormField('user_id', FOwnerID);
  clHttpRequest2.AddFormField('room_id', IntToStr(FCurrRoomID));
  clHttpRequest2.AddFormField('time_pl', GetPlayedTime);
  clHttpRequest2.AddFormField('avg_fps', GetBaseRandomStr(10, 5));
  clHttpRequest2.AddFormField('serv_ver', IntToStr(FServerVer));
  clHttpRequest2.AddFormField('auth_key', FAuthKey);

  if FriendId <> 0 then
    clHttpRequest2.AddFormField('view_friend_id', IntToStr(FriendId));

  if OwnerID <> 0 then
  begin // their bug here!!!
    clHttpRequest2.AddFormField('owner_id', IntToStr(OwnerID));
    clHttpRequest2.AddFormField('owner_id', IntToStr(OwnerID));
  end;

  clHttpRequest2.AddFormField('session_key', FSessionKey);
  clHttpRequest2.AddFormField('revision', FRevision);
  clHttpRequest2.AddFormField('rand', GetRandomStr);

  FillFormData(clHttpRequest2, data);

  frn := frn + 1;

  SendPost(
    'http://88.212.222.164/city_server_vk_prod/get_user_stat?uid=56895991&crev=' +
    FRevision + '&fp=' + FVerFP + '&rn=' + IntToStr(Frn),
    clHttpRequest2,
    Fres);

  Result := Fres.Text;
end;

function TMPServer.GotRevisionTime: cardinal;
begin
  Result := GetTickCount - FGetRevisionTick;
end;

function TMPServer.LoadFile(URL, FileName: string): integer;
var
  mstream: TMemoryStream;
begin
  mstream := TMemoryStream.Create;
  try
    try
      clHttp.Get(URL, mstream);
      mstream.Position := 0;
      Result := mstream.Size;
      mstream.SaveToFile(FileName);
    finally
      mstream.Free;
    end;
  except
    Result := 0;
  end;
end;

function TMPServer.SendGet(vURL: string; vHttpRequest: TclHttpRequest;
  sl: TstringList): boolean;
var
 i: integer;
begin
  Result := false;
  for i := 0 to 1 do
  begin
    try
      clHttp.Get(vURL, vHttpRequest, sl);
      if clHttp.StatusCode = 410 then
      begin
        clHttp.Close;
        continue;
      end;

    except
      clHttp.Close;
      continue;
    end;

    Result := true;
    break;
  end;
end;

function TMPServer.SendPost(vURL: string; vHttpRequest: TclHttpRequest;
  sl: TstringList): boolean;
var
 i: integer;
begin
  Result := false;
  for i := 0 to 1 do
  begin
    try
      clHttp.Post(vURL, vHttpRequest, sl);
      if clHttp.StatusCode = 410 then
      begin
        clHttp.Close;
        continue;
      end;

    except
      clHttp.Close;
      continue;
    end;

    Result := true;
    break;
  end;
end;

function TMPServer.GetNextCheckPerformVerStr: string;
begin
  Result := IntToStr(GetNextCheckPerformVer);
end;

function TMPServer.GetNextUserStatVerStr: string;
begin
  Result := IntToStr(GetNextUserStatVer);
end;

{ TSrvWorld }

function TMWorldHelper.LoadBarn(node: IXMLNode): boolean;
var
 i: integer;
begin
  Result := false;
  if node = nil then exit;

  try
    SetLength(Barn, 0);
    for i := 0 to node.ChildNodes.Count - 1 do
    begin
      SetLength(barn, length(Barn) + 1);
      Barn[length(Barn) - 1].Load(node.ChildNodes[i]);
    end;

    Result := true;
  except
  end;
end;

function TMWorldHelper.LoadFriends(node: IXMLNode): boolean;
var
  i: integer;
begin
  Result := false;
  if node = nil then exit;

  try
    SetLength(Friends, 0);

    for i := 0 to node.ChildNodes.Count - 1 do
      if node.ChildNodes[i].LocalName = 'friend' then
      begin
        SetLength(friends, length(friends) + 1);
        friends[length(friends) - 1].Load(node.ChildNodes[i]);

        // dummy user delete
        if friends[length(friends) - 1].id <= 0 then
          SetLength(friends, length(friends) - 1);
      end;

    Result := true;
  except
  end;
end;

function TMWorldHelper.LoadFromXML(RoomID: integer; data: string): boolean;
var
 doc: IXMLDocument;
 root: IXMLNode;
 room: TMRoom;
begin
  Result := false;
  Valid := false;
  try
    room := AddRoom(RoomID);
    if room = nil then exit;

    // allways avaliable room 0
    if RoomID = 0 then room.Avaliable := true;

    // get document
    doc := LoadXMLData(data);
    if doc = nil then exit;

    // get user data
    root := doc.DocumentElement;
    if root = nil then exit;

    // if user disabled do nothing
    UserDisabled := VarToBoolDef(root.Attributes['disabled_user'], false);
    if UserDisabled then exit;

    // process header
    LastHeader.ProcessResponseHeader(root);
    if room.ID <> LastHeader.RoomID then exit;
    room.Header := LastHeader;

    // process data
    LoadFriends(root.ChildNodes.FindNode('friends'));
    LoadGifts(root.ChildNodes.FindNode('gifts'));
    LoadBarn(root.ChildNodes.FindNode('barn'));

    room.LoadFields(root.ChildNodes.FindNode('field'));

    LastUpdate := now;
    Valid :=
      (not UserDisabled) and
      (LastHeader.OwnerID > 0);
  except
  end;
end;

function TMWorldHelper.LoadGifts(node: IXMLNode): boolean;
var
  i: integer;
  node2: IXMLNode;
begin
  Result := false;
  if node = nil then exit;

  try
    node2 := node.ChildNodes.FindNode('received');
    if node2 <> nil then
    begin
      SetLength(RecvdGift, 0);
      for i := 0 to node2.ChildNodes.Count - 1 do
      begin
        SetLength(RecvdGift, length(RecvdGift) + 1);
        RecvdGift[length(RecvdGift) - 1].Load(node2.ChildNodes[i]);
      end;
    end;

    node2 := node.ChildNodes.FindNode('available');
    if node2 <> nil then
    begin
      SetLength(AvailGift, 0);
      for i := 0 to node2.ChildNodes.Count - 1 do
      begin
        SetLength(AvailGift, length(AvailGift) + 1);
        AvailGift[length(AvailGift) - 1].Load(node2.ChildNodes[i]);
      end;
    end;

    Result := true;
  except
  end;
end;

function TWorldHeaderHelper.ProcessResponseHeader(root: IXMLNode): boolean;
begin
  Result := false;
  Clear;
  if root = nil then exit;
  try
    OwnerID := VarToInt64Def(root.Attributes['owner_id'], 0);
    if OwnerID <= 0 then exit;

    NextTick := VarToIntDef(root.Attributes['next_tick'], 0);
    WishListStr := VarToStr(root.Attributes['wish_list']);
    Population := VarToIntDef(root.Attributes['population'], 0);
    MaxPopulation := VarToIntDef(root.Attributes['max_population'], 0);
    RoomsResources := VarToStr(root.Attributes['rooms_resources']);
    Fuel := VarToIntDef(root.Attributes['fuel'], 0);
    Gold := VarToIntDef(root.Attributes['gold'], 0);
    Coins := VarToIntDef(root.Attributes['coins'], 0);
    Tax := VarToIntDef(root.Attributes['tax'], 0);
    ServerTime := VarToIntDef(root.Attributes['server_time'], 0);
    SessionKey := VarToStr(root.Attributes['session_key']);
    Auto := VarToIntDef(root.Attributes['auto'], 0);
    Exp := VarToIntDef(root.Attributes['exp'], 0);
    RoomID := VarToIntDef(root.Attributes['room_id'], -1);
    Level := VarToIntDef(root.Attributes['level'], 0);
    Visitors := VarToStr(root.Attributes['visitors']);
    RoomInformation := VarToStr(root.Attributes['room_information']);
    ResourceOptions := VarToStr(root.Attributes['resource_options']);
    RespectLevel := VarToIntDef(root.Attributes['respect_level'], 0);

    PopulationMultiplier :=  1 - tax * 7 / 1500;
    ProcessRoomResources(RoomsResources, root);

    LastTick := GetTickCount;
    Result := true;
  except
  end;
end;

{ TMRoomHelper }

function TMRoomHelper.LoadFields(node: IXMLNode): boolean;
var
  i: integer;
  field: TMField;
  ser: cardinal;
begin
  Result := false;
  if node = nil then exit;
  if node.ChildNodes.Count < 1 then exit;
  try
    ser := StartFieldsUpdate;
    for i := 0 to node.ChildNodes.Count - 1 do
    begin
      field := GetField(VarToInt64Def(
        node.ChildNodes[i].Attributes['id'], 0));
      if field = nil then
      begin
        field := TFieldsFactory.GetInstance.GetField(
          LowerCase(VarToStr(node.ChildNodes[i].LocalName)));
        if field = nil then continue;
        AddField(field);
      end;
      field.Load(node.ChildNodes[i]);
      field.serial := ser;
    end;

    EndFieldsUpdate(ser);
    FieldsClearTick;

    Result := true;
  except
  end;
end;

{ TBarnRecHelper }

function TBarnRecHelper.Load(node: IXMLNode): boolean;
begin
  Result := false;
  try
    Clear;

    Name := LowerCase(node.LocalName);
    ID := VarToIntDef(node.Attributes['id'], 0);
    Qty := VarToIntDef(node.Attributes['quantity'], 0);

    // item details from DB
    GameItem := TItemsFactory.GetInstance.GetGameItem(Name);

    Result := true;
  except
  end;
end;

{ TGiftRecHelper }

function TGiftRecHelper.Load(node: IXMLNode): boolean;
begin
  Result := false;
  try
    Clear;

    Name := LowerCase(node.LocalName);
    ID := VarToInt64Def(node.Attributes['id'], 0);
    Qty := VarToIntDef(node.Attributes['quantity'], 0);
    FromUser := VarToInt64Def(node.Attributes['from_user_id'], 0);

    // item details from DB
    GameItem := TItemsFactory.GetInstance.GetGameItem(Name);
    Result := true;
  except
  end;
end;

{ TFriendRecHelper }

function TFriendRecHelper.Load(node: IXMLNode): boolean;
begin
  Result := false;
  try
    Clear;

    ID := VarToInt64Def(node.Attributes['id'], 0);
    Level := VarToIntDef(node.Attributes['level'], 0);
    WishList := VarToStr(node.Attributes['wish_list']);
    HelpItems := VarToStr(node.Attributes['help_items']);
    RoomInfo := VarToStr(node.Attributes['owner_room_info']);
    HaveGift := VarToBoolDef(node.Attributes['have_gift'], false);

    HelpPointsCnt := VarToIntDef(node.Attributes['help_points'], 0);

    // in seconds
    if VarIsNull(node.Attributes['next_visit_time_end']) then
      NextVisitDT := 0
    else
      NextVisitDT := Now + 1 / (24 * 60 * 60) *
          VarToIntDef(node.Attributes['next_visit_time_end'], 0);

    // from DB
    RewardPoints := TMPdatabase.GetInstance.GetRewardPoints(id);

    Result := true;
  except
  end;
end;

{ TMFieldHelper }

function TMFieldHelper.Load(node: IXMLNode): boolean;
var
  inum: integer;
begin
  Result := false;
  try
    Clear;

    Name := LowerCase(VarToStr(node.LocalName));
    ID := VarToInt64Def(node.Attributes['id'], 0);
    State := VarToIntDef(node.Attributes['state'], 0);
    x := VarToIntDef(node.Attributes['x'], 0);
    y := VarToIntDef(node.Attributes['y'], 0);
    ContractInput := VarToStr(node.Attributes['contract_input']);
    ContractOutput := VarToStr(node.Attributes['contract_output']);
    InputFill := VarToStr(node.Attributes['input_fill']);
    OutputFill := VarToStr(node.Attributes['output_fill']);
    Rotated := VarToBoolDef(node.Attributes['rotation'], false);
    BuildingPosition := VarToInt64Def(node.Attributes['building_position'], 0);
    ProcessEnd := VarToIntDef(node.Attributes['process_end'], 0);
    ExtraValue := VarToIntDef(node.Attributes['extra_value'], 0);

    GameItem := TItemsFactory.GetInstance.GetGameItem(Name);

    inum := StrToIntDef(ContractOutput, 0);
    if inum > 0 then
      ContractOutputItem :=
        TItemsFactory.GetInstance.GetGameItem(inum);
    isDeny := TMPdatabase.GetInstance.FieldIsDeny(Name);

    if Self is TMFieldFactory then
      with TMFieldFactory(Self) do
      begin
        PutKlass := TMPdatabase.GetInstance.GetExecContract(Name).Klass;
        PutGameItem := TItemsFactory.GetInstance.GetGameItem(PutKlass);
      end;

    if Self is TMFieldFactoryWithHelp then
      with TMFieldFactoryWithHelp(Self) do
      begin
        ExecContract := TMPdatabase.GetInstance.GetExecContract(Name);
      end;

    LastUpdate := Now;

    Result := true;
  except
  end;
end;

procedure TWorldHeaderHelper.ProcessRoomResources(data: string; root: IXMLNode);
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ',';
    sl.DelimitedText := data;
    SetLength(RoomResourcesArray, sl.Count);
    for i := 0 to sl.Count - 1 do
    begin
      RoomResourcesArray[i].Name := sl[i];
      RoomResourcesArray[i].Value := VarToStr(root.Attributes[sl[i]]);
    end;
  finally
    sl.Free;
  end;
end;

initialization
end.
