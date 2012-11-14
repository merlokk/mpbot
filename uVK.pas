unit uVK;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  clHttpRequest, clGZip, clCookies, clConnection, clMultiDC, clSingleDC,
  clDownLoader, clTcpClient, clHttp, IdHashMessageDigest, IdHash,
  Dialogs, StdCtrls, XMLIntf, XMLDoc, uDefs, uLogger;

type
  HlpVKFriendRec=record helper for VKFriendRec
    procedure FillFromXML(node: IXMLNode);
  end;

 TVKAPI = class
 private
   clCookieManager: TclCookieManager;
   clGZip: TclGZip;
   clHttpRequest,
   clHttpRequest2: TclHttpRequest;
   clHttp: TclHttp;

   FUserName,
   FPassword: string;

   FAppID,
   FUserID,
   FSecretKey: string;

   FSessionID: string;
   FSessionLen,
   FSessionTick: cardinal;
   FUserFirstName: string;

   FFriendList: array of VKFriendRec;
   FFriendListLoaded: boolean;

   FGameFriends: string;
   FGameFriendsCount: integer;

   function VKAuthenticate: boolean;
   procedure LoadFriendList;

   ///  from older version of API
   function Parse(_to, AFrom, to_: string): string;
   function md5(s: string): string;
   function SignRequest(const Request, mid, Secret: string): string;

   function URLGetParamValue(data, param: string): string;
 public
   constructor Create(AUserName, APassword: string);

   function VKGetProfiles(list: string): string;
   function VKGetUserFirstName(uid: int64): string;

   function GetAppFriends: String;
   function GetFriends: String;
   function GetFriendFirstName(id: Int64): string;
   function GetFriendsCount: integer;
   function UpdateFriendsDetails(var friends: TFriendRecArray): boolean;

   function Authenticated: boolean;
   function TryAuthenticate: boolean;

   property ApplicationID: string read FAppID write FAppID;
   property SecretKey: string read FSecretKey write FSecretKey;
   property UserID: string read FUserID write FUserID;
   property UserName: string read FUserName;
   property GameFriendsCount: integer read FGameFriendsCount;
   property UserFirstName: string read FUserFirstName;
 end;

implementation

{ TVKAPI }

// От и до...
function TVKAPI.Parse(_to, AFrom, to_: string): string;
begin
 If (Length(_to) = 0) or (Length(AFrom) = 0) or (Length(to_) = 0) Then Exit;

  Result := Copy(AFrom, Pos(_to, AFrom) + Length(_to), Length(AFrom));
  Delete(Result, Pos(to_, Result), Length(Result));
end;

{ Перевод в MD5 }
function TVKAPI.md5(s: string): string;
begin
Result := '';
  with TIdHashMessageDigest5.Create do
  try
    Result := LowerCase(HashStringAsHex(s));
  finally
    Free;
  end;
end;

{ Функция подписи запроса}
function TVKAPI.SignRequest(const Request, mid, Secret: string): string;
var
  sIn  : TStringList;
  sTmp : string;
  sCur : string;
  I    : Integer;
begin
  sIn := TStringList.Create;

  sTmp := Request;
  sTmp := StringReplace(sTmp, '%20', ' ', [rfReplaceAll]);
  sTmp := sTmp + '&';

  While (Pos('&', sTmp) <> 0) do
  begin
    sCur := Copy(sTmp, 1, Pos('&', sTmp) - 1);
    Delete(sTmp, 1, Pos('&', sTmp));
    sIn.Add(sCur);
  end;
   sIn.Sort;

 For I := 0 To sIn.Count -1 do
 begin
   Result := Result + sIn[i];
 end;
   Result := MD5(mid + result + secret);
   sIn.Free;
end;

function TVKAPI.TryAuthenticate: boolean;
begin
  Result := Authenticated;
  if not Result then
    Result := VKAuthenticate;
end;

function TVKAPI.UpdateFriendsDetails(var friends: TFriendRecArray): boolean;
var
 i,
 j: integer;
 fn,
 qry,
 rsp: string;

 doc: IXMLDocument;
 root,
 n: IXMLNode;

 uid: int64;
 firstname: string;
begin
  Result := false;
  try
    qry := '';
    for i := 0 to length(friends) - 1 do
    begin
      fn := GetFriendFirstName(friends[i].id);
      if fn <> '' then
        friends[i].rusname := fn
      else
        if friends[i].id > 0 then
          qry := qry + IntToStr(friends[i].id) + ',';
    end;

    if qry = '' then exit;
    qry := Copy(qry, 1, length(qry) - 1);

    rsp := VKGetProfiles(qry);
    if rsp = '' then exit;

    doc := LoadXMLData(rsp);
    root := doc.DocumentElement;

    for i := 0 to root.ChildNodes.Count - 1 do
      if root.ChildNodes[i].LocalName = 'user' then
      begin
        uid := 0;
        n := root.ChildNodes[i].ChildNodes.FindNode('uid');
        if n <> nil then uid := StrToIntDef(n.Text ,0);

        n := root.ChildNodes[i].ChildNodes.FindNode('first_name');
        if n <> nil then firstname := Trim(Utf8ToAnsi(RawByteString(n.Text)));

        for j := 0 to length(friends) - 1 do
          if friends[j].id = uid then
          begin
            friends[j].rusname := firstname;
            break;
          end;
      end;

    Result := true;
  except
  end;
end;

function TVKAPI.URLGetParamValue(data, param: string): string;
var
 s: string;
begin
  Result := '';
  if Pos(param + '=', data) <= 0  then exit;

  s := Copy(data, Pos(param + '=', data) + length(param) + 1, length(data));
  if Pos('&', s) > 0 then
    Result := Copy(s, 1, Pos('&', s) - 1)
  else
    Result := s;
end;

function TVKAPI.VKAuthenticate: boolean;
var
 i: integer;
 sl: TStringList;
 res,
 s: string;
begin
  Result := false;
  sl := TStringList.Create;

  for i := 0 to 1 do
  try
    clHTTP.Get(
      'https://vk.com/', sl);

    s := Copy(sl.Text, Pos('id="quick_login_form"', sl.Text) + 21, length(sl.Text));
    s := Copy(s, Pos('"ip_h"', s) + 6, length(s));
    s := Copy(s, Pos('"', s) + 1, length(s));
    s := Copy(s, 1, Pos('"', s) - 1);

    clHttpRequest2.Assign(clHttpRequest);
    clHttpRequest2.Header.ContentType := 'application/x-www-form-urlencoded';
    clHttpRequest2.AddFormField('act', 'login');
    clHttpRequest2.AddFormField('role', 'al_frame');
    clHttpRequest2.AddFormField('expire', '');
    clHttpRequest2.AddFormField('captcha_sid', '');
    clHttpRequest2.AddFormField('captcha_key', '');
    clHttpRequest2.AddFormField('_origin', 'https://vk.com');
    clHttpRequest2.AddFormField('ip_h', s);
    clHttpRequest2.AddFormField('email', FUserName);
    clHttpRequest2.AddFormField('pass', FPassword);

    clHTTP.Post(
      'https://login.vk.com/?act=login',
      clHttpRequest2,
      sl);

    res := '';
    if Pos('onlogindone', AnsiLowerCase(sl.Text)) > 0 then
    begin
    clHTTP.Get(
      'https://vk.com/feed', sl);

      clHTTP.Get(
      'https://vk.com/app' + FAppID,
      sl);

      if Pos('"access_token"', sl.Text) > 0 then
      begin
        s := Copy(sl.Text, Pos('"access_token"', sl.Text) + 15, length(sl.Text));
        s := Copy(s, Pos('"', s) + 1, length(s));
        s := Copy(s, 1, Pos('"', s) - 1);

        if length(s) > 10 then
        begin
          FSessionID := s;
          FSessionLen := 86400;
        end
        else
        begin
          clHTTP.Get(
              'https://api.vk.com/oauth/authorize?client_id=' + FAppID +
      //         '&scope=262151&redirect_uri=http://api.vkontakte.ru/blank.html&response_type=token',
               '&scope=friends&redirect_uri=http://api.vkontakte.ru/blank.html&response_type=token',
              sl);

          if Pos('function approve', sl.Text) > 0 then
          begin
            s := Copy(sl.Text, Pos('function approve', sl.Text) + 17, length(sl.Text));
            s := Copy(s, 1, Pos('}', s) - 1);
            s := Copy(s, Pos('"', s) + 1, length(s));
            s := Copy(s, 1, Pos('"', s) - 1);

            clHTTP.Get(s, sl);
          end;

          res := clHttp.Url.Extra;

          FSessionID := URLGetParamValue(res, 'access_token');
          FSessionLen := StrToIntDef(URLGetParamValue(res, 'expires_in'), 86400);
        end;
      end;
    end;

    Result := length(FSessionID) > 10;
    if Result then
    begin
      FSessionTick := GetTickCount;
      FUserFirstName := VKGetUserFirstName(StrToIntDef(FUserID, 0));
      break;
    end;
  except
  end;

  sl.Free;
  AddLog('VK authenticate token=' + FSessionID, 5);
end;

function TVKAPI.Authenticated: boolean;
begin
  if int64(GetTickCount) >
     int64(FSessionLen * 1000 + FSessionTick) - 600000
  then FSessionID := '';
  Result := FSessionID <> '';
end;

constructor TVKAPI.Create(AUserName, APassword: string);
begin
  inherited Create;

  FUserName := AUserName;
  FPassword := APassword;

  FAppID := '1858070'; //'2635396'; // mp '1858070'
  FSecretKey := 'ET8HhOAeuOF4nDfS7GnT';
  FUserID := '';
  FSessionID := '';
  FSessionTick := 0;
  FUserFirstName := '';

  FGameFriends := '';
  FGameFriendsCount := 0;
  FFriendListLoaded := false;

  clGZip := TclGZip.Create(Nil);

  clHttpRequest := TclHttpRequest.Create(Nil);
  clHttpRequest.Header.AcceptEncoding := 'gzip,deflate,sdch';
  clHttpRequest.Header.AcceptCharSet := 'windows-1251,utf-8;q=0.7,*;q=0.3';
  clHttpRequest.Header.AcceptLanguage := 'ru-RU,ru;q=0.8,en-US;q=0.6,en;q=0.4';
//  clHttpRequest.Header.Referer := 'http://cs11481.vkontakte.ru/u6148904/6884a96ab0d76c.zip';
  clHttpRequest.Header.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.835.186 Safari/535.1';

  clHttpRequest2 := TclHttpRequest.Create(Nil);
  clHttpRequest2.Assign(clHttpRequest);

  clHttp := TclHttp.Create(Nil);
  clHTTP.Request := clHttpRequest;

  clCookieManager := TclCookieManager.Create(Nil);
  clHttp.CookieManager := clCookieManager;

  AddLog('VK created', 5);
end;

function TVKAPI.GetAppFriends: String;
var
  sl: TStringList;
  i: integer;
  doc: IXMLDocument;
  root: IXMLNode;
begin
  Result := '';
  sl := TStringList.Create;

  if FGameFriends = '' then
  begin
    FGameFriends := '';
    FGameFriendsCount := 0;

    if not Authenticated then
      if not VKAuthenticate then exit;

    try
      clHTTP.Get(
        'https://api.vk.com/method/friends.getAppUsers.xml?' +
         'api_id=' + FAppID +
         '&access_token=' + FSessionID,
        sl);

      doc := LoadXMLData(sl.Text);
      root := doc.DocumentElement;

      for i := 0 to root.ChildNodes.Count - 1 do
        if root.ChildNodes[i].LocalName = 'uid' then
        begin
          FGameFriends := FGameFriends + root.ChildNodes[i].Text + ',';
          FGameFriendsCount := FGameFriendsCount + 1;
        end;

      FGameFriends := Copy(FGameFriends, 1, length(FGameFriends) - 1);

      //  game friends backup
      sl.Text := FGameFriends;
      sl.SaveToFile(ExtractFilePath(Application.ExeName) + 'gamefriends.txt');
    except
    end;
  end;

  Result := FGameFriends;
  if Result = '' then
  begin
    sl.LoadFromFile(ExtractFilePath(Application.ExeName) + 'gamefriends.txt');
    Result := sl.Text;
  end;

  sl.Free;
  AddLog('VK get app friends cnt=' + IntToStr(FGameFriendsCount), 5);
end;

function TVKAPI.GetFriendFirstName(id: Int64): string;
var
 i: integer;
begin
  Result := '';
  if not FFriendListLoaded then LoadFriendList;

  for i := 0 to length(FFriendList) - 1 do
    if FFriendList[i].id = id then
    begin
      Result := FFriendList[i].FirstName;
      exit;
    end;
end;

function TVKAPI.GetFriends: String;
begin
  Result := '';

  AddLog('VK get friends: empty function', 5);
end;

function TVKAPI.GetFriendsCount: integer;
begin
  Result := length(FFriendList);
end;

function TVKAPI.VKGetProfiles(list: string): string;
var
 sl: TStringList;
begin
  Result := '';
  if not Authenticated then
    if not VKAuthenticate then exit;

  sl := TStringList.Create;
  try
    clHTTP.Get(
      'https://api.vk.com/method/getProfiles.xml?uids=' + list +
       '&fields=sex&api_id=' + FAppID +
       '&access_token=' + FSessionID,
      sl);
    Result := sl.Text;
  except
  end;

  sl.Free;
  AddLog('VK get profiles len=' + IntToStr(length(Result)), 5);
end;

function TVKAPI.VKGetUserFirstName(uid: int64): string;
var
  i: integer;
  data: string;
  doc: IXMLDocument;
  root,
  n: IXMLNode;
begin
  Result := '';
  if uid <= 0 then exit;

  data := VKGetProfiles(IntToStr(uid));
  try
    doc := LoadXMLData(data);
    if doc = nil then exit;

    root := doc.DocumentElement;
    if root = nil then exit;

    for i := 0 to root.ChildNodes.Count - 1 do
      if root.ChildNodes[i].LocalName = 'user' then
      begin
        n := root.ChildNodes[i].ChildNodes.FindNode('uid');
        if n = nil then continue;
        if StrToIntDef(n.Text ,0) <> uid then continue;

        n := root.ChildNodes[i].ChildNodes.FindNode('first_name');
        if n <> nil then
        begin
          Result := Trim(Utf8ToAnsi(RawByteString(n.Text)));
          exit;
        end;
      end;
  except
  end;
end;

procedure TVKAPI.LoadFriendList;
var
  sl: TStringList;
  i: integer;
  doc: IXMLDocument;
  root: IXMLNode;
begin
  if FFriendListLoaded then exit;
  SetLength(FFriendList, 0);

  if not Authenticated then
    if not VKAuthenticate then exit;

  sl := TStringList.Create;

  clHTTP.Get(
    'https://api.vk.com/method/friends.get.xml?fields=sex&api_id=' + FAppID +
     '&uid=' + FUserID + '&access_token=' + FSessionID,
    sl);
//  Sig  := SignRequest(Req, FUserID, FSecretKey);

  doc := LoadXMLData(sl.Text);
  root := doc.DocumentElement;

  for i := 0 to root.ChildNodes.Count - 1 do
    if root.ChildNodes[i].LocalName = 'user' then
    begin
      SetLength(FFriendList, length(FFriendList) + 1);
      FFriendList[length(FFriendList) - 1].FillFromXML(root.ChildNodes[i]);
    end;

  sl.Free;
  FFriendListLoaded := true;
  AddLog('VK load friend list cnt=' + IntToStr(length(FFriendList)), 5);
end;

{    VK queries
http://api.vkontakte.ru/oauth/authorize?client_id=2642393&scope=friends&redirect_uri=http://api.vkontakte.ru/blank.html&response_type=token
megapolis
http://api.vkontakte.ru/oauth/authorize?client_id=1858070&scope=friends&redirect_uri=http://api.vkontakte.ru/blank.html&response_type=token

https://api.vkontakte.ru/oauth/access_token/?client_id=2635396&client_secret=ET8HhOAeuOF4nDfS7GnT&grant_type=client_credentials
https://api.vkontakte.ru/oauth/access_token/?client_id=2642393&client_secret=EVElc4fsXk35CNshnAcE&grant_type=client_credentials

https://api.vkontakte.ru/method/friends.get.xml?uid=56895991&access_token=4fa460ff4fa460ff4ff61517644f8c567bb4fa44fa460ff9ed597b4b38f0c2a

https://api.vkontakte.ru/method/getProfiles.xml?uids=6492&fields=sex


запрос к мегаполису от вконтакте
http://city-vk-prod.socialquantum.ru/city_server_vk_prod/assets/index.html?api_url=http://api.vkontakte.ru/api.php&
api_id=1858070&api_settings=262151&viewer_id=56895991&viewer_type=2&
sid=a185b057b8c510dc719954db1acab5ac1f3f05e8e1b99bf64e701d8d4846e7&secret=b1cc312a7f&access_token=61113fcf2cb26118622763d07862694c2ea62756271163ff1c5045fa626c39d&user_id=56895991&
group_id=0&is_app_user=1&auth_key=3b4fd2ca7f718dd8a07f28c4170673c1&language=0&parent_language=0&referrer=profile&lc_name=f181331f&hash=

}

{ HlpVKFriendRec }

procedure HlpVKFriendRec.FillFromXML(node: IXMLNode);
var
 n: IXMLNode;
begin
  n := node.ChildNodes.FindNode('uid');
  if n <> nil then id := StrToIntDef(n.Text ,0);

  n := node.ChildNodes.FindNode('first_name');
  if n <> nil then FirstName := Trim(Utf8ToAnsi(RawByteString(n.Text)));

  n := node.ChildNodes.FindNode('last_name');
  if n <> nil then LastName := Trim(Utf8ToAnsi(RawByteString(n.Text)));

  n := node.ChildNodes.FindNode('sex');
  if n <> nil then Sex := StrToIntDef(n.Text ,0);
end;

initialization
end.
