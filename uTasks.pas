unit uTasks;

interface
uses
  SysUtils, Variants, Types, Classes, IOUtils, Forms, XMLIntf, XMLDoc, StrUtils,
  Windows, DateUtils, ShellAPI, pFIBQuery,
  uMPserv, uVK, uDB, uGameItems, uLogger, uQueue, uFactories,
  uCalc, uDefs;

type
  TMTaskType = (ttBaseClass, ttInitDB, ttLoadSWF,
    ttInit, ttWorldUpdate, ttCurRoomWork, ttWorkDispatcher,
    ttWhishListUpdate, ttProcessGifts, ttFriendHelp,
    ttLast);
const
  StrMTaskType: array [ttBaseClass..ttLast] of string = (
    'BaseClass',
    'InitDB',
    'LoadSWF',
    'Init',
    'WorldUpdate',
    'CurRoomWork',
    'WorkDispatcher',
    'WhishListUpdate',
    'GiftSend',
    'FriendHelp',
    'last');
type
  TMTaskExecutor = class;

  TMTask = class
  private
    FTaskType: TMTaskType;

    FExecuteOnce: boolean;
    FExecuteInterval,
    FExecuteDeviation: TDateTime;

    FExecCount: integer;
    FNextExecution: TDateTime;

    FTaskExec: TMTaskExecutor;
    FVK: TVKAPI;
    FMPServ: TMPServer;
    FDB: TMPdatabase;
    FQu: TActionQueue;

    procedure SetTaskType(AType: TMTaskType);
    procedure SetExecutionInterval(Minutes: integer; DeviationPrc: integer = 10);
    procedure SetExecutionIntervalSec(Seconds: integer; DeviationPrc: integer = 10);
    procedure SetExecuteOnce;
    procedure PlanNextExecution;
    function GetNextExecution: TDateTime;
  public
    constructor Create; virtual;

    procedure SetObjects(ATaskExec: TMTaskExecutor; AVK: TVKAPI; AMPServ: TMPServer; ADB: TMPdatabase; AQu: TActionQueue);

    procedure Clear; virtual;
    procedure Execute(Force: boolean = false);
    procedure IntExecute; virtual;
    function canExecute: boolean;

    property TaskType: TMTaskType read FTaskType;
    property ExecCount: integer read FExecCount;
    property NextExecution: TDateTime read GetNextExecution;
  end;

  TMTaskInitDB = class (TMTask)
    constructor Create; override;
    procedure IntExecute; override;
  end;

  TMTaskLoadSWF = class (TMTask)
    constructor Create; override;
    procedure IntExecute; override;
  end;

  TMTaskInit = class (TMTask)
    constructor Create; override;
    procedure IntExecute; override;
  end;

  TMTaskWorldUpdate = class (TMTask)
    constructor Create; override;
    procedure IntExecute; override;
  end;

  TMTaskCurRoomWork = class (TMTask)
    constructor Create; override;
    procedure IntExecute; override;
  end;

  TMTaskWorkDispatcher = class (TMTask)
    constructor Create; override;
    procedure IntExecute; override;
  end;

  TMTaskWhishListUpdate = class (TMTask)
  private
    {TODO:  REFACTORING}
    FCurrentWl,
    FNeededWl: TIntegerDynArray;

    function GetListCount(vList: string; vID: integer): integer;

    function isNeededWlFull: boolean;
    procedure AddNeededWl(GameItemID: integer);
    procedure AddNeededWlByBuilding(world: TMWorld; field: TMField);
    function inList(lst: TIntegerDynArray; val: integer): boolean;

    procedure FillCurrentWl(data: string);
    procedure FillListFromPriorityBld(world: TMWorld);
    procedure FillListFromWorld(world: TMWorld);

    function GetWlStr: string;
  public
    constructor Create; override;
    procedure IntExecute; override;
  end;

  TMTaskProcessGifts = class (TMTask)
  private
    {TODO: перейти на faMassSendGift}
    procedure QueueAddGift(gift: TSendGiftRec);
  public
    constructor Create; override;
    procedure IntExecute; override;
  end;

  TMTaskFriendHelp = class (TMTask)
    constructor Create; override;
    procedure IntExecute; override;
  end;

  TMTaskFactory = class
  private
    FVK: TVKAPI;
    FTaskExec: TMTaskExecutor;

    FMTasks: array of TMTask;
    function IntGetTask(TaskType: TMTaskType): TMTask;
    function IntAddTask(TaskType: TMTaskType): TMTask;
  public
    constructor Create(AVK: TVKAPI);

    procedure Clear;
    function GetTask(TaskType: TMTaskType): TMTask;
    function GetNextTask: TMTask;

    property TaskExec: TMTaskExecutor read FTaskExec write FTaskExec;
  end;

  TMTaskExecutor = class
  private
    FVK: TVKAPI;

    FTaskFactory: TMTaskFactory;
  public
    constructor Create(AVK: TVKAPI);
    destructor Destroy; override;

    function AddTask(TaskType: TMTaskType): boolean;
    function ExecuteTask(TaskType: TMTaskType): boolean;
    function Execute: boolean;
    function GetNextExecution(TaskType: TMTaskType): TDateTime;
  end;

implementation

{ TMTaskFactory }

procedure TMTaskFactory.Clear;
var
  i: integer;
begin
  for i := 0 to length(FMTasks) - 1 do
    FMTasks[i].Free;
  SetLength(FMTasks, 0);
end;

constructor TMTaskFactory.Create;
begin
  inherited Create;

  FVK := AVK;
  FTaskExec := nil;
  SetLength(FMTasks, 0);
  Clear;
end;

function TMTaskFactory.GetNextTask: TMTask;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to length(FMTasks) - 1 do
  if FMTasks[i].canExecute then
    begin
      Result := FMTasks[i];
      break;
    end;
end;

function TMTaskFactory.GetTask(TaskType: TMTaskType): TMTask;
begin
  Result := IntGetTask(TaskType);
  if Result = nil then
    Result := IntAddTask(TaskType);
end;

function TMTaskFactory.IntAddTask(TaskType: TMTaskType): TMTask;
begin
  Result := nil;
  case TaskType of
    ttInitDB: Result := TMTaskInitDB.Create;
    ttLoadSWF: Result := TMTaskLoadSWF.Create;
    ttInit: Result := TMTaskInit.Create;
    ttWorldUpdate: Result := TMTaskWorldUpdate.Create;
    ttCurRoomWork: Result := TMTaskCurRoomWork.Create;
    ttWorkDispatcher: Result := TMTaskWorkDispatcher.Create;
    ttWhishListUpdate: Result := TMTaskWhishListUpdate.Create;
    ttProcessGifts: Result := TMTaskProcessGifts.Create;
    ttFriendHelp: Result := TMTaskFriendHelp.Create;
  else
  end;

  if Result <> nil then
  begin
    SetLength(FMTasks, length(FMTasks) + 1);
    FMTasks[length(FMTasks) - 1] := Result;

    Result.SetObjects(
      FTaskExec,
      FVK,
      TMPServer.GetInstance,
      TMPdatabase.GetInstance,
      TActionQueue.GetInstance);
    AddLog('Task added id=' + StrMTaskType[TaskType], 4);
  end;
end;

function TMTaskFactory.IntGetTask(TaskType: TMTaskType): TMTask;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to length(FMTasks) - 1 do
  if FMTasks[i].FTaskType = TaskType then
    begin
      Result := FMTasks[i];
      break;
    end;
end;

{ TMTaskExecutor }

function TMTaskExecutor.AddTask(TaskType: TMTaskType): boolean;
begin
  Result :=FTaskFactory.GetTask(TaskType) <> nil;
end;

constructor TMTaskExecutor.Create;
begin
  inherited Create;

  FVK := AVK;
  FTaskFactory := TMTaskFactory.Create(FVK);
  FTaskFactory.TaskExec := Self;
end;

destructor TMTaskExecutor.Destroy;
begin
  FTaskFactory.Free;
  inherited;
end;

function TMTaskExecutor.Execute: boolean;
var
  task: TMTask;
begin
  Result := true;
  task := FTaskFactory.GetNextTask;
  if task = nil then exit;
  task.Execute;
end;

function TMTaskExecutor.ExecuteTask(TaskType: TMTaskType): boolean;
var
  task: TMTask;
begin
  task := FTaskFactory.GetTask(TaskType);

  if task <> nil then
  begin
    task.Execute(true);
    Result := true;
  end
  else
  begin
    Result := false;
  end;
end;

function TMTaskExecutor.GetNextExecution(TaskType: TMTaskType): TDateTime;
var
  task: TMTask;
begin
  Result := 0;
  task := FTaskFactory.GetTask(TaskType);

  if task <> nil then
    Result := task.NextExecution;
end;

{ TMTask }

function TMTask.canExecute: boolean;
begin
  Result :=  ((FExecuteOnce) and (FExecCount = 0)) or
              ((not FExecuteOnce) and (FNextExecution < now));
end;

procedure TMTask.Clear;
begin
  FExecCount := 0;
  SetExecuteOnce;
end;

constructor TMTask.Create;
begin
  inherited;
  SetTaskType(ttBaseClass);
  Clear;

  FTaskExec := nil;
  FDB := nil;
  FVK := nil;
  FMPServ := nil;
  FQu := nil;
end;

procedure TMTask.Execute(Force: boolean = false);
begin
  if Force or canExecute then
  try
    FExecCount := FExecCount + 1;
    PlanNextExecution;

    IntExecute;
  except
  end;
end;

function TMTask.GetNextExecution: TDateTime;
begin
  Result := FNextExecution;
  if FExecuteOnce then
  begin
    if FExecCount = 0 then
      Result := Now
    else
      Result := 0;
  end;
end;

procedure TMTask.IntExecute;
begin

end;

procedure TMTask.PlanNextExecution;
begin
  if FExecuteOnce then
  begin
    FNextExecution := 0;
  end
  else
  begin
    FNextExecution :=
      now + FExecuteInterval + FExecuteDeviation * Random;
  end;
end;

procedure TMTask.SetExecutionInterval(Minutes, DeviationPrc: integer);
begin
  SetExecutionIntervalSec(Minutes * 60, DeviationPrc);
end;

procedure TMTask.SetExecutionIntervalSec(Seconds, DeviationPrc: integer);
begin
  FExecuteOnce := false;
  FNextExecution := 0;
  FExecuteInterval := Seconds / SecsPerDay;
  FExecuteDeviation := FExecuteInterval * (DeviationPrc / 100);
end;

procedure TMTask.SetExecuteOnce;
begin
  FExecuteOnce := true;
  FExecuteInterval := 0;
  FExecuteDeviation := 0;
  FNextExecution := 0;
end;

procedure TMTask.SetObjects(ATaskExec: TMTaskExecutor; AVK: TVKAPI; AMPServ: TMPServer; ADB: TMPdatabase; AQu: TActionQueue);
begin
  FTaskExec := ATaskExec;
  FVK := AVK;
  FMPServ := AMPServ;
  FDB := ADB;
  FQu:= AQu;
end;

procedure TMTask.SetTaskType(AType: TMTaskType);
begin
  FTaskType := AType;
end;

{ TMTaskInitDB }

constructor TMTaskInitDB.Create;
begin
  inherited;
  SetTaskType(ttInitDB);
end;

function FindField(sl: TStringList; ident: String):String;
var
 i: integer;
 s: string;
begin
  Result := '';
  ident := ' _' + ident + ':';

  for i := 0 to sl.Count - 1 do
    if ((Pos('const', sl[i]) > 0) or
        (Pos('var', sl[i]) > 0)) and
       (Pos('=', sl[i]) > 0) and
       (Pos(ident, sl[i]) > 0) and
       (Pos(ident, sl[i]) < Pos('=', sl[i]))
    then
    begin
      s := Copy(sl[i], Pos('const', sl[i]), length(sl[i]));
      s := Copy(s, Pos(ident, s), length(s));
      s := Copy(s, Pos('=', s) + 1, length(s));
      s := Copy(s, 1, Pos(';', s) - 1);
      s := Trim(s);
      if (s <> '') and (s[1] = '"') then
        s := Copy(s, 2, length(s) - 2);

      Result := s;
      break;
    end;
end;

function TryToFindFieldS(data, ident: String):string;
var
 s: string;
begin
  Result := '';
  ident := ' _' + ident + ':';

  if (Pos(ident, data) > 0) and
     (Pos(ident, data) < Pos('=', data))
  then
  begin
    s := Copy(data, Pos('const', data), length(data));
    s := Copy(s, Pos(ident, s), length(s));
    s := Copy(s, Pos('=', s) + 1, length(s));
    s := Copy(s, 1, Pos(';', s) - 1);
    s := Trim(s);
    if (s <> '') and (s[1] = '"') then
      s := Copy(s, 2, length(s) - 2);

    Result := s;
  end;
end;

function TryToFindFieldI(data, ident: String; default: integer = 0):integer;
begin
  Result := StrToIntDef(TryToFindFieldS(data, ident), default);
end;

procedure TMTaskInitDB.IntExecute;
var
  sl,
  res: TStringList;
  i,
  j,
  k,
  AllItemsCnt,
  DBItemsCnt: integer;
  s: string;
  dir: TStringDynArray;
  GItem: TMGameItem;
  Attrs: TAttrs;
  Attr: TAttr;

  dbs: TMPdatabase;
  mpsrv: TMPServer;

  doc: IXMLDocument;
  root,
  items: IXMLNode;
begin
  AllItemsCnt := 0;
  DBItemsCnt := 0;

  mpsrv := TMPServer.GetInstance;
  AddLog('init db started...');
  dbs := TMPdatabase.GetInstance;
  dbs.Connect;
  if dbs.Connected then
    AddLog('connected to DB.');

  mpsrv.GetRevision(TMWorld.GetInstance);

  AddLog('load translation from server...');

  res := TStringList.Create;
  mpsrv.GetCityXML(res);
  AddLog('loaded city.xml ' + IntToStr(length(res.Text)) + ' bytes.');

  AddLogFile('!server', 'city.xml', res);
  doc := LoadXMLData(ReplaceStr(res.Text, '&', ' '));
  root := doc.DocumentElement;
  items := root.ChildNodes.FindNode('items');
  if items = nil then
  begin
   AddLog('cant find <item> element in translation');
   exit;
  end;

  Attrs := dbs.GetItemAttributes;

  sl := TStringList.Create;
  try
    dbs.ClearGameItemsDB;
    AddLog('gift db cleared.');

    dir := TDirectory.GetFiles(ExtractFilePath(Application.ExeName) + 'item\');
    for i := 0 to Length(dir) - 1 do
    try
      sl.LoadFromFile(dir[i]);

      GItem := TMGameItem.Create;
      AllItemsCnt := AllItemsCnt + 1;

      for j := 0 to sl.Count - 1 do
        if ((Pos('const', sl[j]) > 0) or
            (Pos('var', sl[j]) > 0)) and
           (Pos('=', sl[j]) > 0) then
        begin
          if GItem.ID = 0 then
            GItem.ID := TryToFindFieldI(sl[j], 'id');
          if GItem.Name = '' then
            GItem.Name := TryToFindFieldS(sl[j], 'item_name');
          if GItem.ShopDept = '' then
            GItem.ShopDept :=  TryToFindFieldS(sl[j], 'shop_department');
          if GItem.SuperClass = '' then
            GItem.SuperClass := TryToFindFieldS(sl[j], 'super_class');

          for k := 0 to length(Attrs) - 1 do
          begin
            s := TryToFindFieldS(sl[j], Attrs[k].Name);
            if s = '' then continue;
            Attr := Attrs[k];
            Attr.StrSetVal(s);
            GItem.AddAttr(Attr);
          end;
        end;

      GItem.RusName := '';
      for j := 0 to items.ChildNodes.Count - 1 do
        if items.ChildNodes[j].Attributes['name'] = GItem.Name + ':description' then
        begin
          GItem.RusName := items.ChildNodes[j].Text;
          GItem.RusName := ReplaceStr(GItem.RusName, '\"', '"');
          GItem.RusName := ReplaceStr(GItem.RusName, '''', '"');
          GItem.RusName := Utf8ToAnsi(RawByteString(GItem.RusName));
          break;
        end;

(*      GItem.XP := StrToIntDef(FindField(sl, ' _exp'), 0);
      GItem.ExtraXP := StrToIntDef(FindField(sl, ' _extra_exp'), 0);
      GItem.canPick := FindField(sl, ' _pick') = 'true';
      GItem.canClean := FindField(sl, ' _clean') = 'true';
      GItem.canPut := FindField(sl, ' _put') = 'true';
      GItem.canShed := FindField(sl, ' _shed') = 'true';
      GItem.canMove := FindField(sl, ' _move') = 'true';
      GItem.hasWallImage := FindField(sl, ' _has_wall_image') = 'true';
      GItem.isBuildSite := FindField(sl, ' _buildsite') = 'true';
      GItem.isPseudoItem := FindField(sl, ' _pseudo_item') = 'true';
      GItem.Height := StrToIntDef(FindField(sl, ' _height'), 0);
      GItem.Width := StrToIntDef(FindField(sl, ' _width'), 0);
      GItem.BuildableHelp := FindField(sl, ' _buildable_help') = 'true';
      GItem.Actions := StrToIntDef(FindField(sl, ' _actions'), 0);
      GItem.Radius := StrToIntDef(FindField(sl, ' _radius'), 0);
      GItem.Effect := StrToFloatDef(
        ReplaceStr(FindField(sl, ' _effect'), '.', ','), 0);

      GItem.ContractsStr := FindField(sl, ' _contracts_ary');
      if Pos('[', GItem.ContractsStr) <= 0 then
        GItem.ContractsStr := '';
      GItem.ContractsStr := ReplaceStr(GItem.ContractsStr, '[', '');
      GItem.ContractsStr := ReplaceStr(GItem.ContractsStr, ']', '');
      GItem.ContractsStr := Trim(GItem.ContractsStr);

      GItem.BldMaterialsQty := FindField(sl, ' _materials_quantity_obj');
      GItem.BldMaterialsQty := ReplaceStr(GItem.BldMaterialsQty, '{', '');
      GItem.BldMaterialsQty := ReplaceStr(GItem.BldMaterialsQty, '}', '');
      GItem.BldMaterialsQty := Trim(GItem.BldMaterialsQty);

      GItem.AccessibleRoom := FindField(sl, ' _accessible_room');
      GItem.AccessibleRoom := ReplaceStr(GItem.AccessibleRoom, '[', '');
      GItem.AccessibleRoom := ReplaceStr(GItem.AccessibleRoom, ']', '');
      GItem.AccessibleRoom := Trim(GItem.AccessibleRoom);

      GItem.canHelp := FindField(sl, ' _help') = 'true';
      GItem.buffPopulation := FindField(sl, ' _buff_population') = 'true';
      GItem.buffAccelerate := FindField(sl, ' _buff_accelerate') = 'true';
      GItem.buffMobile := FindField(sl, ' _buff_mobile') = 'true';
      GItem.usesExtraOutput := FindField(sl, ' _uses_extra_output') = 'true';
      GItem.isExtendedFactory := FindField(sl, ' _extended_factory') = 'true';
      GItem.ExtraParams := FindField(sl, ' _extra_params');
      GItem.RequireOption := FindField(sl, ' _require_option');
      GItem.Require := FindField(sl, ' _require:Array');
      GItem.Require := ReplaceStr(GItem.Require, '[', '');
      GItem.Require := ReplaceStr(GItem.Require, ']', '');
      GItem.AllStates := FindLine(sl, 'this._all_states');
      GItem.SWFDirectory := FindField(sl, ' _swf_directory');
      GItem.Produce := FindField(sl, ' _produce:');
      if Pos('Array()', GItem.Produce) > 0 then
        GItem.Produce := '';

      GItem.PopulationInc := StrToIntDef(FindField(sl, ' _population_increase'), 0);
      GItem.MaxPopulationInc := StrToIntDef(FindField(sl, ' _max_population_increase'), 0);

*)
      if GItem.ID <> 0 then
        if not dbs.AddGameItem(GItem) then
          AddLog('error adding item to db: ' + GItem.Name +
          '(' + IntToStr(GItem.ID) + ') file="' + dir[i] +'"')
        else
          DBItemsCnt := DBItemsCnt + 1;

      sl.Clear;
    except
     on e: Exception do
       if length(dir) > i then
         AddLog('Error in file "' + dir[i] + '"  ' + e.Message)
       else
         AddLog('Processing error: ' + e.Message)
    end;
  except
    on e: Exception do
      AddLog('Processing fatal error: ' + e.Message)
  end;

  dbs.Commit;
  sl.Free;

  AddLog('completed. AllFiles=' + IntToStr(AllItemsCnt) +
    ' DBGameItems=' + IntToStr(DBItemsCnt));
  res.Free;
end;

{ TMTaskInit }

constructor TMTaskInit.Create;
begin
  inherited;
  SetTaskType(ttInit);
end;

procedure TMTaskInit.IntExecute;
begin
  inherited;

  TActionQueue.GetInstance.OwnerID := StrToIntDef(FVK.UserID, 0);

  AddLog('init vk api');
  FVK.TryAuthenticate;
  FVK.GetFriendFirstName(StrToIntDef(FVK.UserID, 0));
  if not FVK.Authenticated then AddLog('vk not authorized!!!');

  FDB.Connect;

  FMPServ.AppFriends := FVK.GetAppFriends;
end;

{ TMTaskWorldUpdate }

constructor TMTaskWorldUpdate.Create;
begin
  inherited;
  SetTaskType(ttWorldUpdate);
  SetExecutionInterval(50);
end;

procedure TMTaskWorldUpdate.IntExecute;
var
  world: TMWorld;
  room,
  room0: TMRoom;
  i: Integer;
  fr: TFriendRec;
begin
  inherited;

  world := TMWorld.GetInstance;
  FMPServ.GetRevision(world);

  sleep(500);
  // first request to MP server
  FMPServ.GetUserStat(world, 0, true);
  room0 := world.GetRoom(0);

  for i := 1 to world.GetRoomCount - 1 do
  begin
    room := world.GetRoom(i);
    if (room = nil) or (not room.Avaliable) then continue;

    sleep(2000);
    FMPServ.GetUserStat(world, i);
  end;

  // update friends
  FVK.UpdateFriendsDetails(world.Friends);
  FDB.FriendsUpdate(world.Friends);
  FDB.FillGameFriends(FMPServ.AppFriends);

  // update owner in DB (for wishlist and other things)
  fr.id := world.OwnerID;
  fr.level := world.LastHeader.Level;
  fr.wishlist := world.LastHeader.WishListStr;
  fr.RewardPoints := 0;
  FDB.FriendUpdate(fr);
  FDB.Commit;

  // update gifts
  FDB.RecvdGiftsUpdate(world.RecvdGift);
  FDB.AvailGiftsUpdate(world.AvailGift);
  FDB.FillGiftsScore(world.AvailGift);
  FDB.CalcRewardPoints(world.OwnerID);

  // add statistic
  if world.Valid and (room0 <> nil) then
  begin
    AddLog(
      'valid=true' +
      ' owner(' + IntToStr(room0.Header.Level) + '):' + IntToStr(room0.Header.OwnerID) +
      ' room cnt=' + IntToStr(world.GetRoomCount) +
      ' friends=' + IntToStr(length(world.Friends)) +
      ' barn=' + IntToStr(length(world.Barn)) +
      ' money=' + IntToStr(world.LastHeader.Gold) + '/' +
         IntToStr(world.LastHeader.Coins) +
      ' xp='  + IntToStr(world.LastHeader.Exp)
      );

    AddLog('today used gifts count: ' + IntToStr(FDB.GetTodayUsedGiftsCnt));
    if length(world.AvailGift) > 0 then
      AddLog('gifts ' + world.StrGiftStat);

    for i := 0 to world.GetRoomCount - 1 do
    begin
      room := world.GetRoom(i);
      if (room = nil) or (not room.Avaliable) then continue;

      AddLog('ID=' + IntToStr(i) +
        ' avail=' + BoolToStr(room.Avaliable, true) +
        ' exp=' + IntToStr(room.Header.Exp) +
        ' ppl=' + IntToStr(room.Header.GetPopulation) +
                  '(' + IntToStr(room.Header.GetFreePopulation) + ')' +
        ' tax=' + IntToStr(room.Header.Tax) +
        ' fields=' + IntToStr(room.FieldsCount)
        );
      AddLog('fields(' + IntToStr(i) + ') ' + room.StrFieldsStat);
    end;
  end
  else
    AddLog('invalid room data');

  //  execute tasks after got valid world info
  if world.Valid then
  begin
    FTaskExec.ExecuteTask(ttWhishListUpdate);

    // if we have gifts ---> send them)
    if length(world.AvailGift) > 0 then
      FTaskExec.ExecuteTask(ttProcessGifts);
  end;
end;

{ TMTaskLoadSWF }

constructor TMTaskLoadSWF.Create;
begin
  inherited;
  SetTaskType(ttLoadSWF);
end;

procedure TMTaskLoadSWF.IntExecute;
var
  res: integer;
  mpsrv: TMPServer;
  world: TMWorld;
  FileName: string;
begin
  inherited;

  mpsrv := TMPServer.GetInstance;
  AddLog('load SWF started...');

  world := TMWorld.GetInstance;
  mpsrv.GetRevision(world);

  if world.SWFRevisionAppID = '' then
  begin
    AddLog('cant get SWFAppID. exit.');
    exit;
  end;

  FileName := ExtractFilePath(Application.ExeName) +
    world.SWFRevisionAppID + '.swf';
  if FileExists(FileName) then
  begin
    SysUtils.DeleteFile(FileName);
    AddLog('file "' + FileName +'" deleted...');
  end;

  AddLog('loading SWF from server (' + world.SWFRevisionAppID + '.swf' + ')...');

  res := mpsrv.LoadFile(
    'http://mb.static.socialquantum.ru/assets_vk_city_prod/app.swf?' +
    'rev=' + world.SWFRevisionAppID + '.swf',
    FileName);

  AddLog('completed. length(bytes): ' + IntToStr(res));
  ShellExecute(
    0,
    'open',
    'explorer.exe',
    PChar('/select, "' + FileName + '"'),
    nil,
    SW_SHOWNORMAL);
end;

{ TMTaskCurRoomWork }

constructor TMTaskCurRoomWork.Create;
begin
  inherited;
  SetTaskType(ttCurRoomWork);
end;

procedure TMTaskCurRoomWork.IntExecute;
var
  world: TMWorld;
  room,
  roomn: TMRoom;
  cnt: integer;
  i: Integer;
  cWork1m: Integer;
  nWork10m: Integer;
  cWork10m: Integer;
  nWork: Integer;
begin
  inherited;

  world := TMWorld.GetInstance;
  if (world = nil) or (not world.Valid) then exit;
  if world.LastUpdate + 10 * OneSecond > Now then exit;

  room := world.GetRoom(FMPServ.CurrRoomID);
  if room = nil then exit;

  // ---- room tick
  //  ticks more then 30  or
  //  we have "old" ticks - older then 40 seconds   or
  //  we have no more ticks in 22 seconds
  if (room.FieldsExecuteCount(true, false, 0, Now - 5 * OneSecond) > 30) or
     (room.FieldsExecuteCount(true, false, 0, Now - 40 * OneSecond) > 0) or
     (room.FieldsExecuteCount(true, false, Now - 5 * OneSecond, Now + 22 * OneSecond) <= 0)
  then
  begin
    FQu.Clear;
    FQu.CurrentXP := world.LastHeader.Exp;
    room.FieldsExecute(80 + random(35), true, false);
    if FQu.Count > 0 then
    begin
      FMPServ.CheckAndPerform(world, FQu);

      AddLog('(' + FQu.GetItemsLog + ')', 9);
      FQu.Clear;
      // if we put ticks to server - end of the task!!!
      exit;
    end;
  end;

  // ---- room work
  // work items more then 30  or
  // work items more then tick items in 60 seconds * 3
  cnt := room.FieldsExecuteCount(false, true, 0, Now - 10 * OneSecond);
  if ((cnt > 30) or
      (cnt > 3 * room.FieldsExecuteCount(true, false, 0, Now + 60 * OneSecond))
     )
  then
  begin
    FQu.Clear;
    FQu.CurrentXP := world.LastHeader.Exp;
    room.FieldsExecute(15 + random(20), false, true);
    if FQu.Count > 0 then
    begin
      FMPServ.CheckAndPerform(world, FQu);

      AddLog('(' + FQu.GetItemsLog + ')', 9);
      FQu.Clear;
      // if we put work to server - end of the task!!!
      exit;
    end;
  end;

  // ---- room switching
  // 1. last switching was at least 2 miutes ago
  // 2. current room work  up to 60 seconds in future =0
  // 3a. next room work > current room work * 1.3 in period of 10 min
  // 3b. next room work count (- 10 sec) more, then 10
  for i := 0 to world.GetRoomCount - 1 do
  try
    roomn := world.GetRoom(i);
    if roomn = nil then continue;

    cWork1m := room.FieldsExecuteCount(true, true, 0, Now + 60 * OneSecond);
    nWork10m := roomn.FieldsExecuteCount(true, true, 0, Now + 10 * OneMinute);
    cWork10m := room.FieldsExecuteCount(true, true, 0, Now + 10 * OneMinute);
    nWork := roomn.FieldsExecuteCount(true, true, 0, Now - 10 * OneSecond);

    if (i <> room.ID) and
       (world.LastRoomChange + 2 * OneMinute < Now) and
       (cWork1m <= 0) and
       ( (nWork10m > cWork10m * 1.3 ) or
         (nWork > 10)
       )
    then
    begin
      AddLog('NEED SWITCH:' +
        IntToStr(room.ID) + '->' + IntToStr(i) +
        ' cWork60:' + IntToStr(cWork1m) +
        ' nWork10m:' + IntToStr(nWork10m) +
        ' cWork10m:' + IntToStr(cWork10m) +
        ' nWork:' + IntToStr(nWork)
        );
      FMPServ.GetUserStat(world, i);

      // if we changed room - end of the task!!!
      exit;
    end;
  except
    exit;
  end;
end;

{ TMTaskWorkDispatcher }

constructor TMTaskWorkDispatcher.Create;
begin
  inherited;
  SetTaskType(ttWorkDispatcher);
  SetExecutionIntervalSec(10);
end;

procedure TMTaskWorkDispatcher.IntExecute;
var
  world: TMWorld;
  room: TMRoom;
begin
  inherited;

  world := TMWorld.GetInstance;
  if (world = nil) or (not world.Valid) then exit;

  room := world.GetRoom(FMPServ.CurrRoomID);
  if room <> nil then
  begin
    FTaskExec.ExecuteTask(ttCurRoomWork);

    if room.FieldsExecuteCount(true, true, 0, Now + 10 * OneMinute) = 0 then
    begin
      FTaskExec.ExecuteTask(ttFriendHelp);
      exit;
    end;
  end;
end;

{ TMTaskWhishListUpdate }

procedure TMTaskWhishListUpdate.AddNeededWl(GameItemID: integer);
var
 i: integer;
begin
  if isNeededWlFull or (GameItemID <= 0) then exit;

  for i := 0 to length(FNeededWl) - 1 do
    if FNeededWl[i] = GameItemID then exit;

  SetLength(FNeededWl, length(FNeededWl) + 1);
  FNeededWl[length(FNeededWl) - 1] := GameItemID;
end;

procedure TMTaskWhishListUpdate.AddNeededWlByBuilding(world: TMWorld; field: TMField);
var
  i,
  cnt: integer;
  sl: TStringList;
  gi: TMGameItem;
begin
  sl := TStringList.Create;
  sl.NameValueSeparator := ':';
  sl.Delimiter := ',';
  sl.DelimitedText := field.GameItem.MaterialQty;

  for i := 0 to sl.Count - 1 do
  begin
    gi := TItemsFactory.GetInstance.GetGameItem(sl.Names[i]);
    if (gi = nil) or
       (gi.ID <= 0) or
       (gi.ShopDept <> 'materials')
    then continue;

    cnt := StrToIntDef(sl.ValueFromIndex[i], 1) -
           GetListCount(field.InputFill, gi.ID) -
           world.GetAvailGiftCount(gi.ID);

    if cnt > 0 then AddNeededWl(gi.ID);
  end;

  sl.Free;
end;

constructor TMTaskWhishListUpdate.Create;
begin
  inherited;
  SetTaskType(ttWhishListUpdate);

  SetLength(FCurrentWl, 0);
  SetLength(FNeededWl, 0);
end;

procedure TMTaskWhishListUpdate.FillCurrentWl(data: string);
var
 sl: TStringList;
 i: integer;
begin
  SetLength(FCurrentWl, 0);

  sl := TStringList.Create;
  sl.Delimiter := ',';
  sl.DelimitedText := data;

  for i := 0 to sl.Count - 1 do
  begin
    SetLength(FCurrentWl, length(FCurrentWl) + 1);
    FCurrentWl[length(FCurrentWl) - 1] := StrToIntDef(sl[i], 0);
  end;

  sl.Free;
end;

procedure TMTaskWhishListUpdate.FillListFromPriorityBld(world: TMWorld);
var
  i,
  j,
  r: Integer;
  room: TMRoom;
  field: TMField;

  FPriorityBldList: TStringDynArray;
begin
  FPriorityBldList := FDB.GetPriorityBuildList;

  for i := 0 to length(FPriorityBldList) - 1 do
    for r := 0 to world.GetRoomCount - 1 do
    begin
      room := world.GetRoom(r);
      if room = nil then continue;

      for j := 0 to room.FieldsCount - 1 do
      begin
        field := room.GetFieldI(j);
        if field = nil then continue;

        if Pos(FPriorityBldList[i], field.Name) = 1 then
        begin
          AddNeededWlByBuilding(world, field);

          if isNeededWlFull then exit
        end;
      end;
    end;
end;

procedure TMTaskWhishListUpdate.FillListFromWorld(world: TMWorld);
var
  i,
  r: Integer;
  room: TMRoom;
  field: TMField;
  wl: TWishListCalc;
begin
  wl := TWishListCalc.Create;

  // get all fields materials info
  for r := 0 to world.GetRoomCount - 1 do
  begin
    room := world.GetRoom(r);
    if room = nil then continue;

    for i := 0 to room.FieldsCount - 1 do
    begin
      field := room.GetFieldI(i);
      if (field = nil) or (field.GameItem = nil) then continue;
      if (field.GameItem.isBuildSite) and
         (field.GameItem.MaterialQty <> '') and
         //  bugs from database
{         ((field.GameItem.GetAttr('produce').AsString <> '') or
          (field.InputFill <> ''))} true
      then
      begin
        wl.AddNeedList(field.GameItem.MaterialQty);
        wl.AddCurrList(field.InputFill);
      end;
    end;
  end;

  wl.FillGiftsCount(world);
  wl.SortByBalance;

  //  fill wishlist
  if not isNeededWlFull then
  begin
    for i := 0 to wl.Count - 1 do
    begin
      if wl.WList[i].GetBalance > 0 then
        AddNeededWl(wl.WList[i].ID);
      if isNeededWlFull then break;
    end;
  end;

  AddLog(wl.GetStat);
  wl.Free;
end;

function TMTaskWhishListUpdate.GetListCount(vList: string; vID: integer): integer;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.NameValueSeparator := ':';
  sl.Delimiter := ',';
  sl.DelimitedText := vList;

  Result := StrToIntDef(sl.Values[IntToStr(vID)], 0);

  sl.Free;
end;

function TMTaskWhishListUpdate.GetWlStr: string;
var
 i: integer;
 gi: TMGameItem;
begin
  Result := '';
  for i := 0 to length(FNeededWl) - 1 do
  begin
    gi := TItemsFactory.GetInstance.GetGameItem(FNeededWl[i]);
    if gi <> nil then
      Result := Result + gi.RusName + ', '
    else
      Result := Result + 'nil, '
  end;
  Result := Copy(Result, 1, length(Result) - 2);
end;

function TMTaskWhishListUpdate.inList(lst: TIntegerDynArray;
  val: integer): boolean;
var
 i: integer;
begin
  Result := false;
  for i := 0 to length(lst) - 1 do
    if lst[i] = val then
    begin
      Result := true;
      break;
    end;
end;

procedure TMTaskWhishListUpdate.IntExecute;
var
  world: TMWorld;
  i: integer;
begin
  inherited;

  world := TMWorld.GetInstance;
  if (world = nil) or (not world.Valid) then exit;

  // clear needed wish list
  SetLength(FNeededWl, 0);
  // fill current wish list
  FillCurrentWl(world.LastHeader.WishListStr);

  // fill list by needed gifts
{  if (NeedGifts <> '') and
     (NeedGiftsCount > 0) and
     (st.GetGiftCntByClassId(db.GeItemClassId(NeedGifts)) < NeedGiftsCount)
  then
    AddNeededWl(db.GeItemClassId(NeedGifts));
}

  // fill wish list by priority
  FillListFromPriorityBld(world);
  //  need gifts and used(current) gifts
  FillListFromWorld(world);

  // logging
  AddLog('calc wl: ' + GetWlStr, 5);

  // calc commands
  FQu.Clear;
  FQu.CurrentXP := world.LastHeader.Exp;
  //  FMPServ.CurrRoomID ---- небольшое отклонение от нормального клиента
  for i := 0 to length(FCurrentWl) - 1 do
    if not inList(FNeededWl, FCurrentWl[i]) then
      FQu.Add(FMPServ.CurrRoomID, FCurrentWl[i], IntToStr(FCurrentWl[i]), faRemoveWishList);

  for i := 0 to length(FNeededWl) - 1 do
    if not inList(FCurrentWl, FNeededWl[i]) then
      FQu.Add(FMPServ.CurrRoomID, FNeededWl[i], IntToStr(FCurrentWl[i]), faAddWishList);

  if FQu.Count > 0 then
  begin
    FMPServ.CheckAndPerform(world, FQu);
    FQu.Clear;
  end;
end;

function TMTaskWhishListUpdate.isNeededWlFull: boolean;
begin
  Result := length(FNeededWl) >= 10;
end;

{ TMTaskGiftSend }

constructor TMTaskProcessGifts.Create;
begin
  inherited;
  SetTaskType(ttProcessGifts);
end;

procedure TMTaskProcessGifts.IntExecute;
var
  i: integer;
  world: TMWorld;
  FIBQuery: TpFIBQuery;
  gift: TSendGiftRec;
  PckCount: integer;
begin
  inherited;

  world := TMWorld.GetInstance;
  if (world = nil) or (not world.Valid) then exit;

  PckCount := 40 + random(20);
  FQu.Clear;
  FQu.CurrentXP := world.LastHeader.Exp;

  // send gifts
  if length(world.AvailGift) > 0 then
    for i := 1 to 5 do
    begin
      // wishlist
      FIBQuery := FDB.MakeGifts;
      while (not FIBQuery.Eof) and (FQu.Count < PckCount) do
      begin
        gift.Clear;
        gift.ID := FIBQuery.FieldByName('gift_un_id').AsInteger;
        gift.GameItemID := FIBQuery.FieldByName('game_items_id').AsInteger;
        gift.UserID := FIBQuery.FieldByName('ID').AsInt64;

        if world.MakeGift(gift) then
        begin
          QueueAddGift(gift);
          FDB.SubtractReward(gift);
          if FQu.Count >= PckCount then break;
        end;

        FIBQuery.Next;
      end;
      FIBQuery.Close;

      // send other gifts. max level for random gifts - 100 (dont spam with gifts)
      FIBQuery := FDB.GetGiftFriendsList(100);
      while (not FIBQuery.Eof) and (FQu.Count < PckCount) do
      begin
        gift.Clear;
        gift.UserID := FIBQuery.FieldByName('ID').AsInt64;

        if world.CanGift(gift.UserID) then
        begin
          world.GetNextGift(gift);

          if world.MakeGift(gift) then
          begin
            QueueAddGift(gift);
            FDB.SubtractReward(gift, 0.5);    //  тут 2 раза вычисляется стоимость гифта - это может быть медленно!
            if FQu.Count >= PckCount then break;
          end;
        end;

        FIBQuery.Next;
      end;
      FIBQuery.Close;

      FDB.Commit;

      if FQu.Count > 0 then
      begin
        FMPServ.CheckAndPerform(world, FQu);
        sleep(FQu.Count * 900);
        FQu.Clear;
      end
      else
       break;  // nothing to send
    end;
end;

procedure TMTaskProcessGifts.QueueAddGift(gift: TSendGiftRec);
var
  elm: TActionQueueElm;
begin
  elm := FQu.Add(FMPServ.CurrRoomID, gift.ID, IntToStr(gift.ID), faSendGift);
  elm.AddAttr('second_user_id', IntToStr(gift.UserID));
  elm.AddAttr('recipient_name', FVK.GetFriendFirstName(gift.UserID));
  elm.AddAttr('sender_name', FVK.UserFirstName);
end;

{ TMTaskFriendHelp }

constructor TMTaskFriendHelp.Create;
begin
  inherited;
  SetTaskType(ttFriendHelp);
end;

procedure TMTaskFriendHelp.IntExecute;
begin
  inherited;

end;

end.
