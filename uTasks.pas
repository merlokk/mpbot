unit uTasks;

interface
uses
  SysUtils, Variants, Types, Classes, IOUtils, Forms, XMLIntf, XMLDoc, StrUtils,
  Windows, DateUtils,
  uMPserv, uVK, uDB, uGameItems, uLogger, uQueue;

type
  TMTaskType = (ttBaseClass, ttInitDB, ttLoadSWF,
    ttInit, ttWorldUpdate, ttCurRoomWork, ttWorkDispatcher,
    ttWhishListUpdate, ttGiftSend, ttFriendHelp,
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
  public
    constructor Create; virtual;

    procedure SetObjects(ATaskExec: TMTaskExecutor; AVK: TVKAPI; AMPServ: TMPServer; ADB: TMPdatabase; AQu: TActionQueue);

    procedure Clear; virtual;
    procedure Execute(Force: boolean = false);
    procedure IntExecute; virtual;
    function canExecute: boolean;

    property TaskType: TMTaskType read FTaskType;
    property ExecCount: integer read FExecCount;
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
    constructor Create; override;
    procedure IntExecute; override;
  end;

  TMTaskGiftSend = class (TMTask)
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
    ttGiftSend: Result := TMTaskGiftSend.Create;
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
  FDB.FillGameFriends(FMPServ.AppFriends);

  // use cahed data if avaliable...
//  Result := fvk.Authenticated or (fvk.GameFriendsCount > 0);
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
begin
  inherited;

  world := TMWorld.GetInstance;
  FMPServ.GetRevision(world);

  // first request to MP server
  FMPServ.GetUserStat(world, 0, true);
  room0 := world.GetRoom(0);

  for i := 1 to world.GetRoomCount - 1 do
  begin
    room := world.GetRoom(i);
    if (room = nil) or (not room.Avaliable) then continue;

    FMPServ.GetUserStat(world, i);
  end;

  FVK.UpdateFriendsDetails(world.Friends);
//  FDB.UpdateFriends();

  if world.Valid and (room0 <> nil) then
  begin
    AddLog(
      'valid=true' +
      ' owner(' + IntToStr(room0.Header.Level) + '):' + IntToStr(room0.Header.OwnerID) +
      ' room cnt=' + IntToStr(world.GetRoomCount) +
      ' friends=' + IntToStr(length(world.Friends)) +
      ' barn=' + IntToStr(length(world.Barn)) );

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

  if world.Valid then
  begin
    FTaskExec.ExecuteTask(ttWhishListUpdate);

    if length(world.AvailGift) > 0 then
      FTaskExec.ExecuteTask(ttGiftSend);
  end;

{
  stat.FullUpdate;
  AddLog('today used gifts count: ' + IntToStr(db.GetTodayUsedGiftsCnt));
       }
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
  room: TMRoom;
  cnt: integer;
begin
  inherited;

  world := TMWorld.GetInstance;
  if (world = nil) or (not world.Valid) then exit;

  room := world.GetRoom(FMPServ.CurrRoomID);
  if room = nil then exit;

  //  ticks more then 30  or
  //  we have "old" ticks - older then 60 seconds   or
  //  we have no more ticks in 20 seconds
  if (room.FieldsExecuteCount(true, false, 0, Now - 5 * OneSecond) > 30) or
     (room.FieldsExecuteCount(true, false, Now - 60 * OneSecond, Now - 5 * OneSecond) > 0) or
     (room.FieldsExecuteCount(true, false, Now - 5 * OneSecond, Now + 20 * OneSecond) <= 0)
  then
  begin
    FQu.Clear;
    FQu.CurrentXP := world.LastHeader.Exp;
    room.FieldsExecute(80 + random(35), true, false);
    if FQu.Count > 0 then
    begin
      FMPServ.CheckAndPerform(world, FQu);

      // if we put ticks to server - end of the task!!!
      exit;
    end;
  end;

  // work items more then 30  or
  // work items more then tick items in 60 seconds * 3
  cnt := room.FieldsExecuteCount(false, true, 0, Now - 10 * OneSecond);
  if (cnt > 30) or
     (cnt > 3 * room.FieldsExecuteCount(true, false, 0, Now + 60 * OneSecond))
  then
  begin
    FQu.Clear;
    FQu.CurrentXP := world.LastHeader.Exp;
    room.FieldsExecute(15 + random(20), false, true);
    if FQu.Count > 0 then
      FMPServ.CheckAndPerform(world, FQu);

    FQu.Clear;
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

constructor TMTaskWhishListUpdate.Create;
begin
  inherited;
  SetTaskType(ttWhishListUpdate);
end;

procedure TMTaskWhishListUpdate.IntExecute;
begin
  inherited;

end;

{ TMTaskGiftSend }

constructor TMTaskGiftSend.Create;
begin
  inherited;
  SetTaskType(ttGiftSend);
end;

procedure TMTaskGiftSend.IntExecute;
begin
  inherited;

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
