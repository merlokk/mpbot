unit uFasade;

interface
uses
  SysUtils, Variants, Classes,
  uGameItems, uTasks, uVK, uMPServ, uLogger;

type
  TFasadeParams = packed record
    Key,
    VerStat,
    VerFPStat,
    VerCheck,
    VerFP,
    OwnerID,
    Revision,

    VKUserName,
    VKPasswd,
    VKKey,
    VKAppID: string;

    procedure Clear;
  end;

  TMPFasade = class
  private
    FVK: TVKAPI;
    FMPSrv: TMPServer;
    FWorking: boolean;

    Params: TFasadeParams;
    TaskExec: TMTaskExecutor;
  public
    constructor Create;

    procedure Init(par: TFasadeParams);
    procedure Run;

    procedure DBUpdate;
    procedure LoadSWF;
  end;

implementation

{ TMPFasade }

constructor TMPFasade.Create;
begin
  inherited;

  FWorking := false;
  Params.Clear;
  TaskExec := nil;
  FVK := Nil;
end;

procedure TMPFasade.DBUpdate;
begin
  if FWorking then exit;

  FWorking := true;
  try
    TaskExec.ExecuteTask(ttInitDB);
  finally
    FWorking := false;
  end;
end;

procedure TMPFasade.Init;
begin
  AddLog('Init...');
  Params := par;

  if FVK <> nil then TaskExec.Free;
  if TaskExec <> nil then TaskExec.Free;

  FVK := TVKAPI.Create(Params.VKUserName, Params.VKPasswd);
  FVK.UserID := Params.OwnerID;
  FVK.ApplicationID := Params.VKAppID;
  FVK.SecretKey := Params.VKKey;

  FMPSrv := TMPServer.GetInstance;
  FMPSrv.OwnerID := Params.OwnerID;
  FMPSrv.AuthKey := Params.Key;
  FMPSrv.UserStatVer := StrToIntDef(Params.VerStat, 1);
  FMPSrv.UserStatFPVer := StrToIntDef(Params.VerFPStat, 1);
  FMPSrv.CheckAndPerformVer := StrToIntDef(Params.VerCheck, 2);
  FMPSrv.VerFP := Params.VerFP;

  TaskExec := TMTaskExecutor.Create(FVK);
  TaskExec.ExecuteTask(ttInit);

  TaskExec.AddTask(ttWorldUpdate);
  TaskExec.AddTask(ttWorkDispatcher);
  AddLog('Init completed.');
end;

procedure TMPFasade.LoadSWF;
begin
  if FWorking then exit;

  FWorking := true;
  try
    TaskExec.ExecuteTask(ttLoadSWF);
  finally
    FWorking := false;
  end;
end;

procedure TMPFasade.Run;
begin
  FWorking := true;
  try
    TaskExec.Execute;
  finally
    FWorking := false;
  end;
end;

{ TFasadeParams }

procedure TFasadeParams.Clear;
begin
  Key := '';
  VerStat := '';
  VerFPStat := '';
  VerCheck := '';
  VerFP := '';
  OwnerID := '';
  Revision := '';
end;

end.
