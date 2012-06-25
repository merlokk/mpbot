unit uFasade;

interface
uses
  SysUtils, Variants, Classes, Graphics, Math,
  uGameItems, uTasks, uVK, uMPServ, uLogger, uDefs;

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
    function GetNextWorldUpdate: TDateTime;
  public
    constructor Create;

    procedure Init(par: TFasadeParams);
    procedure Run;

    procedure DBUpdate;
    procedure LoadSWF;

    procedure PaintGraf(world: TMWorld; cnv: TCanvas);

    property NextWorldUpdate: TDateTime read GetNextWorldUpdate;
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

function TMPFasade.GetNextWorldUpdate: TDateTime;
begin
  Result := TaskExec.GetNextExecution(ttWorldUpdate);
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

procedure TMPFasade.PaintGraf(world: TMWorld; cnv: TCanvas);
var
 i,
 maxItems,
 delta,
 tall: integer;
 data: array[0..2] of TFieldGraf;
 room: TMRoom;
begin
  for i := 0 to 2 do
  begin
    room := world.GetRoom(i);
    if room = nil then continue;
    room.FieldGrafFill(data[i]);
  end;

  cnv.Brush.Color := clBtnFace;
  cnv.FillRect(cnv.ClipRect);

  delta := (cnv.ClipRect.Right - cnv.ClipRect.Left) div 13;
  tall := (cnv.ClipRect.Bottom - cnv.ClipRect.Top);

  maxItems := 0;
  for i := 0 to 12 do
  begin
    maxItems := Max(maxItems, data[0][i]);
    maxItems := Max(maxItems, data[1][i]);
    maxItems := Max(maxItems, data[2][i]);
  end;

  if maxItems = 0 then exit;

  cnv.Brush.Color := clBlue;
  for i := 0 to 12 do
  begin
    cnv.FillRect(Rect(
      i * delta + 1,
      0,
      i * delta + 2,
      tall));
  end;

  for i := 0 to 12 do
  begin
    cnv.Brush.Color := clGreen;
    if data[0][i] > 0 then
      cnv.FillRect(Rect(
        i * delta + 4,
        tall - max(Trunc(data[0][i] / maxItems * tall), 1),
        i * delta + 4 + (delta div 3 - 1),
        tall));

    cnv.Brush.Color := clOlive;
    if data[1][i] > 0 then
      cnv.FillRect(Rect(
        i * delta + 4 + (delta div 3),
        tall - max(Trunc(data[1][i] / maxItems * tall), 1),
        i * delta + 4 + ((delta div 3)*2 - 1),
        tall));

    cnv.Brush.Color := clNavy;
    if data[2][i] > 0 then
      cnv.FillRect(Rect(
        i * delta + 4 + (delta div 3)*2,
        tall - max(Trunc(data[2][i] / maxItems * tall), 1),
        i * delta + 4 + ((delta div 3)*3 - 1),
        tall));
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
