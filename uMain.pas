unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, DateUtils,
  uFasade, uLogger, uDB, uGameItems, uMPServ, uDefs;

type
  TMainFrm = class(TForm)
    PageControl1: TPageControl;
    tsEditor: TTabSheet;
    tsLog: TTabSheet;
    Label1: TLabel;
    edVKUser: TEdit;
    Label2: TLabel;
    edVKPasswd: TEdit;
    Label3: TLabel;
    edVKAccessKey: TEdit;
    Label4: TLabel;
    edVKAppID: TEdit;
    btParamSave: TButton;
    btParamLoad: TButton;
    btInit: TButton;
    btRun: TButton;
    cbAutorun: TCheckBox;
    lbLog: TListBox;
    btLoadDatrabase: TButton;
    Label6: TLabel;
    edRevision: TEdit;
    Label7: TLabel;
    edVerStat: TEdit;
    edVerCheck: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    edVerFPStat: TEdit;
    edVerFP: TEdit;
    cbNeedGifts: TCheckBox;
    edNeedGiftId: TEdit;
    Label5: TLabel;
    edVKUserID: TEdit;
    Timer1: TTimer;
    Label11: TLabel;
    edAppAuthKey: TEdit;
    btDownloadSWF: TButton;
    Label12: TLabel;
    lbLevel: TLabel;
    Label14: TLabel;
    lbMoney: TLabel;
    Label10: TLabel;
    lbFuel: TLabel;
    imGraph: TImage;
    Label13: TLabel;
    lbCurRoom: TLabel;
    Label15: TLabel;
    cbParamGroup: TComboBox;
    Label16: TLabel;
    lbTourists: TLabel;
    Label17: TLabel;
    lbNextWorkInt: TLabel;
    Label18: TLabel;
    lbLastUpd: TLabel;
    TrayIcon1: TTrayIcon;
    procedure btInitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btLoadDatrabaseClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btRunClick(Sender: TObject);
    procedure btDownloadSWFClick(Sender: TObject);
    procedure btParamSaveClick(Sender: TObject);
    procedure btParamLoadClick(Sender: TObject);
    procedure cbParamGroupChange(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
  private
    { Private declarations }
    Fasade: TMPFasade;
    FViewCnt: integer;

    function GetParamGroup: integer;
    procedure SetParamGroup(grp: string);
    procedure FillParamGroups;
    procedure ParamGroupLoad;
  public
    { Public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.dfm}

procedure TMainFrm.btDownloadSWFClick(Sender: TObject);
begin
  if Fasade = nil then btInit.Click;

  PageControl1.ActivePage := tsLog;
  Fasade.LoadSWF;
end;

procedure TMainFrm.btInitClick(Sender: TObject);
var
  par: TFasadeParams;
begin
  if Fasade <> nil then Fasade.Free;

  par.Clear;
  par.Key := edAppAuthKey.Text;
  par.VerStat := edVerStat.Text;
  par.VerFPStat := edVerFPStat.Text;
  par.VerCheck := edVerCheck.Text;
  par.VerFP := edVerFP.Text;
  par.OwnerID := edVKUserID.Text;
  par.Revision := edRevision.Text;

  par.VKUserName := edVKUser.Text;
  par.VKPasswd := edVKPasswd.Text;
  par.VKKey := edVKAccessKey.Text;
  par.VKAppID := edVKAppID.Text;

  Fasade := TMPFasade.Create;
  Fasade.Init(par);
end;

procedure TMainFrm.btLoadDatrabaseClick(Sender: TObject);
begin
  if Fasade = nil then btInit.Click;

  PageControl1.ActivePage := tsLog;
  Fasade.DBUpdate;
end;

procedure TMainFrm.btParamLoadClick(Sender: TObject);
var
  db: TMPdatabase;
begin
  db := TMPdatabase.GetInstance;
  if not db.Connected then db.Connect;
  if db.Connected then
  begin
    SetParamGroup(db.GetParam(-1, 'CurParamGroup', ''));

    edVerStat.Text := db.GetParam(-1, 'VerStat', '1');
    edVerFPStat.Text := db.GetParam(-1, 'VerFPStat', '11');
    edVerCheck.Text := db.GetParam(-1, 'VerCheck', '5');
    edVerFP.Text := db.GetParam(-1, 'VerFP', 'WIN%2011,2,202,235');
    edRevision.Text := db.GetParam(-1, 'Revision', '');
    edVKAppID.Text := db.GetParam(-1, 'VKAppID', '1858070');

    ParamGroupLoad;
  end;

end;

procedure TMainFrm.btParamSaveClick(Sender: TObject);
var
  db: TMPdatabase;
  grp: integer;
begin
  db := TMPdatabase.GetInstance;
  if not db.Connected then db.Connect;
  if not db.Connected then exit;

  grp := GetParamGroup;
  db.SetParam('CurParamGroup', grp);
  db.SetParam(grp, 'AppAuthKey', edAppAuthKey.Text);
  db.SetParam('VerStat', edVerStat.Text);
  db.SetParam('VerFPStat', edVerFPStat.Text);
  db.SetParam('VerCheck', edVerCheck.Text);
  db.SetParam('VerFP', edVerFP.Text);
  db.SetParam(grp, 'VKUserID', edVKUserID.Text);
  db.SetParam('Revision', edRevision.Text);
  db.SetParam(grp, 'VKUser', edVKUser.Text);
  db.SetParam(grp, 'VKPasswd', edVKPasswd.Text);
  db.SetParam(grp, 'VKAccessKey', edVKAccessKey.Text);
  db.SetParam('VKAppID', edVKAppID.Text);
end;

procedure TMainFrm.btRunClick(Sender: TObject);
begin
  if Fasade = nil then btInit.Click;

  PageControl1.ActivePage := tsLog;
  Fasade.Run;
end;

procedure TMainFrm.cbParamGroupChange(Sender: TObject);
begin
  ParamGroupLoad;
end;

procedure TMainFrm.FillParamGroups;
var
  db: TMPdatabase;
  arr: TNameValArr;
  i: Integer;
begin
  db := TMPdatabase.GetInstance;
  if not db.Connected then db.Connect;
  if db.Connected then
  begin
    arr := db.GetParamGroups;
    for i := 0 to Length(arr) - 1 do
      cbParamGroup.Items.AddObject(
        arr[i].Name,
        TObject(arr[i].Value));
  end;
end;

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  Fasade := nil;
  FViewCnt := 0;

  FillParamGroups;
  btParamLoad.Click;
end;

function TMainFrm.GetParamGroup: integer;
begin
  Result := 1;
  if cbParamGroup.ItemIndex < 0 then exit;

  Result := Integer(cbParamGroup.Items.Objects[cbParamGroup.ItemIndex]);
end;

procedure TMainFrm.ParamGroupLoad;
var
  db: TMPdatabase;
  grp: integer;
begin
  db := TMPdatabase.GetInstance;
  if not db.Connected then db.Connect;
  if db.Connected then
  begin
    grp := GetParamGroup;
    if grp < 0 then exit;

    edAppAuthKey.Text := db.GetParam(grp, 'AppAuthKey', '');
    edVKUserID.Text := db.GetParam(grp, 'VKUserID', '0');
    edVKUser.Text := db.GetParam(grp, 'VKUser', '');
    edVKPasswd.Text := db.GetParam(grp, 'VKPasswd', '');
    edVKAccessKey.Text := db.GetParam(grp, 'VKAccessKey', '');
  end;
end;

procedure TMainFrm.SetParamGroup(grp: string);
var
  i,
  g: integer;
begin
  if cbParamGroup.Items.Count <= 0 then exit;
  cbParamGroup.ItemIndex := 0;

  g := StrToIntDef(grp, -1);

  for i := 0 to cbParamGroup.Items.Count - 1 do
    if integer(cbParamGroup.Items.Objects[i]) = g then
    begin
      cbParamGroup.ItemIndex := i;
      break;
    end;
end;

procedure TMainFrm.Timer1Timer(Sender: TObject);
var
  world: TMWorld;
  dt: TDateTime;
begin
  Timer1.Enabled := false;
  try
    FViewCnt := FViewCnt + 1;
    if (Fasade <> nil) and (FViewCnt > 5) then
    begin
      FViewCnt := 0;

      GetSLLog(TStringList(lbLog.Items));
      world := TMWorld.GetInstance;
      if (world <> nil) and (world.Valid) then
      begin
        lbLevel.Caption := IntToStr(world.LastHeader.Level) + '/' +
           IntToStr(world.LastHeader.RespectLevel) + ' (' +
           IntToStr(world.LastHeader.Exp) + ')';
        lbMoney.Caption := IntToStr(world.LastHeader.Gold) + '/' +
           IntToStr(world.LastHeader.Coins);
        lbFuel.Caption := world.GetRoomResource(1, 'fuel');
        lbTourists.Caption := world.GetRoomResource(2, 'tourists') + '/' +
           world.GetRoomResource(2, 'tourist_capacity');
        lbCurRoom.Caption := IntToStr(TMPServer.GetInstance.CurrRoomID);
        dt := Fasade.NextWorldUpdate;
        if dt > now - 1 then
          lbNextWorkInt.Caption := SecMinToStr(Fasade.NextWorldUpdate - Now)
        else
          lbNextWorkInt.Caption := 'never';

        lbLastUpd.Caption := SecMinToStr(Now - world.LastUpdate) + '/' +
          SecMinToStr(Now - world.LastRoomChange);

        Fasade.PaintGraf(world, imGraph.Canvas);
      end
      else
      begin
        lbLevel.Caption := '---';
        lbMoney.Caption := '---';
        lbFuel.Caption := '---';
        lbTourists.Caption := '---';
        lbCurRoom.Caption := '---';
        lbNextWorkInt.Caption := '---';
        lbLastUpd.Caption := '---';
      end;
    end;

    if cbAutorun.Checked then btRun.Click;
  finally
    Timer1.Enabled := true;
  end;
end;

procedure TMainFrm.TrayIcon1Click(Sender: TObject);
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;

  SetForegroundWindow(MainFrm.Handle);
end;

end.
