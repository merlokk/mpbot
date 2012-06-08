unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  uFasade, uLogger, uDB, uGameItems, uMPServ;

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
    procedure btInitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btLoadDatrabaseClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btRunClick(Sender: TObject);
    procedure btDownloadSWFClick(Sender: TObject);
    procedure btParamSaveClick(Sender: TObject);
    procedure btParamLoadClick(Sender: TObject);
  private
    { Private declarations }
    Fasade: TMPFasade;
    FViewCnt: integer;
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
    edAppAuthKey.Text := db.GetParam('AppAuthKey', '');
    edVerStat.Text := db.GetParam('VerStat', '1');
    edVerFPStat.Text := db.GetParam('VerFPStat', '11');
    edVerCheck.Text := db.GetParam('VerCheck', '5');
    edVerFP.Text := db.GetParam('VerFP', 'WIN%2011,2,202,235');
    edVKUserID.Text := db.GetParam('VKUserID', '0');
    edRevision.Text := db.GetParam('Revision', '');
    edVKUser.Text := db.GetParam('VKUser', '');
    edVKPasswd.Text := db.GetParam('VKPasswd', '');
    edVKAccessKey.Text := db.GetParam('VKAccessKey', '');
    edVKAppID.Text := db.GetParam('VKAppID', '1858070');
  end;

end;

procedure TMainFrm.btParamSaveClick(Sender: TObject);
var
  db: TMPdatabase;
begin
  db := TMPdatabase.GetInstance;
  if not db.Connected then db.Connect;
  if not db.Connected then exit;

  db.SetParam('AppAuthKey', edAppAuthKey.Text);
  db.SetParam('VerStat', edVerStat.Text);
  db.SetParam('VerFPStat', edVerFPStat.Text);
  db.SetParam('VerCheck', edVerCheck.Text);
  db.SetParam('VerFP', edVerFP.Text);
  db.SetParam('VKUserID', edVKUserID.Text);
  db.SetParam('Revision', edRevision.Text);
  db.SetParam('VKUser', edVKUser.Text);
  db.SetParam('VKPasswd', edVKPasswd.Text);
  db.SetParam('VKAccessKey', edVKAccessKey.Text);
  db.SetParam('VKAppID', edVKAppID.Text);
end;

procedure TMainFrm.btRunClick(Sender: TObject);
begin
  if Fasade = nil then btInit.Click;

  PageControl1.ActivePage := tsLog;
  Fasade.Run;
end;

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  Fasade := nil;
  FViewCnt := 0;

  btParamLoad.Click;
end;

procedure TMainFrm.Timer1Timer(Sender: TObject);
var
  world: TMWorld;
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
        lbFuel.Caption := IntToStr(world.LastHeader.Fuel);
        lbCurRoom.Caption := IntToStr(TMPServer.GetInstance.CurrRoomID);

        Fasade.PaintGraf(world, imGraph.Canvas);
      end
      else
      begin
        lbLevel.Caption := '---';
        lbMoney.Caption := '---';
        lbFuel.Caption := '---';
        lbCurRoom.Caption := '---';
      end;
    end;

    if cbAutorun.Checked then btRun.Click;
  finally
    Timer1.Enabled := true;
  end;
end;

end.
