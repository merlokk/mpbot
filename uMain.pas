unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  uFasade, uLogger;

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
    Label10: TLabel;
    edNeedGiftCount: TEdit;
    Label5: TLabel;
    edVKUserID: TEdit;
    Timer1: TTimer;
    Label11: TLabel;
    edAppAuthKey: TEdit;
    btDownloadSWF: TButton;
    procedure btInitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btLoadDatrabaseClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btRunClick(Sender: TObject);
    procedure btDownloadSWFClick(Sender: TObject);
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
end;

procedure TMainFrm.Timer1Timer(Sender: TObject);
begin
  FViewCnt := FViewCnt + 1;
  if FViewCnt > 5 then
  begin
    FViewCnt := 0;

    GetSLLog(TStringList(lbLog.Items));
  end;

end;

end.
