program MPolis2;

uses
  Forms,
  uMain in 'uMain.pas' {MainFrm},
  uFasade in 'uFasade.pas',
  uTasks in 'uTasks.pas',
  uGameItems in 'uGameItems.pas',
  uLogger in 'uLogger.pas',
  uMPServ in 'uMPServ.pas',
  uDefs in 'uDefs.pas',
  uDB in 'uDB.pas',
  uVK in 'uVK.pas',
  uFactories in 'uFactories.pas',
  superobject in 'JSON\superobject.pas',
  uQueue in 'uQueue.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
