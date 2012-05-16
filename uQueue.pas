unit uQueue;

interface
uses
  Classes, SysUtils, StrUtils, SyncObjs;

type
  TActionQueue = class
   private
    class var FInstance: TActionQueue;
    class constructor Create;
   public
    class function GetInstance: TActionQueue;
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
  end;

implementation

{ TActionQueue }

procedure TActionQueue.Clear;
begin

end;

class constructor TActionQueue.Create;
begin
  FInstance := nil;
end;

constructor TActionQueue.Create;
begin
  inherited;

  Clear;
end;

destructor TActionQueue.Destroy;
begin

  inherited;
end;

class function TActionQueue.GetInstance: TActionQueue;
begin
  if not Assigned(FInstance) then
    FInstance := TActionQueue.Create;
  Result := FInstance;
end;

end.
