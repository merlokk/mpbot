unit uFactories;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  clHttpRequest, clGZip, clCookies, clConnection, clMultiDC, clSingleDC,
  clDownLoader, clTcpClient, clHttp,  Dialogs, StdCtrls, XMLIntf, XMLDoc,
  StrUtils,
  uDB, uDefs, uGameItems, uLogger;

type
  TItemsFactory = class
  protected
    class var FInstance: TItemsFactory;
    class constructor ClassCreate;
  private
    FDB: TMPdatabase;
    FItems: array of TMGameItem;

    function IntGetItem(id: integer): TMGameItem;
    procedure IntAddItem(item: TMGameItem);
  public
    class function GetInstance: TItemsFactory; static;
    constructor Create; virtual;
    destructor Destroy; override;

    function GetGameItem(id: integer): TMGameItem; overload;
    function GetGameItem(name: string): TMGameItem; overload;
  end;

  TFieldsFactory = class
  protected
    class var FInstance: TFieldsFactory;
    class constructor ClassCreate;
  private
    FDB: TMPdatabase;
    FIFactory: TItemsFactory;

  public
    class function GetInstance: TFieldsFactory; static;
    constructor Create; virtual;
    destructor Destroy; override;

    function GetField(name: string): TMField;
  end;

implementation

{ TItemsFactory }

class constructor TItemsFactory.ClassCreate;
begin
  FInstance := nil;
end;

constructor TItemsFactory.Create;
begin
  inherited;

  FDB := TMPdatabase.GetInstance;
  SetLength(FItems, 0);
end;

destructor TItemsFactory.Destroy;
var
  i: Integer;
begin
  for i := 0 to length(FItems) - 1 do
    FItems[i].Free;

  inherited;
end;

function TItemsFactory.GetGameItem(id: integer): TMGameItem;
begin
  Result := IntGetItem(id);
  if Result = nil then
  begin
    Result := FDB.GetGameItem(id);
    IntAddItem(Result);
  end;
end;

function TItemsFactory.GetGameItem(name: string): TMGameItem;
begin
  Result := GetGameItem(FDB.GetGameItemId(name));
end;

class function TItemsFactory.GetInstance: TItemsFactory;
begin
  if not Assigned(FInstance) then
    FInstance := TItemsFactory.Create;
  Result := FInstance;
end;

procedure TItemsFactory.IntAddItem(item: TMGameItem);
begin
  if item = nil then exit;

  SetLength(FItems, length(FItems) + 1);
  FItems[length(FItems) - 1] := item;
end;

function TItemsFactory.IntGetItem(id: integer): TMGameItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to length(FItems) - 1 do
    if FItems[i].ID = id then
    begin
      Result := FItems[i];
      break;
    end;
end;

{ TFieldsFactory }

class constructor TFieldsFactory.ClassCreate;
begin
  FInstance := nil;
end;

constructor TFieldsFactory.Create;
begin
  inherited;

  FDB := TMPdatabase.GetInstance;
  FIFactory := TItemsFactory.GetInstance;
end;

destructor TFieldsFactory.Destroy;
begin

  inherited;
end;

function TFieldsFactory.GetField(name: string): TMField;
var
  item: TMGameItem;
begin
  item := FIFactory.GetGameItem(name);

  if item = nil then
  begin
    Result := TMField.Create;
    exit;
  end;

  case item.ItemType of
    gitFactory: Result := TMFieldFactory.Create;
    gitBuilding: Result := TMFieldBuilding.Create;
    gitHouse: Result := TMFieldHouse.Create;
  else
    Result := TMField.Create;
  end;
end;

class function TFieldsFactory.GetInstance: TFieldsFactory;
begin
  if not Assigned(FInstance) then
    FInstance := TFieldsFactory.Create;
  Result := FInstance;
end;

end.
