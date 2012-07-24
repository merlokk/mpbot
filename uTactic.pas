unit uTactic;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StrUtils,
  uDefs, uLogger, uGameItems;

type
  TMTactic = class
  private
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; virtual;

    function CanExecuteContract(Field: TMField; Contract: TMGameItem): boolean; virtual;
    function CanPickContract(Field: TMField; Contract: TMGameItem): boolean; virtual;
  end;

  TMRoom1Tactic = class (TMTactic)
  private
    FWorld: TMWorld;
    FRoom: TMRoom;

    function CheckResourcesCapacity(Contract: TMGameItem): boolean;
  public
    function CanExecuteContract(Field: TMField; Contract: TMGameItem): boolean; override;
    function CanPickContract(Field: TMField; Contract: TMGameItem): boolean; override;
  end;

  TMRoom2Tactic = class (TMTactic)
  private
  public
    function CanExecuteContract(Field: TMField; Contract: TMGameItem): boolean; override;
  end;

implementation

{ TMTactic }

function TMTactic.CanExecuteContract(Field: TMField; Contract: TMGameItem): boolean;
begin
  Result := true;
end;

function TMTactic.CanPickContract(Field: TMField; Contract: TMGameItem): boolean;
begin
  Result := true;
end;

procedure TMTactic.Clear;
begin

end;

constructor TMTactic.Create;
begin
  inherited;

  Clear;
end;

destructor TMTactic.Destroy;
begin

  inherited
end;

{ TMRoom1Tactic }

function TMRoom1Tactic.CanExecuteContract(Field: TMField; Contract: TMGameItem): boolean;
var
  FuelNeeded: integer;
begin
  Result := false;
  if Contract = nil then exit;

  FWorld := TMWorld.GetInstance;
  FRoom := FWorld.GetRoom(1);
  if (FRoom = nil) or (not FRoom.Avaliable) then exit;

  if not CheckResourcesCapacity(Contract) then exit;

  // нужно топлива для контракта
  FuelNeeded := Contract.GetAllStatesParamInt(
      'put',
      'fuel') * -1; // was < 0 !!!
  if FuelNeeded > StrToIntDef(FRoom.Header.GetRoomResource('fuel'), 0) then exit;

  Result := true;
end;

function TMRoom1Tactic.CanPickContract(Field: TMField; Contract: TMGameItem): boolean;
begin
  Result := false;
  if Contract = nil then exit;

  FWorld := TMWorld.GetInstance;
  FRoom := FWorld.GetRoom(1);
  if (FRoom = nil) or (not FRoom.Avaliable) then exit;

  Result := CheckResourcesCapacity(Contract);
end;

function TMRoom1Tactic.CheckResourcesCapacity(Contract: TMGameItem): boolean;
var
  ResourcesCapacity,
  ResourcesCount,
  ResourcesNeeded: integer;
  StorageField: TMField;
begin
  Result := false;

  // размер склада
  StorageField := FRoom.GetField('mining_storage');
  if (StorageField = nil) or (StorageField.GameItem = nil) then exit;
  ResourcesCapacity := StorageField.GameItem.GetAllStatesParamInt(
      'create',
      'mining_resources_capacity');
  if ResourcesCapacity <= 0 then exit;

  // количество ресурсов на складе
  ResourcesCount :=
    FWorld.GetBarnCount(16150); // iron_ore

  // количество ресурсов появится после исполнения контракта
  ResourcesNeeded := Contract.GetAllStatesParamInt(
      'pick',
      'mining_resources_capacity'); // was < 0 !!!
  if ResourcesCapacity - ResourcesCount + ResourcesNeeded < 0 then exit;

  Result := true;
end;

{ TMRoom2Tactic }

function TMRoom2Tactic.CanExecuteContract(Field: TMField; Contract: TMGameItem): boolean;
begin
  Result := false;




end;

end.
