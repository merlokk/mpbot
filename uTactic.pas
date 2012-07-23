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

    function CanExecuteContract(Contract: TMGameItem): boolean; virtual;
  end;

  TMRoom1Tactic = class (TMTactic)
  private
  public
    function CanExecuteContract(Contract: TMGameItem): boolean; override;
  end;

  TMRoom2Tactic = class (TMTactic)
  private
  public
    function CanExecuteContract(Contract: TMGameItem): boolean; override;
  end;

implementation

{ TMTactic }

function TMTactic.CanExecuteContract(Contract: TMGameItem): boolean;
begin
  Result := false;
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

function TMRoom1Tactic.CanExecuteContract(Contract: TMGameItem): boolean;
var
  world: TMWorld;
  room: TMRoom;
  ResourcesCapacity,
  ResourcesCount,
  ResourcesNeeded,
  FuelNeeded: integer;
  StorageField: TMField;
begin
  Result := inherited;

  world := TMWorld.GetInstance;
  room := world.GetRoom(1);
  if (room = nil) or (not room.Avaliable) then exit;

  // размер склада
  StorageField := room.GetField('mining_storage');
  if (StorageField = nil) or (StorageField.GameItem = nil) then exit;
  ResourcesCapacity := StorageField.GameItem.GetAllStatesParamInt(
      'create',
      'mining_resources_capacity');
  if ResourcesCapacity <= 0 then exit;

  // количество ресурсов на складе
  ResourcesCount := world.GetBarnCount(16150); // iron_ore

  // количество ресурсов появится после исполнения контракта
  ResourcesNeeded := Contract.GetAllStatesParamInt(
      'pick',
      'mining_resources_capacity'); // was < 0 !!!
  if ResourcesCapacity - ResourcesCount + ResourcesNeeded < 0 then exit;

  // нужно топлива для контракта
  FuelNeeded := Contract.GetAllStatesParamInt(
      'put',
      'fuel') * -1; // was < 0 !!!
  if FuelNeeded > StrToIntDef(room.Header.GetRoomResource('fuel'), 0) then exit;

  Result := true;
end;

{ TMRoom2Tactic }

function TMRoom2Tactic.CanExecuteContract(Contract: TMGameItem): boolean;
begin
  Result := inherited;

end;

end.
