unit uTactic;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StrUtils,
  uDefs, uLogger, uGameItems;

type
  TMTactic = class
  private
    FWorld: TMWorld;
    FRoom: TMRoom;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; virtual;

    function CanExecuteContract(Field: TMField; Contract: TMGameItem; Exec: boolean = false): boolean; virtual;
    function ExecuteContract(Field: TMField; Contract: TMGameItem): boolean; virtual;
    function CanPickContract(Field: TMField; Contract: TMGameItem): boolean; virtual;
    function SelectContract(Field: TMField; ContractList: string): TMGameItem; virtual;
  end;

  TMRoom1Tactic = class (TMTactic)
  private
    function CheckResourcesCapacity(Contract: TMGameItem): boolean;
  public
    function CanExecuteContract(Field: TMField; Contract: TMGameItem; Exec: boolean = false): boolean; override;
    function CanPickContract(Field: TMField; Contract: TMGameItem): boolean; override;
  end;

  TMRoom2Tactic = class (TMTactic)
  private
  public
    function CanExecuteContract(Field: TMField; Contract: TMGameItem; Exec: boolean = false): boolean; override;
    function CanPickContract(Field: TMField; Contract: TMGameItem): boolean; override;
  end;

implementation
uses
  uFactories;

{ TMTactic }

function TMTactic.CanExecuteContract(Field: TMField; Contract: TMGameItem; Exec: boolean = false): boolean;
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

  FWorld := nil;
  FRoom := nil;
  Clear;
end;

destructor TMTactic.Destroy;
begin

  inherited
end;

function TMTactic.ExecuteContract(Field: TMField;
  Contract: TMGameItem): boolean;
begin
  Result := CanExecuteContract(Field, Contract, true);
end;

function TMTactic.SelectContract(Field: TMField;
  ContractList: string): TMGameItem;
begin
  Result := nil;
  if ContractList = '' then exit;

  if Pos(',', ContractList) > 0 then
   TItemsFactory.GetInstance.GetGameItem(Copy(ContractList, 1, Pos(',', ContractList) - 1))
  else
   TItemsFactory.GetInstance.GetGameItem(ContractList);
end;

{ TMRoom1Tactic }

function TMRoom1Tactic.CanExecuteContract(Field: TMField; Contract: TMGameItem; Exec: boolean = false): boolean;
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

  if Exec then
  begin
    FRoom.Header.DecRoomResource('fuel', FuelNeeded);
  end;

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

function TMRoom2Tactic.CanExecuteContract(Field: TMField; Contract: TMGameItem; Exec: boolean = false): boolean;
var
  Toгrists,
  ToгristsVIP,
  ToгristsNeeded,
  ToгristsVIPNeeded: integer;
begin
  Result := false;
  if Contract = nil then exit;

  FWorld := TMWorld.GetInstance;
  FRoom := FWorld.GetRoom(2);
  if (FRoom = nil) or (not FRoom.Avaliable) then exit;

  // расходная часть бюджета
  if (Pos('aquapark_', Field.Name) = 1) or
     (Pos('ancient_fort_', Field.Name) = 1)then
  begin
    // нужно туристов для контракта
    ToгristsNeeded := Contract.GetAllStatesParamInt(
        'put',
        'tourists') * -1; // was < 0 !!!
    ToгristsVIPNeeded := Contract.GetAllStatesParamInt(
        'put',
        'vip_tourists') * -1; // was < 0 !!!

    if (ToгristsNeeded > StrToIntDef(FRoom.Header.GetRoomResource('tourists'), 0)) or
       (ToгristsVIPNeeded > StrToIntDef(FRoom.Header.GetRoomResource('vip_tourists'), 0))
    then exit;

    if Exec then
    begin
      FRoom.Header.DecRoomResource('tourists', ToгristsNeeded);
      FRoom.Header.DecRoomResource('vip_tourists', ToгristsVIPNeeded);
    end;
  end;

  // приходная часть бюджета
  if (Pos('marine_terminal_', Field.Name) = 1) or
     (Pos('island_airport_', Field.Name) = 1) then
  begin
    // проверить будет ли к тому времени как закончится контракт такое количество пустых мест
    Contract.GetAttr('stage_length').AsInteger;


    // заглушка - проверка на текущий момент
    Toгrists := Contract.GetAllStatesParamInt(
        'pick',
        'tourists');
    ToгristsVIP := Contract.GetAllStatesParamInt(
        'pick',
        'vip_tourists');

    if (Toгrists + StrToIntDef(FRoom.Header.GetRoomResource('tourists'), 0) >
            StrToIntDef(FRoom.Header.GetRoomResource('tourist_capacity'), 0) + 20) or
       (ToгristsVIP + StrToIntDef(FRoom.Header.GetRoomResource('vip_tourists'), 0) >
            StrToIntDef(FRoom.Header.GetRoomResource('vip_tourist_capacity'), 0) + 20)
    then exit;
  end;

  Result := true;
end;

function TMRoom2Tactic.CanPickContract(Field: TMField;
  Contract: TMGameItem): boolean;
var
  Toгrists,
  ToгristsVIP,
  exp: integer;
begin
  Result := false;
  if Contract = nil then exit;

  FWorld := TMWorld.GetInstance;
  FRoom := FWorld.GetRoom(2);
  if (FRoom = nil) or (not FRoom.Avaliable) then exit;

  // приходная часть бюджета
  if (Pos('marine_terminal_', Field.Name) = 1) or
     (Pos('island_airport_', Field.Name) = 1) then
  begin
    // контракт сделает туристов
    Toгrists := Contract.GetAllStatesParamInt(
        'pick',
        'tourists');
    ToгristsVIP := Contract.GetAllStatesParamInt(
        'pick',
        'vip_tourists');

    if (Toгrists + StrToIntDef(FRoom.Header.GetRoomResource('tourists'), 0) >
            StrToIntDef(FRoom.Header.GetRoomResource('tourist_capacity'), 0) + 20) or
       (ToгristsVIP + StrToIntDef(FRoom.Header.GetRoomResource('vip_tourists'), 0) >
            StrToIntDef(FRoom.Header.GetRoomResource('vip_tourist_capacity'), 0) + 20)
    then exit;

    exp := Contract.GetAllStatesParamInt(
        'pick',
        'exp');
    if exp > 1 then Contract.SetAttr('exp', exp);
  end;

  Result := true;
end;

end.
