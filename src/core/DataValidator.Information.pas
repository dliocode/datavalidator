{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Information;

interface

uses
  DataValidator.Types, DataValidator.Information.Intf,
  System.Classes, System.Generics.Collections;

type
  TDataValidatorInformation = class(TInterfacedObject, IDataValidatorInformation)
  private
    FValue: Variant;
    FMessage: string;
    FExecute: TDataValidatorInformationExecute;
  public
    function Value: string;
    function Message: string;
    function Execute: TDataValidatorInformationExecute;
    procedure OnExecute;

    constructor Create(const AValue: string; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
    destructor Destroy; override;

  end;

  TDataValidatorInformations = class(TInterfacedObject, IDataValidatorInformations)
  private
    FList: TList<IDataValidatorInformation>;
  public
    function Add(const ADataInformation: IDataValidatorInformation): IDataValidatorInformations; overload;
    function Add(const ADataInformations: IDataValidatorInformations): IDataValidatorInformations; overload;

    function GetItem(const Index: Integer): IDataValidatorInformation;
    function Count: Integer;
    function Message: string;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TDataValidatorInformations }

constructor TDataValidatorInformations.Create;
begin
  inherited;
  FList := TList<IDataValidatorInformation>.Create;
end;

destructor TDataValidatorInformations.Destroy;
begin
  FList.Clear;
  FList.DisposeOf;

  inherited;
end;

function TDataValidatorInformations.Add(const ADataInformation: IDataValidatorInformation): IDataValidatorInformations;
begin
  Result := Self;
  FList.Add(ADataInformation);
end;

function TDataValidatorInformations.Add(const ADataInformations: IDataValidatorInformations): IDataValidatorInformations;
var
  I: Integer;
begin
  Result := Self;

  for I := 0 to Pred(ADataInformations.Count) do
    FList.Add(ADataInformations.GetItem(I));
end;

function TDataValidatorInformations.GetItem(const Index: Integer): IDataValidatorInformation;
begin
  Result := FList.Items[Index];
end;

function TDataValidatorInformations.Count: Integer;
begin
  Result := FList.Count;
end;

function TDataValidatorInformations.Message: string;
var
  LSL: TStringList;
  I: Integer;
begin
  LSL := TStringList.Create;
  try
    for I := 0 to Pred(Count) do
      LSL.Add(GetItem(I).Message);

    Result := LSL.Text;
  finally
    LSL.DisposeOf
  end;
end;

{ TDataValidatorInformation }

constructor TDataValidatorInformation.Create(const AValue: string; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FValue := AValue;
  FMessage := AMessage;
  FExecute := AExecute;
end;

destructor TDataValidatorInformation.Destroy;
begin
  inherited;
end;

function TDataValidatorInformation.Value: string;
begin
  Result := FValue;
end;

function TDataValidatorInformation.Message: string;
begin
  Result := FMessage;
end;

function TDataValidatorInformation.Execute: TDataValidatorInformationExecute;
begin
  Result := FExecute;
end;

procedure TDataValidatorInformation.OnExecute;
begin
  if Assigned(FExecute) then
    FExecute;
end;

end.
