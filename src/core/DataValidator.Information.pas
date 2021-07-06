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
  System.Classes;

type
  TDataValidatorInformation = class(TInterfacedObject, IValidatorInformation)
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

    class function New(const AValue: string; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil): IValidatorInformation;
  end;

  TDataValidatorInformations = class(TInterfacedObject, IDataValidatorInformations)
  private
    FList: IInterfaceList;
  public
    function Add(const ADataInformation: IValidatorInformation): IDataValidatorInformations; overload;
    function Add(const ADataInformations: IDataValidatorInformations): IDataValidatorInformations; overload;

    function GetItem(const Index: Integer): IValidatorInformation;
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
  FList := TInterfaceList.Create;
end;

destructor TDataValidatorInformations.Destroy;
begin
  FList.Clear;
  inherited;
end;

function TDataValidatorInformations.Add(const ADataInformation: IValidatorInformation): IDataValidatorInformations;
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

function TDataValidatorInformations.GetItem(const Index: Integer): IValidatorInformation;
begin
  Result := FList.Items[Index] as IValidatorInformation;
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

class function TDataValidatorInformation.New(const AValue: string; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil): IValidatorInformation;
begin
  Result := TDataValidatorInformation.Create(AValue, AMessage, AExecute);
end;

constructor TDataValidatorInformation.Create(const AValue: string; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FValue := AValue;
  FMessage := AMessage;
  FExecute := AExecute;
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
