{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Result;

interface

uses
  DataValidator.Result.Intf, DataValidator.Information.Intf, DataValidator.Information,
  System.SysUtils;

type
  TDataValidatorResult = class(TInterfacedObject, IDataValidatorResult)
  private
    FOK: Boolean;
    FDataInformations: IDataValidatorInformations;
    FDataValues: TArray<string>;
  public
    function OK: Boolean;
    function Informations: IDataValidatorInformationsResult;
    function Values: TArray<string>;

    constructor Create(const AOK: Boolean; const ADataInformations: IDataValidatorInformations; const ADataValues: TArray<string>);
    destructor Destroy; override;

    class function New(const AOK: Boolean; const ADataInformation: IValidatorInformation): IDataValidatorResult; overload;
    class function New(const AOK: Boolean; const ADataInformations: IDataValidatorInformations; const ADataValues: TArray<string> = []): IDataValidatorResult; overload;
  end;

implementation

{ TDataValidatorResult }

class function TDataValidatorResult.New(const AOK: Boolean; const ADataInformations: IDataValidatorInformations; const ADataValues: TArray<string> = []): IDataValidatorResult;
begin
  Result := TDataValidatorResult.Create(AOK, ADataInformations, ADataValues);
end;

class function TDataValidatorResult.New(const AOK: Boolean; const ADataInformation: IValidatorInformation): IDataValidatorResult;
begin
  Result := TDataValidatorResult.Create(AOK, TDataValidatorInformations.New.Add(ADataInformation), []);
end;

constructor TDataValidatorResult.Create(const AOK: Boolean; const ADataInformations: IDataValidatorInformations; const ADataValues: TArray<string>);
begin
  FOK := AOK;
  FDataInformations := ADataInformations;
  FDataValues := ADataValues;
end;

destructor TDataValidatorResult.Destroy;
begin
  inherited;
end;

function TDataValidatorResult.OK: Boolean;
begin
  Result := FOK;
end;

function TDataValidatorResult.Informations: IDataValidatorInformationsResult;
begin
  Result := FDataInformations;
end;

function TDataValidatorResult.Values: TArray<string>;
begin
  Result := FDataValues;
end;

end.
