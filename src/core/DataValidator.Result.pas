{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Result;

interface

uses
  DataValidator.Result.Intf, DataValidator.Information.Intf,
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

    constructor Create(const AOK: Boolean; const ADataInformation: IDataValidatorInformation); overload;
    constructor Create(const AOK: Boolean; const ADataInformations: IDataValidatorInformations; const ADataValues: TArray<string>); overload;
    destructor Destroy; override;
  end;

implementation

uses
   DataValidator.Information;

{ TDataValidatorResult }

constructor TDataValidatorResult.Create(const AOK: Boolean; const ADataInformation: IDataValidatorInformation);
begin
  Create(AOK, TDataValidatorInformations.Create.Add(ADataInformation), []);
end;

constructor TDataValidatorResult.Create(const AOK: Boolean; const ADataInformations: IDataValidatorInformations; const ADataValues: TArray<string>);
begin
  FOK := AOK;
  FDataInformations := ADataInformations;
  FDataValues := ADataValues;
end;

destructor TDataValidatorResult.Destroy;
begin
  FDataInformations := nil;
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
