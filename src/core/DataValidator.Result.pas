{
  ********************************************************************************

  Github - https://github.com/dliocode/datavalidator

  ********************************************************************************

  MIT License

  Copyright (c) 2021 Danilo Lucas

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

  ********************************************************************************
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
var
  LValue: string;
begin
  LValue := '';

  if Assigned(ADataInformation) then
    LValue := ADataInformation.Value;

  Create(AOK, TDataValidatorInformations.Create.Add(ADataInformation), [LValue]);
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
