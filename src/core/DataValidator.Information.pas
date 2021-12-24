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

unit DataValidator.Information;

interface

uses
  DataValidator.Information.Intf,
  System.Classes, System.Generics.Collections, System.SysUtils;

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

    constructor Create(const AValue: string; const AMessage: string; const AExecute: TDataValidatorInformationExecute);
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
  FList.Free;

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

    Result := Trim(LSL.Text);
  finally
    LSL.Free
  end;
end;

{ TDataValidatorInformation }

constructor TDataValidatorInformation.Create(const AValue: string; const AMessage: string; const AExecute: TDataValidatorInformationExecute);
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
