{
  ********************************************************************************

  Github - https://github.com/dliocode/datavalidator

  ********************************************************************************

  MIT License

  Copyright (c) 2022 Danilo Lucas

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

unit DataValidator.Value.Base;

interface

uses
  DataValidator.Intf,
  DataValidator.Context;

type
  TDataValidatorValueBase = class(TDataValidatorContext<IDataValidatorValueBase>, IDataValidatorValueBase, IDataValidatorValueValues)
  private
    [weak]
    FResult: IDataValidatorValueResult;
    FValue: TArray<string>;
    FName: string;
  public
    function &End: IDataValidatorValueResult;
    function GetValues: TArray<string>;
    function GetName: string;

    constructor Create(const AResult: IDataValidatorValueResult; const AValue: TArray<string>; const AName: string = ''); reintroduce;
  end;

implementation

{ TDataValidatorValueBase }

constructor TDataValidatorValueBase.Create(const AResult: IDataValidatorValueResult; const AValue: TArray<string>; const AName: string = '');
begin
  inherited Create(Self, '');

  FResult := AResult;
  FValue := AValue;
  FName := AName;
end;

function TDataValidatorValueBase.&End: IDataValidatorValueResult;
begin
  Result := FResult;
end;

function TDataValidatorValueBase.GetValues: TArray<string>;
begin
  Result := FValue;
end;

function TDataValidatorValueBase.GetName: string;
begin
  Result := FName;
end;

end.
