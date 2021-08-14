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

unit DataValidator.Value.Base;

interface

uses
  DataValidator.Intf, DataValidator.Context;

type
  TDataValidatorValueBase = class(TDataValidatorContext<IDataValidatorValueBase>, IDataValidatorValueBase, IDataValidatorValueValues)
  private
    [weak]
    FResult: IDataValidatorValueResult;
    FValue: TArray<string>;
  public
    function &End(): IDataValidatorValueResult;

    function GetValues: TArray<string>;

    constructor Create(const AResult: IDataValidatorValueResult; const AValue: TArray<string>); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDataValidatorValueBase }

constructor TDataValidatorValueBase.Create(const AResult: IDataValidatorValueResult; const AValue: TArray<string>);
begin
  inherited Create(Self, '');
  FValue := AValue;
  FResult := AResult;
end;

destructor TDataValidatorValueBase.Destroy;
begin
  inherited Destroy;
end;

function TDataValidatorValueBase.&End(): IDataValidatorValueResult;
begin
  Result := FResult;
end;

function TDataValidatorValueBase.GetValues: TArray<string>;
begin
  Result := FValue;
end;

end.
