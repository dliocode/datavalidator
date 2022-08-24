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

unit DataValidator.JSON.Base;

interface

uses
  DataValidator.Intf,
  DataValidator.JSON.Context,
  System.SysUtils, System.JSON;

type
  TDataValidatorJSONBase = class(TDataValidatorJSONContext<IDataValidatorJSONBase>, IDataValidatorJSONBase, IDataValidatorJSONValues)
  private
    [weak]
    FResult: IDataValidatorJSONResult;
    FName: string;
  public
    function &End: IDataValidatorJSONResult;
    function GetName: string;

    constructor Create(const AResult: IDataValidatorJSONResult; const AValue: TJSONPair; const AName: string = ''); reintroduce;
  end;

implementation

{ TDataValidatorJSONBase }

constructor TDataValidatorJSONBase.Create(const AResult: IDataValidatorJSONResult; const AValue: TJSONPair; const AName: string = '');
begin
  inherited Create(Self, AValue);

  FResult := AResult;
  FName := AName;
end;

function TDataValidatorJSONBase.&End: IDataValidatorJSONResult;
begin
  Result := FResult;
end;

function TDataValidatorJSONBase.GetName: string;
begin
  Result := FName;
end;

end.
