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
unit Validator.IsJSON; // JSON (JavaScript Object Notation)

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.JSON;

type
  TTypeJSON = (tjAll, tjArray, tjObject);

  TValidatorIsJson = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FTypeJSON: TTypeJSON;
  public
    function Check: IDataValidatorResult;
    constructor Create(const ATypeJSON: TTypeJSON; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsJson }

constructor TValidatorIsJson.Create(const ATypeJSON: TTypeJSON; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FTypeJSON := ATypeJSON;
  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorIsJson.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LJV: TJsonValue;
begin
  LValue := Trim(GetValueAsString);
  R := False;

  if not LValue.IsEmpty then
  begin
    LValue := StringReplace(LValue, '\r\n', '', [rfReplaceAll]);
    LValue := StringReplace(LValue, sLineBreak, '', [rfReplaceAll]);

    LJV := nil;

    try
      LJV := TJSONObject.ParseJSONValue(LValue);
    except
    end;

    if Assigned(LJV) then
    begin
      try
        case FTypeJSON of
          tjAll:
            R := (LJV is TJSONArray) or (LJV is TJSONObject);
          tjArray:
            R := LJV is TJSONArray;
          tjObject:
            R := LJV is TJSONObject;
        end;
      finally
        LJV.Free;
      end;
    end;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, GetMessage, FExecute));
end;

end.
