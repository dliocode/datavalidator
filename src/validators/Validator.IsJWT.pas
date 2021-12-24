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

unit Validator.IsJWT;

interface

uses
  DataValidator.ItemBase, Validator.IsBase64,
  System.SysUtils, System.StrUtils, System.Types, System.NetEncoding, System.JSON;

type
  TValidatorIsJWT = class(TDataValidatorItemBase, IDataValidatorItem) // JWT (JSON Web Token)
  private
  public
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsJWT }

constructor TValidatorIsJWT.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorIsJWT.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LSplit: TStringDynArray;
  LResult: IDataValidatorResult;
  LValidatorBase64: IDataValidatorItem;
  I: Integer;
  LValueDecode: string;
  LJSONValue: TJSONValue;
  LJSONObjectValue: string;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
  begin
    LValue := LValue.Replace('\r\n', '').Replace(sLineBreak, '');
    LSplit := SplitString(LValue, '.');

    if Length(LSplit) = 3 then
    begin
      LValidatorBase64 := TValidatorIsBase64.Create('');

      for I := 0 to Pred(Length(LSplit)) do
      begin
        if I > 1 then
          Continue;

        // Valid Base64
        LValidatorBase64.SetValue(LSplit[I]);
        LResult := LValidatorBase64.Check;
        R := LResult.OK;

        if not R then
          Break;
      end;

      // Valid JSON Object
      if R then
      begin
        for I := 0 to Pred(Length(LSplit)) do
        begin
          if I > 1 then
            Continue;

          LValueDecode := TNetEncoding.Base64.Decode(LSplit[I]);
          LJSONValue := nil;

          try
            if Length(LValueDecode) > 2 then
              try
                LJSONValue := TJSONObject.ParseJSONValue(LValueDecode);
                R := Assigned(LJSONValue);
              except
              end;

            if (I = 0) and R then
              if LJSONValue is TJSONObject then
                R := (LJSONValue as TJSONObject).TryGetValue<string>('alg', LJSONObjectValue);
          finally
            if Assigned(LJSONValue) then
              LJSONValue.Free;
          end;

          if not R then
            Break;
        end;
      end;
    end;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, GetMessage, FExecute));
end;

end.
