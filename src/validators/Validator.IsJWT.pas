{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsJWT;

interface

uses
  DataValidator.ItemBase, Validator.IsBase64,
  System.SysUtils, System.StrUtils, System.NetEncoding, System.JSON;

type
  TValidatorIsJWT = class(TDataValidatorItemBase, IDataValidatorItem) // JWT (JSON Web Token)
  private
    FValidatorBase64: IDataValidatorItem;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsJWT }

constructor TValidatorIsJWT.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
  FValidatorBase64 := TValidatorIsBase64.Create('Value is not base64!');
end;

function TValidatorIsJWT.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LSplit: TArray<string>;
  LResult: IDataValidatorResult;
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
      for I := 0 to Pred(Length(LSplit)) do
      begin
        if I > 1 then
          Continue;

        // Valid Base64
        FValidatorBase64.SetValue(LSplit[I]);
        LResult := FValidatorBase64.Checked;
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
              LJSONValue.DisposeOf;
          end;

          if not R then
            Break;
        end;
      end;
    end;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
