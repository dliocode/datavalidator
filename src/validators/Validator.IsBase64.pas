{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsBase64;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.StrUtils, System.NetEncoding;

type
  TValidatorIsBase64 = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsBase64 }

constructor TValidatorIsBase64.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsBase64.Check: IDataValidatorResult;
var
  LValue: string;
  LValueDecode: string;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
  begin
    LValue := LValue.Replace('\r\n', '').Replace(sLineBreak, '');

    try
      LValueDecode := TNetEncoding.Base64.Decode(LValue);
      R := not LValueDecode.Equals(LValue);
    except
    end;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
