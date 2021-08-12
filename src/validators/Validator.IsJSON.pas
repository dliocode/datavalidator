{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsJSON; // JSON (JavaScript Object Notation)

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.JSON;

type
  TValidatorIsJson = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsJson }

constructor TValidatorIsJson.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
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
    LJV := nil;

    try
      LValue := LValue.Replace(#$D, '').Replace(#$A, '').Replace(#9, '').Replace('\r\n','');
      LJV := TJSONObject.ParseJSONValue(LValue, False, False);
    except
    end;

    if Assigned(LJV) then
    begin
      R := LJV is TJSONObject;

      if not R then
        R := LJV is TJSONArray;
    end;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
