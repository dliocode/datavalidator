{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsTime;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.DateUtils;

type
  TValidatorIsTime = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FJSONISO8601ReturnUTC: Boolean;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AJSONISO8601ReturnUTC: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsTime }

constructor TValidatorIsTime.Create(const AJSONISO8601ReturnUTC: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FJSONISO8601ReturnUTC := AJSONISO8601ReturnUTC;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsTime.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LTime: TDateTime;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
  begin
    LValue := LValue.Replace('\','');

    R := TryStrToTime(LValue, LTime);

    if not R then
      R := TryISO8601ToDate(LValue, LTime, FJSONISO8601ReturnUTC);
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
