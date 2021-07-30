{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsDateBetween;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.DateUtils;

type
  TValidatorIsDateBetween = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FStartDate: TDate;
    FEndDate: TDate;
    FJSONISO8601ReturnUTC: Boolean;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AStartDate: TDate; const AEndDate: TDate; const AJSONISO8601ReturnUTC: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsDateBetween }

constructor TValidatorIsDateBetween.Create(const AStartDate: TDate; const AEndDate: TDate; const AJSONISO8601ReturnUTC: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FStartDate := AStartDate;
  FEndDate := AEndDate;
  FJSONISO8601ReturnUTC := AJSONISO8601ReturnUTC;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsDateBetween.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LDate: TDateTime;
begin
  LValue := GetValueAsString;

  R := TryStrToDate(LValue, LDate);

  if not R then
    R := TryISO8601ToDate(LValue, LDate, FJSONISO8601ReturnUTC);

  if R then
    R := DateInRange(LDate, FStartDate, FEndDate);

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
