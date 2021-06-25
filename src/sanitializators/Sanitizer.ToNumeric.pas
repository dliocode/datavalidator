{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.ToNumeric;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.SysUtils, System.Variants, System.StrUtils;

type
  TSanitizerToNumeric = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerToNumeric }

constructor TSanitizerToNumeric.Create;
begin
end;

function TSanitizerToNumeric.Sanitize: TValue;
var
  LValue: string;
  LPosPoint: Integer;
  LPosComma: Integer;
begin
  LValue := GetValueAsString;

  if Trim(LValue).IsEmpty then
    Exit;

  LPosPoint := Pos('.', LValue);
  LPosComma := Pos(',', LValue);

  case IndexStr(FormatSettings.DecimalSeparator, ['.', ',']) of
    0:
      begin
        if (LPosComma > 0) and (LPosPoint > 0) then
        begin
          if LPosPoint < LPosComma then
            LValue := VarToStr(LValue).Replace('.', '').Replace(',', '.')
          else
            LValue := VarToStr(LValue).Replace(',', '');
        end
        else
        begin
          if (LPosPoint = 0) and (LPosComma > 0) then
            LValue := VarToStr(LValue).Replace(',', '.')
        end;
      end;
    1:
      begin
        if (LPosComma > 0) and (LPosPoint > 0) then
        begin
          if LPosComma < LPosPoint then
            LValue := VarToStr(LValue).Replace(',', '').Replace('.', ',')
          else
            LValue := VarToStr(LValue).Replace('.', '');
        end
        else
        begin
          if (LPosComma = 0) and (LPosPoint > 0) then
            LValue := VarToStr(LValue).Replace('.', ',')
        end;
      end;
  end;

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
