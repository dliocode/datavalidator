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

unit Validator.JSON.Value.IsMinMaxItems;

interface

uses
  DataValidator.ItemBase,
  System.JSON, System.SysUtils;

type
  TTypeJSONValueMinMax = (tmMin, tmMax);

  TDataValidatorJSONValueIsMinMaxItems = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FTypeMinMax: TTypeJSONValueMinMax;
    FTotalItems: Integer;
  public
    function Check: IDataValidatorResult;
    constructor Create(const ATypeMinMax: TTypeJSONValueMinMax; const ATotalItems: Integer; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TDataValidatorJSONValueIsMinMaxItems }

constructor TDataValidatorJSONValueIsMinMaxItems.Create(const ATypeMinMax: TTypeJSONValueMinMax; const ATotalItems: Integer; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  inherited Create;

  FTypeMinMax := ATypeMinMax;
  FTotalItems := ATotalItems;

  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TDataValidatorJSONValueIsMinMaxItems.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LJSONPair: TJSONPair;
  LCountItems: Integer;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    if FValue.IsType<TJSONPair> then
    begin
      LJSONPair := FValue.AsType<TJSONPair>;

      if Assigned(LJSONPair) then
      begin
        LCountItems := 0;

        if LJSONPair.JsonValue is TJSONArray then
          LCountItems := (LJSONPair.JsonValue as TJSONArray).Count
        else
          if LJSONPair.JsonValue is TJSONObject then
            LCountItems := (LJSONPair.JsonValue as TJSONObject).Count;

        case FTypeMinMax of
          tmMin:
            R := LCountItems >= FTotalItems;
          tmMax:
            R := LCountItems <= FTotalItems;
        end;
      end;
    end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(FKey, FName, LValue, GetMessage, FExecute));
end;

end.
