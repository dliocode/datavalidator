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

unit Validator.IsPhoneNumber;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TValidatorIsPhoneNumber = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    function GetPattern: string;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsPhoneNumber }

constructor TValidatorIsPhoneNumber.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorIsPhoneNumber.Check: IDataValidatorResult;
var
  R: Boolean;
  LValue: string;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    R := TRegEx.IsMatch(LValue, GetPattern);

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, GetMessage, FExecute));
end;

function TValidatorIsPhoneNumber.GetPattern: string;
begin
  case FLocaleLanguage of
    TDataValidatorLocaleLanguage.tl_en_US:
      Result := Format('^(\+1)?(\(?(%s%s%s%s)\)?\d{7})$',
        [
        '20[1-9]|21[0-9]|22(0|[4-6]|8|9)|23(1|4|6|9)|24(0|8|9)|25([0-4]|6)|26(0|2|7|9)|27(0|2|6)|28(1|9)|30[1-9]|31(0|[2-9])|32(0|1|3|5)|33([0-2]|4|[6-7]|9)|34(0|3|6|7)|35[1-2]|36(0|1|4|5)|38(0|5|6)|40[1-9]|41(0|[2-9])|42[3-5]|43([0-5]|7|8)|',
        '44(0|2|3)|45(0|8)|46(3|9)|47(0|5|8|9)|48(0|4)|50[1-9]|51(0|[2-9])|520|53(0|1|4|9)|54(0|1|8)|55(1|9)|56([1-3|7)|57(0|1|[3-5)|9)|58(0|1|[5-7])|60[1-9]|61(0|[2-9])|62(0|3|6|8|9)|63(0|1|6|9)|64(1|6|7)|65(0|1|7)|66([0-2]|7|9)|67(0|1|8)|',
        '68([0-2]|4)|70[1-9]|71[2-9]|72(0|4|5|7)|73(1|2|4|7)|74(0|3|7)|75(4|7)|76(0|2|3|5|9)|77(0|[2-5]|8|9)|78([0-2]|[5-7])|80[1-8]|81(0|[2-9])|82(5|8)|83[0-2]|84(3|5|7|8)|85(0|4|[6-9])|86(0|[2-5]|7)|87(0|2|3|8)|90[1-9]|91(0|[2-9])|92(0|5|8|9)|',
        '93(0|1|4|[6-9])|94(0|1|7|9)|95(1|2|4|6|9)|97([0-3]|8|9)|98(0|4|5|9)'
        ]);

    TDataValidatorLocaleLanguage.tl_de_DE:
      Result := Format('^(\+49)?(%s%s%s)$',
        [
        '(3(0|1)|40|69|89|20(1|3|9))\d{8}|',
        '(21(1|2|4)|22(1|8)|23(1|4)|241|251|261|271|34(0|1|5)|35(1|5)|36(1|5)|37(1|5)|381|391|4[2-8]1|5[1-6]1|552|6([1-2]|8)1|7([1-4]|6)1|821|911|9[3-4]1)\d{6,7}|',
        '((213|215|233|364|534|613|615|622|722|913)1|6867|9093)\d{6}'
        ]);

    TDataValidatorLocaleLanguage.tl_fr_FR:
      Result := Format('^(\+33)?((%s)\d{6}|(%s)\d{5,9})$',
        [
        '1[1-9][0-9]|2[1-5][0-9]|26(0|1|[4-8])|2[7-9][0-9]|3[1-9][0-9]|4[1-9][0-9]|51[6-9]|5[2-8][0-9]|60[1-9]|6[1-2][0-9]|63[0-8]|64[0-9]|65[0-9]|6[6-8][0-9]|69(5|8|9)|7[3-8][0-9]|80[0-9]|8[1-2][0-9]|89[0-9]|9[5-6][0-9]|97([0-5]|[7-9])|98[0-9]|99[0-8]',
        '700[0-4]'
        ]);

    TDataValidatorLocaleLanguage.tl_it_IT:
      Result := Format('^(\+39)?((%s%s)\d{5,9})$',
        [
        '2|6|1(0|1|5|9)|3(0|1|5|9)|4(0|1|5|9)|5(0|1|5|9)|7(0|1|5|9)|8(0|1|5|9)|9(0|1|5|9)|12[1-5]|131|14[1-4]|16(0|1|3|5|6)|17[1-5]|18([2-5]|7)|32[1-4]|33(1|2)|34[1-6]|36([2-5]|9)|37[1-7]|38[1-6]|42[1-9]|43[1-9]|44(2|4|5)|46[1-5]|47[1-4]|481|',
        '52[1-5]|53[2-6]|54([1-7]|9)|56[4-6]|57([1-5]|7|8)|58[3-8]|72(1|2)|73[1-7]|74([2-4]|6)|76(1|3|5|6|9)|77(1|3-6)|78([1-5]|9)|82([3-5]|7|8)|83([1-3]|5|6)|86[1-5]|87[1-5]|88[1-5]|92[1-5]|93[1-5]|94(1|2)|96[1-8]|97[1-6]|98[1-5]'
        ]);

    TDataValidatorLocaleLanguage.tl_es_ES:
      Result := Format('^(\+34)?((%s)\d{6})$', ['810|82[0-8]|830|84([1-3]|[5-9])|85(0|1|3|4|[6-9])|86(0|4|5|[7-9])|87[1-9]|88([0-4]|[6-8])|91[0-9]|92[0-8]|93[0-8]|94[1-9]|95[0-9]|96[0-9]|97[1-9]|98[0-8]']);

    TDataValidatorLocaleLanguage.tl_ru_RU:
      Result := Format('^(\+7)?(%s)$', ['\(?(30[1-2]|336|34[1-9]|35[1-3]|365|38[1-5]|388|39[0-1]|39[4-5]|401|41(1|3|5|6)|42(1|3|4|6|7)|47[1-5]|48([1-3]|[5-7])|49([1-6]|[8-9])|81[1-8]|82[0-1]|83(1|2|[4-6])|84[1-8]|85(1|5)|86([1-3]|[5-7]|9)|87([1-3]|[7-9])|80[0-9]|90[0-9]|9[1-9][0-9])\)?\d{7}']);

    TDataValidatorLocaleLanguage.tl_pt_BR:
      Result := Format('^(\+55)?(%s|%s)$', ['((030(0|3)|0(5|8|9)00)\d{6,7})', '(\(?(1[1-9]|2([1-2]|4|[7-8])|3([1-5]|[7-8])|4[1-9]|5(1|[3-5])|6[1-9]|7(1|[3-5])|7(7|9)|8[1-9]|9[1-9])\)?9?\d{4}-?\d{4})']);
  end;
end;

end.
