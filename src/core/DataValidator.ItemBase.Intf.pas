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

unit DataValidator.ItemBase.Intf;

interface

uses
  DataValidator.Types, DataValidator.Result.Intf, DataValidator.Information.Intf,
  System.RTTI;

type
  IDataValidatorItemBase = interface
    ['{7A448738-20D6-439D-868C-F28D135B65D8}']
    function GetDataValidatorLocaleLanguage: TDataValidatorLocaleLanguage;
    procedure SetDataValidatorLocaleLanguage(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US);
    procedure SetIsNot(const AIsNot: Boolean);

    procedure SetName(const AName: string);
    procedure SetValue(const AValue: TValue);
    procedure SetMessage(const AMessage: string);
    procedure SetExecute(const AExecute: TDataValidatorInformationExecute); overload;
  end;

  IDataValidatorItem = interface(IDataValidatorItemBase)
    ['{277F2E2E-BBA1-4823-8DCC-8D4FD399CF02}']
    function Check: IDataValidatorResult;
  end;

  IDataSanitizerItem = interface(IDataValidatorItem)
    ['{0491AB3E-2D23-42AC-9CE8-FBA77C3D253D}']
    function Sanitize: TValue;
  end;

implementation

end.
