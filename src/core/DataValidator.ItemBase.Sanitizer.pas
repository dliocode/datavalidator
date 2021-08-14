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

unit DataValidator.ItemBase.Sanitizer;

interface

uses
  DataValidator.ItemBase.Intf, DataValidator.ItemBase, DataValidator.Result.Intf, DataValidator.Result, DataValidator.Information;

type
  TValue = DataValidator.ItemBase.TValue;
  IDataValidatorResult = DataValidator.Result.Intf.IDataValidatorResult;
  IDataSanitizerItem = DataValidator.ItemBase.Intf.IDataSanitizerItem;
  TDataValidatorResult = DataValidator.Result.TDataValidatorResult;
  TDataValidatorInformation = DataValidator.Information.TDataValidatorInformation;

  TDataValidatorItemBaseSanitizer = class(TDataValidatorItemBase, IDataSanitizerItem)
  private
  public
    function Check: IDataValidatorResult;
    function Sanitize: TValue; virtual; abstract;
  end;

implementation

{ TDataValidatorItemBaseSanitizer }

function TDataValidatorItemBaseSanitizer.Check: IDataValidatorResult;
begin
  // not implementation
end;

end.
