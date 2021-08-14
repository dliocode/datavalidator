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

unit DataValidator.Schema.Base;

interface

uses
  DataValidator.Intf, DataValidator.Context, DataValidator.Context.Intf;

type
  TDataValidatorSchemaBase = class(TDataValidatorContext<IDataValidatorSchemaBase>, IDataValidatorSchemaBase, IDataValidatorSchemaContext)
  private
    [weak]
    FDataValidatorSchema: IDataValidatorSchema;
  public
    function &End(): IDataValidatorSchemaContext;

    constructor Create(const ADataValidatorSchema: IDataValidatorSchema); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDataValidatorSchemaBase }

constructor TDataValidatorSchemaBase.Create(const ADataValidatorSchema: IDataValidatorSchema);
begin
  inherited Create(Self, '');
  FDataValidatorSchema := ADataValidatorSchema;
end;

destructor TDataValidatorSchemaBase.Destroy;
begin
  inherited Destroy;
end;

function TDataValidatorSchemaBase.&End: IDataValidatorSchemaContext;
begin
  Result := Self;
end;

end.
