unit TeskPkg.TestJsonValueHelper;

interface

uses
  DUnitX.TestFramework, DUnitX.Assert.Ex,

  System.JSON, System.SysUtils,

  Pkg.JSON.JsonValueHelper;

type
  TAssertHelper = class Helper for Assert
  public
    class procedure AreEqual(const expected: TJsonType; const actual: TJsonType);
  end;

type

  [TestFixture]
  TTestJsonValueHelper = class
  private
    procedure SimpleTypeTest<T: TJSONValue, constructor>(exprected: TJsonType); overload;
    procedure JsonNumberTest(const Value: string; exprected: TJsonType);
    procedure JsonStringTest(const Value: string; exprected: TJsonType);
  public
{$REGION 'Simple type test'}
    [Test]
    procedure Test_nil_JsonObject;
    [Test]
    procedure Test_JSONObject_jtObject;
    [Test]
    procedure Test_TJSONArray_jtArray;
    [Test]
    procedure Test_TJSONNull_jtUnknown;
    [Test]
    procedure Test_TJSONBool_jtUnknown;
    [Test]
    procedure Test_TJSONTrue_jtTrue;
    [Test]
    procedure Test_TJSONFalse_jtfalse;
{$ENDREGION}
{$REGION 'TJSONNumber'}
    [Test]
    procedure Test_TJSONNumber_int_jtInteger;
    [Test]
    procedure Test_TJSONNumber_int64_jtInteger64;
    [Test]
    procedure Test_TJSONNumber_float_jtNumber;
    [Test]
    procedure Test_TJSONNumber_float_wrong_decimalseperator_jtString;
{$ENDREGION}
{$REGION 'TJSONString'}
    [Test]
    procedure Test_TJSONString_ISO8601Date_jtDate;
    [Test]
    procedure Test_TJSONString_ISO8601DateTime_jtDateTime;
    [Test]
    procedure Test_TJSONString_True_jrTrue;
    [Test]
    procedure Test_TJSONString_False_jrFalse;
    [Test]
    procedure Test_TJSONString_IntInString_jrString;
    [Test]
    procedure Test_TJSONString_Int64InString_jrString;
    [Test]
    procedure Test_TJSONString_FloatInString_jrString;
{$ENDREGION}
  end;

implementation

{ TTestJsonValueHelper }

{$REGION 'Simple type test'}

procedure TTestJsonValueHelper.SimpleTypeTest<T>(exprected: TJsonType);
var
  jsonValue: T;
begin
  jsonValue := T.Create;
  try
    Assert.AreEqual(exprected, TJsonValueHelper.GetJsonType(jsonValue));
  finally
    jsonValue.Free;
  end;
end;

procedure TTestJsonValueHelper.Test_JSONObject_jtObject;
begin
  SimpleTypeTest<TJSONObject>(jtObject);
end;

procedure TTestJsonValueHelper.Test_nil_JsonObject;
begin
  Assert.AreEqual(jtObject, TJsonValueHelper.GetJsonType(nil));
end;

procedure TTestJsonValueHelper.Test_TJSONArray_jtArray;
begin
  SimpleTypeTest<TJSONArray>(jtArray);
end;

procedure TTestJsonValueHelper.Test_TJSONBool_jtUnknown;
begin
  SimpleTypeTest<TJSONBool>(jtUnknown);
end;

procedure TTestJsonValueHelper.Test_TJSONFalse_jtfalse;
begin
  SimpleTypeTest<TJSONFalse>(jtFalse);
end;

procedure TTestJsonValueHelper.Test_TJSONNull_jtUnknown;
begin
  SimpleTypeTest<TJSONNull>(jtUnknown);
end;

procedure TTestJsonValueHelper.Test_TJSONTrue_jtTrue;
begin
  SimpleTypeTest<TJSONTrue>(jtTrue);
end;
{$ENDREGION}
{$REGION 'TJSONNumber'}

procedure TTestJsonValueHelper.JsonNumberTest(const Value: string; exprected: TJsonType);
var
  JSONNumber: TJSONNumber;
begin
  JSONNumber := TJSONNumber.Create(Value);
  try
    Assert.AreEqual(exprected, TJsonValueHelper.GetJsonType(JSONNumber));
  finally
    JSONNumber.Free;
  end;
end;

procedure TTestJsonValueHelper.Test_TJSONNumber_float_jtNumber;
begin
  JsonNumberTest('3.14', jtNumber); // NOTE: Json ALLWAYS uses . as decimal seperator
end;

procedure TTestJsonValueHelper.Test_TJSONNumber_float_wrong_decimalseperator_jtString;
begin
  Assert.WillRaise(
    procedure
    begin
      JsonNumberTest('3,14', jtString)
    end);

  JsonStringTest('3,14', jtString);

  // NOTE: Json ALLWAYS uses . as decimal seperator
end;

procedure TTestJsonValueHelper.Test_TJSONNumber_int64_jtInteger64;
begin
  JsonNumberTest(Int64.MaxValue.ToString, jtInteger64);
end;

procedure TTestJsonValueHelper.Test_TJSONNumber_int_jtInteger;
begin
  JsonNumberTest(Integer.MaxValue.ToString, jtInteger);
end;

{$ENDREGION}
{$REGION 'TJSONString'}

procedure TTestJsonValueHelper.JsonStringTest(const Value: string; exprected: TJsonType);
var
  JSONNumber: TJSONString;
begin
  JSONNumber := TJSONString.Create(Value);
  try
    Assert.AreEqual(exprected, TJsonValueHelper.GetJsonType(JSONNumber));
  finally
    JSONNumber.Free;
  end;
end;

procedure TTestJsonValueHelper.Test_TJSONString_ISO8601DateTime_jtDateTime;
begin
  JsonStringTest('2014-02-01T09:28:56.321-10:00', jtDateTime);
  JsonStringTest('2019-08-29T15:33:15', jtDateTime);
  JsonStringTest('2008-08-30T01:45:36.123Z', jtDateTime);
end;

procedure TTestJsonValueHelper.Test_TJSONString_ISO8601Date_jtDate;
begin
  JsonStringTest('2019-08-29', jtDateTime);
end;

procedure TTestJsonValueHelper.Test_TJSONString_True_jrTrue;
begin
  JsonStringTest('true', jtTrue);
  JsonStringTest('True', jtTrue);
end;

procedure TTestJsonValueHelper.Test_TJSONString_False_jrFalse;
begin
  JsonStringTest('False', jtFalse);
  JsonStringTest('false', jtFalse);
end;

procedure TTestJsonValueHelper.Test_TJSONString_IntInString_jrString;
begin
  JsonStringTest('1', jtString);
end;

procedure TTestJsonValueHelper.Test_TJSONString_Int64InString_jrString;
begin
  JsonStringTest(Int64.MaxValue.ToString, jtString);
end;

procedure TTestJsonValueHelper.Test_TJSONString_FloatInString_jrString;
begin
  JsonStringTest(PI.ToString, jtString);
end;

{$ENDREGION}
{ TAssertHelper }

class procedure TAssertHelper.AreEqual(const expected, actual: TJsonType);
begin
  inherited AreEqual(Integer(expected), Integer(actual));
end;

initialization

TDUnitX.RegisterTestFixture(TTestJsonValueHelper);

end.
