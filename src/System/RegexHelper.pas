unit RegexHelper;

interface
  uses Global, SysUtils, StrUtils, PerlRegEx;

  type T2Int = record
    i1, i2: integer;
  end;
  type T2Float = record
    f1, f2: extended;
  end;
  type TRgb = record
    r, g, b: integer;
  end;

function GetStringPart(text, expression: string; group: integer; def: string): string;
function GetBoolPart(text, expression: string; group: integer; def: boolean): boolean;
function GetIntPart(text, expression: string; group: integer; def: integer): integer;
function GetFloatPart(text, expression: string; group: integer; def: extended): extended;
function Get2IntPart(text, expression: string; group: integer; def: integer): T2Int;
function Get2FloatPart(text, expression: string; group: integer; def: extended): T2Float;
function GetRGBPart(text, expression: string; group: integer; def: extended): TRGB;

implementation

(* ***************************** Extract functions ******************************* *)
function GetStringPart(text, expression: string; group: integer; def: string): string;
var Regex: TPerlRegEx;
begin
  Regex := TPerlRegEx.Create(nil);
  Regex.RegEx := expression;
  Regex.Options := [preSingleLine, preCaseless];
  Regex.Subject := text;

  if Regex.Match and (Regex.SubExpressionCount >= group) then
    Result := Regex.SubExpressions[group]
  else Result := def;

  Regex.Free;
end;
function GetBoolPart(text, expression: string; group: integer; def: boolean): boolean;
begin
  Result := GetFloatPart(text, expression, group, StrToFloat(IfThen(def, '1', '0'))) <> 0;
end;
function GetIntPart(text, expression: string; group: integer; def: integer): integer;
var str: string;
begin
  str := GetStringPart(text, expression, group, '');
  Result := StrToIntDef(str, def);
end;
function GetFloatPart(text, expression: string; group: integer; def: extended): extended;
var str: string;
begin
  str := GetStringPart(text, expression, group, '');
  Result := StrToFloatDef(str, def);
end;
function Get2IntPart(text, expression: string; group: integer; def: integer): T2Int;
const expr : string = '(\d+)\s+(\d+)';
var str, s1, s2: string;
begin
  str := GetStringPart(text, expression, group, IntToStr(def) + ' ' + IntToStr(def));
  s1 := GetStringPart(str, expr, 1, IntToStr(def));
  s2 := GetStringPart(str, expr, 2, IntToStr(def));
  Result.i1 := StrToIntDef(s1, def);
  Result.i2 := StrToIntDef(s2, def);
end;
function Get2FloatPart(text, expression: string; group: integer; def: extended): T2Float;
const expr : string = '([\d.eE+-]+)\s+([\d.eE+-]+)';
var str, s1, s2: string;
begin
  str := GetStringPart(text, expression, group, FloatToStr(def) + ' ' + FloatToStr(def));
  s1 := GetStringPart(str, expr, 1, FloatToStr(def));
  s2 := GetStringPart(str, expr, 2, FloatToStr(def));
  Result.f1 := StrToFloatDef(s1, def);
  Result.f2 := StrToFloatDef(s2, def);
end;
function GetRGBPart(text, expression: string; group: integer; def: extended): TRGB;
const expr : string = '([\d.eE+-]+)\s+([\d.eE+-]+)\s+([\d.eE+-]+)';
var str, s1, s2, s3: string;
begin
  str := GetStringPart(text, expression, group, FloatToStr(def) + ' ' + FloatToStr(def) + ' ' + FloatToStr(def));
  s1 := GetStringPart(str, expr, 1, FloatToStr(def));
  s2 := GetStringPart(str, expr, 2, FloatToStr(def));
  s3 := GetStringPart(str, expr, 3, FloatToStr(def));
  Result.r := Round(StrToFloat(s1) * 255);
  Result.g := Round(StrToFloat(s2) * 255);
  Result.b := Round(StrToFloat(s3) * 255);
end;

end.
