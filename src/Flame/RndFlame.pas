{
     Apophysis Copyright (C) 2001-2004 Mark Townsend
     Apophysis Copyright (C) 2005-2006 Ronald Hordijk, Piotr Borys, Peter Sdobnov
     Apophysis Copyright (C) 2007-2008 Piotr Borys, Peter Sdobnov
     
     Apophysis "3D hack" Copyright (C) 2007-2008 Peter Sdobnov
     Apophysis "7X" Copyright (C) 2009-2010 Georg Kiehne

     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation; either version 2 of the License, or
     (at your option) any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}
unit RndFlame;

interface

uses
  ControlPoint, Xform;

procedure RandomGradient(var cp: TControlPoint);

implementation

uses
  SysUtils, Global, cmap, GradientHlpr, XFormMan, Classes;

///////////////////////////////////////////////////////////////////////////////
procedure RGBBlend(a, b: integer; var Palette: TColorMap);
{ Linear blend between to indices of a palette }
var
  c, v: real;
  vrange, range: real;
  i: integer;
begin
  if a = b then
  begin
    Exit;
  end;
  range := b - a;
  vrange := Palette[b mod 256][0] - Palette[a mod 256][0];
  c := Palette[a mod 256][0];
  v := vrange / range;
  for i := (a + 1) to (b - 1) do
  begin
    c := c + v;
    Palette[i mod 256][0] := Round(c);
  end;
  vrange := Palette[b mod 256][1] - Palette[a mod 256][1];
  c := Palette[a mod 256][1];
  v := vrange / range;
  for i := a + 1 to b - 1 do
  begin
    c := c + v;
    Palette[i mod 256][1] := Round(c);
  end;
  vrange := Palette[b mod 256][2] - Palette[a mod 256][2];
  c := Palette[a mod 256][2];
  v := vrange / range;
  for i := a + 1 to b - 1 do
  begin
    c := c + v;
    Palette[i mod 256][2] := Round(c);
  end;
end;

function CreatePalette(strng: string): TColorMap;
{ Loads a palette from a gradient string }
var
  Strings: TStringList;
  index, i: integer;
  Tokens: TStringList;
  Indices, Colors: TStringList;
  a, b: integer;
begin
  Strings := TStringList.Create;
  Tokens := TStringList.Create;
  Indices := TStringList.Create;
  Colors := TStringList.Create;
  try
    try
      Strings.Text := strng;
      if Pos('}', Strings.Text) = 0 then raise EFormatInvalid.Create('No closing brace');
      if Pos('{', Strings[0]) = 0 then raise EFormatInvalid.Create('No opening brace.');
      GetTokens(ReplaceTabs(strings.text), tokens);
      Tokens.Text := Trim(Tokens.text);
      i := 0;
      while (Pos('}', Tokens[i]) = 0) and (Pos('opacity:', Lowercase(Tokens[i])) = 0) do
      begin
        if Pos('index=', LowerCase(Tokens[i])) <> 0 then
          Indices.Add(GetVal(Tokens[i]))
        else if Pos('color=', LowerCase(Tokens[i])) <> 0 then
          Colors.Add(GetVal(Tokens[i]));
        inc(i)
      end;
      for i := 0 to 255 do
      begin
        Result[i][0] := 0;
        Result[i][1] := 0;
        Result[i][2] := 0;
      end;
      if Indices.Count = 0 then raise EFormatInvalid.Create('No color info');
      for i := 0 to Indices.Count - 1 do
      begin
       try
        index := StrToInt(Indices[i]);
        while index < 0 do inc(index, 400);
        index := Round(Index * (255 / 399));
        indices[i] := IntToStr(index);
        assert(index>=0);
        assert(index<256);
        Result[index][0] := StrToInt(Colors[i]) mod 256;
        Result[index][1] := trunc(StrToInt(Colors[i]) / 256) mod 256;
        Result[index][2] := trunc(StrToInt(Colors[i]) / 65536);
       except
       end;
      end;
      i := 1;
      repeat
        a := StrToInt(Trim(Indices[i - 1]));
        b := StrToInt(Trim(Indices[i]));
        RGBBlend(a, b, Result);
        inc(i);
      until i = Indices.Count;
      if (Indices[0] <> '0') or (Indices[Indices.Count - 1] <> '255') then
      begin
        a := StrToInt(Trim(Indices[Indices.Count - 1]));
        b := StrToInt(Trim(Indices[0])) + 256;
        RGBBlend(a, b, Result);
      end;
    except on EFormatInvalid do
      begin
//        Result := False;
      end;
    end;
  finally
    Tokens.Free;
    Strings.Free;
    Indices.Free;
    Colors.Free;
  end;
end;

procedure GetGradientFileGradientsNames(const filename: string; var NamesList: TStringList);
var
  i, p: integer;
  Title: string;
  FStrings: TStringList;
begin
  FStrings := TStringList.Create;
  FStrings.LoadFromFile(filename);
  try
    if (Pos('{', FStrings.Text) <> 0) then
    begin
      for i := 0 to FStrings.Count - 1 do
      begin
        p := Pos('{', FStrings[i]);
        if (p <> 0) and (Pos('(3D)', FStrings[i]) = 0) then
        begin
          Title := Trim(Copy(FStrings[i], 1, p - 1));
          if Title <> '' then
            NamesList.Add(Trim(Copy(FStrings[i], 1, p - 1)));
        end;
      end;
    end;
  finally
    FStrings.Free;
  end;
end;

procedure RandomGradient(var cp: TControlPoint);
begin

end;

end.
