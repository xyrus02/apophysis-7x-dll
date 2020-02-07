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

unit varElliptic;

interface

uses
  BaseVariation, XFormMan;

type
  TVariationElliptic = class(TBaseVariation)
  public
    constructor Create;

    class function GetName: string; override;
    class function GetInstance: TBaseVariation; override;

    function GetNrVariables: integer; override;
    function GetVariableNameAt(const Index: integer): string; override;

    function SetVariable(const Name: string; var value: double): boolean; override;
    function GetVariable(const Name: string; var value: double): boolean; override;


    procedure CalcFunction; override;
  end;

implementation

uses
  Math;


///////////////////////////////////////////////////////////////////////////////
procedure TVariationElliptic.CalcFunction;
var
  a, b, c, tmp, x2, xmax: double;
begin
  tmp := sqr(FTy^) + sqr(FTx^) + 1.0;
	x2 := 2.0 * FTx^;
	xmax := 0.5 * (sqrt(tmp + x2) + sqrt(tmp - x2));

	a := FTx^ / xmax;
	b := 1.0 - sqr(a);
  c := xmax - 1.0;

  if (b > 0) then b := sqrt(b)
  else b := 0;

  if (c > 0) then c := sqrt(c)
  else c := 0;

  FPz^ := FPz^ + vvar * FTz^;
  FPx^ := FPx^ + (2.0 * vvar / PI) * ArcTan2(a, b);
  if (FTy^ > 0) then FPy^ := FPy^ + (2.0 * vvar / PI) * Ln(xmax + c)
  else FPy^ := FPy^ - (2.0 * vvar / PI) * Ln(xmax + c)
end;

///////////////////////////////////////////////////////////////////////////////
constructor TVariationElliptic.Create;
begin
end;

///////////////////////////////////////////////////////////////////////////////
class function TVariationElliptic.GetInstance: TBaseVariation;
begin
  Result := TVariationElliptic.Create;
end;

///////////////////////////////////////////////////////////////////////////////
class function TVariationElliptic.GetName: string;
begin
  Result := 'elliptic';
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationElliptic.GetVariableNameAt(const Index: integer): string;
begin
  Result := '';
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationElliptic.SetVariable(const Name: string; var value: double): boolean;
begin
  Result := False;
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationElliptic.GetNrVariables: integer;
begin
  Result := 0
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationElliptic.GetVariable(const Name: string; var value: double): boolean;
begin
  Result := False;
end;

///////////////////////////////////////////////////////////////////////////////
initialization
  RegisterVariation(TVariationClassLoader.Create(TVariationElliptic), true, false);
end.
