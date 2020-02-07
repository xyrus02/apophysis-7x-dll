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

unit varPostCurl;

interface

uses
  BaseVariation, XFormMan;

const
  variation_name = 'post_curl';
  num_vars = 3;
  var_cx_name = 'post_curl_c1';
  var_cy_name = 'post_curl_c2';
  var_cz_name = 'post_curl_c3';

type
  TVariationPostCurl = class(TBaseVariation)
  private
    cx, cy, cz: double;

    cx2, cy2, cz2, c2,
    c2x, c2y, c2z: double;

    procedure CalcCx;
    procedure CalcCy;
    procedure CalcCz;
    procedure CalcLinear;

  public
    constructor Create;

    class function GetName: string; override;
    class function GetInstance: TBaseVariation; override;

    function GetNrVariables: integer; override;
    function GetVariableNameAt(const Index: integer): string; override;

    function SetVariable(const Name: string; var value: double): boolean; override;
    function GetVariable(const Name: string; var value: double): boolean; override;
    function ResetVariable(const Name: string): boolean; override;

    procedure Prepare; override;
    procedure CalcFunction; override;
    procedure GetCalcFunction(var f: TCalcFunction); override;
  end;

implementation

uses
  Math;

// TVariationCurl3D

///////////////////////////////////////////////////////////////////////////////
constructor TVariationPostCurl.Create;
var
  rnd: double;
begin
  rnd := 2*random - 1;

  // which maniac made this??
  {case random(3) of
    0: cx := rnd;
    1: cy := rnd;
    2: cz := rnd;
  end;}
  cy := 0; cy := 0; cz := 0;
end;

procedure TVariationPostCurl.Prepare;
begin
  c2x := 2 * cx;
  c2y := 2 * cy;
  c2z := 2 * cz;

  cx2 := sqr(cx);
  cy2 := sqr(cy);
  cz2 := sqr(cz);

  c2 := cx2 + cy2 + cz2;
end;

///////////////////////////////////////////////////////////////////////////////
procedure TVariationPostCurl.GetCalcFunction(var f: TCalcFunction);
begin
{
  if IsZero(cx) and IsZero(cy) and IsZero(cz) then f := CalcLinear
  else
  if IsZero(cx) and IsZero(cy) then f := CalcCz
  else
  if IsZero(cy) and IsZero(cz) then f := CalcCx
  else
  if IsZero(cz) and IsZero(cx) then f := CalcCy
  else
  f := CalcFunction;
}
  if IsZero(cx) then begin
    if IsZero(cy) then begin
      if IsZero(cz) then
        f := CalcLinear
      else
        f := CalcCz;
    end
    else begin
      if IsZero(cz) then
        f := CalcCy
      else
        f := CalcFunction;
    end
  end
  else begin
    if IsZero(cy) and IsZero(cz) then
      f := CalcCx
    else
      f := CalcFunction;
  end;

  f := CalcFunction;

end;

///////////////////////////////////////////////////////////////////////////////
procedure TVariationPostCurl.CalcFunction;
var
  r, r2: double;
begin
  r2 := sqr(FPx^) + sqr(FPy^) + sqr(FPz^);
  r := vvar / (r2*c2 + c2x*FPx^ - c2y*FPy^ + c2z*FPz^ + 1);

  FPx^ := r * (FPx^ + cx*r2);
  FPy^ := r * (FPy^ - cy*r2);
  FPz^ := r * (FPz^ + cz*r2);
end;

///////////////////////////////////////////////////////////////////////////////
procedure TVariationPostCurl.CalcCx;
var
  x, r, r2: double;
begin
  r2 := sqr(FPx^) + sqr(FPy^) + sqr(FPz^);

  r := vvar / (cx2*r2 + c2x*FPx^ + 1);

  FPx^ := r * (FPx^ + cx*r2);
  FPy^ := r * FPy^;
  FPz^ := r * FPz^;
end;

///////////////////////////////////////////////////////////////////////////////
procedure TVariationPostCurl.CalcCy;
var
  r, r2: double;
begin
  r2 := sqr(FPx^) + sqr(FPy^) + sqr(FPz^);

  r := vvar / (cy2*r2 - c2y*FPy^ + 1);

  FPx^ := r * FPx^;
  FPy^ := r * (FPy^ - cy*r2);
  FPz^ := r * FPz^;
end;

///////////////////////////////////////////////////////////////////////////////
procedure TVariationPostCurl.CalcCz;
var
  r, r2: double;
begin
  r2 := sqr(FPx^) + sqr(FPy^) + sqr(FPz^);

  r := vvar / (cz2*r2 + c2z*FPz^ + 1);

  FPx^ := r * FPx^;
  FPy^ := r * FPy^;
  FPz^ := r * (FPz^ + cz*r2);
end;

///////////////////////////////////////////////////////////////////////////////
procedure TVariationPostCurl.CalcLinear;
begin
  FPx^ := vvar * FPx^;
  FPy^ := vvar * FPy^;
  FPz^ := vvar * FPz^;
end;

///////////////////////////////////////////////////////////////////////////////
class function TVariationPostCurl.GetInstance: TBaseVariation;
begin
  Result := TVariationPostCurl.Create;
end;

///////////////////////////////////////////////////////////////////////////////
class function TVariationPostCurl.GetName: string;
begin
  Result := variation_name;
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationPostCurl.GetVariableNameAt(const Index: integer): string;
begin
  case Index of
    0: Result := var_cx_name;
    1: Result := var_cy_name;
    2: Result := var_cz_name;
  else
    Result := '';
  end
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationPostCurl.SetVariable(const Name: string; var value: double): boolean;
begin
  Result := False;
  if Name = var_cx_name then begin
    cx := value;
    Result := True;
  end
  else if Name = var_cy_name then begin
    cy := value;
    Result := True;
  end
  else if Name = var_cz_name then begin
    cz := value;
    Result := True;
  end;
end;

function TVariationPostCurl.ResetVariable(const Name: string): boolean;
begin
  Result := False;
  if Name = var_cx_name then begin
    cx := 0;
    Result := True;
  end
  else if Name = var_cy_name then begin
    cy := 0;
    Result := True;
  end
  else if Name = var_cz_name then begin
    cz := 0;
    Result := True;
  end;
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationPostCurl.GetNrVariables: integer;
begin
  Result := num_vars;
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationPostCurl.GetVariable(const Name: string; var value: double): boolean;
begin
  Result := False;
  if Name = var_cx_name then begin
    value := cx;
    Result := True;
  end
  else if Name = var_cy_name then begin
    value := cy;
    Result := True;
  end
  else if Name = var_cz_name then begin
    value := cz;
    Result := True;
  end;
end;

///////////////////////////////////////////////////////////////////////////////
initialization
  RegisterVariation(TVariationClassLoader.Create(TVariationPostCurl), true, false);
end.
