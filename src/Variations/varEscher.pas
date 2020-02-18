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

unit varEscher;

interface

uses
  BaseVariation, XFormMan;

type
  TVariationEscher = class(TBaseVariation)
  private
    escher_beta, con, dist: double;
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
  end;

implementation

uses
  Math;

///////////////////////////////////////////////////////////////////////////////
procedure TVariationEscher.Prepare;
begin
  sincos(escher_beta, dist, con);
  con := 0.5 * (1.0 + con);
  dist := 0.5 * dist;
end;

procedure TVariationEscher.CalcFunction;
var sn, cs, ang, lnr, m : double;
begin
  ang := arctan2(FTy^, FTx^); // Angular polar dimension
  lnr := 0.5 * ln(FTx^*FTx^ + FTy^*FTy^); // Natural logarithm of the radial polar dimension.

  m := VVAR * exp(con * lnr - dist * ang);

    sincos(con * ang + dist * lnr, sn, cs);

    FPx^ := FPx^ + m * cs;
    FPy^ := FPy^ + m * sn;

  FPz^ := FPz^ + vvar * FTz^;
end;

///////////////////////////////////////////////////////////////////////////////
constructor TVariationEscher.Create;
begin
  escher_beta := 0;
end;

///////////////////////////////////////////////////////////////////////////////
class function TVariationEscher.GetInstance: TBaseVariation;
begin
  Result := TVariationEscher.Create;
end;

///////////////////////////////////////////////////////////////////////////////
class function TVariationEscher.GetName: string;
begin
  Result := 'escher';
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationEscher.GetVariableNameAt(const Index: integer): string;
begin
  case Index Of
  0: Result := 'escher_beta';
  else
    Result := '';
  end
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationEscher.SetVariable(const Name: string; var value: double): boolean;
begin
  Result := False;
  if Name = 'escher_beta' then begin
    value := frac((value + PI) / (2 * PI)) * 2 * PI - PI;
    escher_beta := Value;
    Result := True;
  end 
end;
function TVariationEscher.ResetVariable(const Name: string): boolean;
begin
  Result := False;
  if Name = 'escher_beta' then begin
    escher_beta := 0;
    Result := True;
  end;
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationEscher.GetNrVariables: integer;
begin
  Result := 1
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationEscher.GetVariable(const Name: string; var value: double): boolean;
begin
  Result := False;
  if Name = 'escher_beta' then begin
    Value := escher_beta;
    Result := True;
  end
end;

///////////////////////////////////////////////////////////////////////////////
initialization
  RegisterVariation(TVariationClassLoader.Create(TVariationEscher), true, false);
end.
