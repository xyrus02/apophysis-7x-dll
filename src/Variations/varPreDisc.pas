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

unit varPreDisc;

interface

uses
  BaseVariation, XFormMan;

type
  TVariationPreDisc = class(TBaseVariation)
  private
    vvar_by_pi: double;
  public
    constructor Create;

    class function GetName: string; override;
    class function GetInstance: TBaseVariation; override;

    function GetNrVariables: integer; override;
    function GetVariableNameAt(const Index: integer): string; override;

    function SetVariable(const Name: string; var value: double): boolean; override;
    function GetVariable(const Name: string; var value: double): boolean; override;

    procedure Prepare; override;
    procedure CalcFunction; override;
  end;

implementation

uses
  Math;

{ TVariationPreSpherical }

procedure SinCosA(const Theta: double; var Sin, Cos: double); // to avoid using 'extended' type
asm
    FLD     Theta
    FSINCOS
    FSTP    qword ptr [edx]    // Cos
    FSTP    qword ptr [eax]    // Sin
    FWAIT
end;

///////////////////////////////////////////////////////////////////////////////
procedure TVariationPreDisc.Prepare;
begin
  vvar_by_pi := vvar / PI;
end;

procedure TVariationPreDisc.CalcFunction;
var
  r, sinr, cosr: double;
begin
  SinCosA(PI * sqrt(sqr(FTx^) + sqr(FTy^)), sinr, cosr);
  r := vvar_by_pi * arctan2(FTx^, FTy^);
  FTx^ := sinr * r;
  FTy^ := cosr * r;
  FTz^ := VVAR * FTz^;
end;

///////////////////////////////////////////////////////////////////////////////
constructor TVariationPreDisc.Create;
begin
end;

///////////////////////////////////////////////////////////////////////////////
class function TVariationPreDisc.GetInstance: TBaseVariation;
begin
  Result := TVariationPreDisc.Create;
end;

///////////////////////////////////////////////////////////////////////////////
class function TVariationPreDisc.GetName: string;
begin
  Result := 'pre_disc';
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationPreDisc.GetVariableNameAt(const Index: integer): string;
begin
  Result := '';
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationPreDisc.SetVariable(const Name: string; var value: double): boolean;
begin
  Result := False;
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationPreDisc.GetNrVariables: integer;
begin
  Result := 0
end;

///////////////////////////////////////////////////////////////////////////////
function TVariationPreDisc.GetVariable(const Name: string; var value: double): boolean;
begin
  Result := False;
end;

///////////////////////////////////////////////////////////////////////////////
initialization
  RegisterVariation(TVariationClassLoader.Create(TVariationPreDisc), true, false);
end.
