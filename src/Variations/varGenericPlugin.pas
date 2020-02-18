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

{
  Variation Plugin DLL support for Apophysis:
  Generic Plugin Support Unit
  Started by Jed Kelsey, June 2007

  
  Portions Copyright (C) 2008 Joel Faber

  February 2008:
   - Remove 30 plugin limit
   - Reset variables
}

unit varGenericPlugin;

interface

uses
  BaseVariation,  XFormMan, Logging, Registry, Global,
  Classes, SysUtils, Forms;  //TStrings/TStringList

type
  TPluginVariationClass = class of TPluginVariation;

  TPluginData = record
    Instance: Integer;
    PluginHandle: THandle;
    PluginClass: TPluginVariationClass;

    PluginVarGetName:           function: PAnsiChar; cdecl;
    PluginVarGetNrVariables:    function: Integer; cdecl;
    PluginVarGetVariableNameAt: function(const Index: integer): PAnsiChar; cdecl;

    PluginVarCreate:       function: Pointer; cdecl;
    PluginVarDestroy:      function(var MyVariation: Pointer): LongBool; cdecl;
    PluginVarInit:         function(MyVariation, FPx, FPy, FTx, FTy: Pointer; vvar: double): LongBool; cdecl;
    PluginVarInit3D:       function(MyVariation, FPx, FPy, FPz, FTx, FTy, FTz: Pointer; vvar: double): LongBool; cdecl;
    PluginVarInitDC:       function(MyVariation, FPx, FPy, FPz, FTx, FTy, FTz, color: Pointer; vvar, a, b, c, d, e, f: double): LongBool; cdecl;
    PluginVarPrepare:      function(MyVariation: Pointer): LongBool; cdecl;
    PluginVarCalc:         function(MyVariation: Pointer): LongBool; cdecl;
    PluginVarGetVariable:  function(MyVariation: Pointer; const Name: PAnsiChar; var value: double): LongBool; cdecl;
    PluginVarSetVariable:  function(MyVariation: Pointer; const Name: PAnsiChar; var value: double): LongBool; cdecl;
    PluginVarResetVariable:function(MyVariation: Pointer; const Name: PAnsiChar) : LongBool; cdecl;
  end;
  PPluginData = ^TPluginData;

  // This class serves as a proxy for the plugin variations.
  TPluginVariation = class(TBaseVariation)

  private
    PluginData : TPluginData;
    MyVariation : Pointer;
  public
    constructor Create(varData : TPluginData);
    destructor Destroy; override;

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

type
  TVariationPluginLoader = class (TVariationLoader)
  public
    constructor Create(varData : TPluginData);
    destructor Destroy; override;
    
    function GetName: string; override;
    function GetInstance: TBaseVariation; override;
    function GetNrVariables: integer; override;
    function GetVariableNameAt(const Index: integer): string; override;

  private
    PluginData : TPluginData;
  end;

procedure InitializePlugin(pluginpath, filename: string);
  //////////////////////////////////////////////////////////////////////

implementation

uses
  Windows, //LoadLibrary
  Math;

{$ifdef Apo7XDLL}
var enumerated : boolean;
{$else}
var pluginError:string;
{$endif}

{ TPluginVariation }

///////////////////////////////////////////////////////////////////////////////

constructor TVariationPluginLoader.Create(varData : TPluginData);
begin
  PluginData := varData;
end;

destructor TVariationPluginLoader.Destroy;
begin
  FreeLibrary(PluginData.PluginHandle);
end;

function TVariationPluginLoader.GetName : string;
begin
  Result := String(PluginData.PluginVarGetName);
end;

function TVariationPluginLoader.GetInstance: TBaseVariation;
begin
  Result := TPluginVariation.Create(PluginData);
end;

function TVariationPluginLoader.GetNrVariables: integer;
begin
  Result := PluginData.PluginVarGetNrVariables();
end;

function TVariationPluginLoader.GetVariableNameAt(const Index: integer): string;
begin
  Result := String(PluginData.PluginVarGetVariableNameAt(Index));
end;

///////////////////////////////////////////////////////////////////////////////

procedure TPluginVariation.Prepare;
begin
  with PluginData do begin
    if @PluginVarInitDC <> nil then
      PluginVarInitDC(MyVariation, Pointer(FPX), Pointer(FPy), Pointer(FPz), Pointer(FTx), Pointer(FTy), Pointer(FTz), Pointer(color), vvar, a, b, c, d, e, f)
    else if @PluginVarInit3D <> nil then
      PluginVarInit3D(MyVariation, Pointer(FPX), Pointer(FPy), Pointer(FPz), Pointer(FTx), Pointer(FTy), Pointer(FTz), vvar)
    else
      PluginVarInit(MyVariation, Pointer(FPX), Pointer(FPy), Pointer(FTx), Pointer(FTy), vvar);
    PluginVarPrepare(MyVariation);
  end;
end;

///////////////////////////////////////////////////////////////////////////////
procedure TPluginVariation.CalcFunction;
begin
  PluginData.PluginVarCalc(MyVariation);
end;

///////////////////////////////////////////////////////////////////////////////
constructor TPluginVariation.Create(varData : TPluginData);
begin
  PluginData := varData;
  MyVariation := PluginData.PluginVarCreate;
end;

///////////////////////////////////////////////////////////////////////////////
destructor TPluginVariation.Destroy;
begin
  PluginData.PluginVarDestroy(MyVariation);
  inherited;
end;

///////////////////////////////////////////////////////////////////////////////
class function TPluginVariation.GetInstance: TBaseVariation;
begin
  Result := nil;
end;

///////////////////////////////////////////////////////////////////////////////
class function TPluginVariation.GetName: string;
begin
  Result := '';
end;

///////////////////////////////////////////////////////////////////////////////
function TPluginVariation.GetNrVariables: integer;
begin
  Result := PluginData.PluginVarGetNrVariables();
end;

///////////////////////////////////////////////////////////////////////////////
function TPluginVariation.GetVariableNameAt(const Index: integer): string;
begin
  Result := String(PluginData.PluginVarGetVariableNameAt(Index));
end;

///////////////////////////////////////////////////////////////////////////////
function TPluginVariation.SetVariable(const Name: string; var value: double): boolean;
begin
  Result := PluginData.PluginVarSetVariable(MyVariation,PAnsiChar(AnsiString(Name)),value);
end;

///////////////////////////////////////////////////////////////////////////////
function TPluginVariation.GetVariable(const Name: string; var value: double): boolean;
begin
  Result := PluginData.PluginVarGetVariable(MyVariation,PAnsiChar(AnsiString(Name)),value);
end;

///////////////////////////////////////////////////////////////////////////////
function TPluginVariation.ResetVariable(const Name: string) : boolean;
var
  dummy: double;
begin
  if @PluginData.PluginVarResetVariable <> nil then
    Result := PluginData.PluginVarResetVariable(MyVariation, PAnsiChar(AnsiString(Name)))
  else begin
    dummy := 0;
    Result := PluginData.PluginVarSetVariable(MyVariation,PAnsiChar(AnsiString(Name)), dummy);
  end;
end;

///////////////////////////////////////////////////////////////////////////////
procedure InitializePlugin(pluginpath, filename: string);
var
  name, msg: string;
  PluginData : TPluginData;
  errno:integer;
  errstr:string;
begin
  with PluginData do begin
    PluginHandle := LoadLibrary(PChar(pluginpath + '\' + filename));
    if PluginHandle<>0 then begin
  	  Pointer(PluginVarGetName) := GetProcAddress(PluginHandle,'PluginVarGetName');
  	  if PluginVarGetName = nil then begin  // Must not be a valid plugin!
  		  FreeLibrary(PluginHandle);
  		  msg := 'Invalid plugin type: "' + filename + '" is not a plugin';
        {$ifdef Apo7XDLL}
  		  LogWrite('ERROR|' + msg, 'general.log');
        {$else}
        pluginError := pluginError + msg + #13#10;
        {$endif}
  		  Exit;
  	  end;

      name := PluginVarGetName();
      if GetVariationIndex(name) >= 0 then begin
        FreeLibrary(PluginHandle);
        msg := 'Cannot load plugin from ' + filename + ': variation "' + name + '" already exists!';
        {$ifdef Apo7XDLL}
        LogWrite('ERROR|' + msg, 'general.log');
        {$else}
        pluginError := pluginError + msg + #13#10;
        {$endif}
      end else begin
        Pointer(PluginVarGetNrVariables)    := GetProcAddress(PluginHandle,'PluginVarGetNrVariables');
        Pointer(PluginVarGetVariableNameAt) := GetProcAddress(PluginHandle,'PluginVarGetVariableNameAt');
      	Pointer(PluginVarCreate)            := GetProcAddress(PluginHandle,'PluginVarCreate');
      	Pointer(PluginVarDestroy)           := GetProcAddress(PluginHandle,'PluginVarDestroy');
      	Pointer(PluginVarInit)              := GetProcAddress(PluginHandle,'PluginVarInit');
      	Pointer(PluginVarInit3D)            := GetProcAddress(PluginHandle,'PluginVarInit3D');
      	Pointer(PluginVarInitDC)            := GetProcAddress(PluginHandle,'PluginVarInitDC');
      	Pointer(PluginVarPrepare)           := GetProcAddress(PluginHandle,'PluginVarPrepare');
      	Pointer(PluginVarCalc)              := GetProcAddress(PluginHandle,'PluginVarCalc');
      	Pointer(PluginVarGetVariable)       := GetProcAddress(PluginHandle,'PluginVarGetVariable');
      	Pointer(PluginVarSetVariable)       := GetProcAddress(PluginHandle,'PluginVarSetVariable');
      	Pointer(PluginVarResetVariable)     := GetProcAddress(PluginHandle,'PluginVarResetVariable');
      	RegisterVariation(TVariationPluginLoader.Create(PluginData), @PluginVarInit3D <> nil, @PluginVarInitDC <> nil);
        RegisterVariationFile(pluginpath + '\' + filename, name);
        {$ifdef Apo7XDLL}
      	LogWrite('INFO|Successfully loaded "' + filename + '"', 'general.log');
        {$endif}
      end;
    end else begin
  	  errno := GetLastError;
      errstr := SysErrorMessage(errno);
      msg := 'Cannot open plugin file: ' + filename + ' (Win32-code ' + IntToStr(GetLastError) + ')';
      {$ifdef Apo7XDLL}
      LogWrite('ERROR|' + msg, 'general.log');
      {$else}
      pluginError := pluginError + msg + #13#10;
      {$endif}
    end;
  end;
end;

{$ifdef Apo7XDLL}
procedure InitializePlugins(pluginpath: string);
{$else}
procedure InitializePlugins;
{$endif}
var
  Registry: TRegistry;
  searchResult: TSearchRec;
  name, msg: string;
  PluginData : TPluginData;
  errno:integer;
  errstr:string;
begin
{$ifdef Apo7XDLL}
  if not enumerated then enumerated := true
  else exit;
{$else}
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    { Defaults }
    if Registry.OpenKey('Software\' + APP_NAME + '\Defaults', False) then
      if Registry.ValueExists('PluginPath') then begin
        PluginPath := Registry.ReadString('PluginPath');
      end else begin
        PluginPath := ExtractFilePath(Application.ExeName) + 'Plugins\';
      end
    else PluginPath := ExtractFilePath(Application.ExeName) + 'Plugins\';
    Registry.CloseKey;
  finally
    Registry.Free;
  end;
{$endif}

  NumBuiltinVars := NRLOCVAR + GetNrRegisteredVariations;

  // Try to find regular files matching *.dll in the plugins dir
  {$ifdef Apo7XDLL}
  pluginPath := PluginPath + '\';
  {$endif}
  if FindFirst(pluginpath + '*.dll', faAnyFile, searchResult) = 0 then
  begin
    repeat InitializePlugin(pluginpath, searchResult.Name);
    until (FindNext(searchResult) <> 0);
    SysUtils.FindClose(searchResult); //Since we use Windows unit (LoadLibrary)
  end;

  {$ifdef Apo7XDLL}
  {$else}
  if pluginError <> '' then
      Application.MessageBox(
        PChar('There were problems with some of the plugins:' + #13#10#13#10 + pluginError),
        PChar('Warning'), MB_ICONWARNING or MB_OK);
  {$endif}
end;

///////////////////////////////////////////////////////////////////////////////

end.

