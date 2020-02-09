unit Params;

interface

procedure SetName(v: String);
procedure SetBrightness(v: Double);
procedure SetVibrancy(v: Double);
procedure SetGamma(v: Double);
procedure SetGammaThreshold(v: Double);
procedure SetOversample(v: Integer);
procedure SetFilter(v: Double);
procedure SetZoom(v: Double);
procedure SetScale(v: Double);
procedure SetQuality(v: Double);
procedure SetAngle(v: Double);
procedure SetRotate(v: Double);
procedure SetCamPitch(v: Double);
procedure SetCamYaw(v: Double);
procedure SetCamPerspective(v: Double);
procedure SetCamDist(v: Double);
procedure SetCamZPos(v: Double);
procedure SetCamDof(v: Double);
procedure SetEstimatorRadius(v: Double);
procedure SetEstimatorMinimum(v: Double);
procedure SetEstimatorCurve(v: Double);
procedure SetEnableDE(v: Boolean);
procedure SetCenter(v1, v2: Double);
procedure SetSize(v1, v2: Double);
procedure SetBackground(v1, v2, v3: Integer);
procedure SetSoloXForm(v: Integer);
procedure SetCMapPos(i, r, g, b: Integer);

procedure SetFinalEnabled(v: Boolean);
procedure SetFinalCoefs(i: Integer; v: Double);
procedure SetFinalPost(i: Integer; v: Double);
procedure SetFinalColor(v: Double);
procedure SetFinalVarColor(v: Double);
function  SetFinalVar(k: String; v: Double): Boolean;

procedure SetXName(i: Integer; v: String);
procedure SetXWeight(i: Integer; v: Double);
procedure SetXColorSpeed(i: Integer; v: Double);
procedure SetXChaos(i, j: Integer; v: Double);
procedure SetXOpacity(i: Integer; v: Double);
procedure SetXCoefs(i, j: Integer; v: Double);
procedure SetXPost(i, j: Integer; v: Double);
procedure SetXColor(i: Integer; v: Double);
procedure SetXVarColor(i: Integer; v: Double);
function  SetXVar(i: Integer; k: String; v: Double): Boolean;

procedure Reset;
procedure ApplyFinal;
function  FlameToString: String;
function  WriteBatchTo(fileName: string; fbatchSize, width, height: integer): boolean;

implementation

uses Global, SysUtils, Main, ControlPoint, XForm, XFormMan, Logging,
  ParameterIO, RndFlame, cmap, GradientHlpr;

procedure RandomFlame(var cp: TControlPoint; algorithm: integer);
var
  Min, Max, i, j, rnd: integer;
  r, s, theta, phi: double;
  t: integer; skip: boolean;
begin
  inc(G_Seed);
  RandSeed := G_Seed;

  transforms := random(5 - (2 - 1)) + 2;

  try
    inc(G_Seed);
    RandSeed := G_Seed;

    cp.clear;
    inc(G_Seed);
    RandSeed := G_Seed;

    rnd := random(10);
    for i := 0 to Transforms - 1 do
    begin

      if Random(10) < 9 then
        cp.xform[i].c[0, 0] := 1
      else
        cp.xform[i].c[0, 0] := -1;
      cp.xform[i].c[0, 1] := 0;
      cp.xform[i].c[1, 0] := 0;
      cp.xform[i].c[1, 1] := 1;
      cp.xform[i].c[2, 0] := 0;
      cp.xform[i].c[2, 1] := 0;
      cp.xform[i].color := 0;
      cp.xform[i].symmetry := 0;
      cp.xform[i].SetVariation(0, 1);
      for j := 1 to NRVAR - 1 do
        cp.xform[i].SetVariation(j, 0);
      cp.xform[i].Translate(random * 2 - 1, random * 2 - 1);
      cp.xform[i].Rotate(random * 360);
      if i > 0 then
        cp.xform[i].Scale(random * 0.8 + 0.2)
      else
        cp.xform[i].Scale(random * 0.4 + 0.6);
      if Random(2) = 0 then
        cp.xform[i].Multiply(1, random - 0.5, random - 0.5, 1);
    end;

    LogWrite('INFO|Balancing weights', 'general.log');
    t := cp.NumXForms;
    for i := 0 to t - 1 do
      cp.xform[i].density := 1.0 / t;

  except on E: EmathError do

  end;

  LogWrite('INFO|Randomizing gradient', 'general.log');
  cp.cmap := GradientHelper.RandomGradient;

  LogWrite('INFO|Setting scalars', 'general.log');
  cp.brightness := 4.0;
  cp.gamma := 4.0;
  cp.gamma_threshold := 0.001;
  cp.vibrancy := 1.0;
  cp.sample_density := 5.0;
  cp.spatial_oversample := 1;
  cp.spatial_filter_radius := 0.5;
  cp.zoom := 0;

  LogWrite('INFO|Setting background', 'general.log');
  cp.background[0] := 0;
  cp.background[1] := 0;
  cp.background[2] := 0;

  LogWrite('INFO|Setting final XF', 'general.log');
  cp.xform[cp.NumXForms].Clear;
  cp.xform[cp.NumXForms].symmetry := 1;
end;

function WriteBatchTo(fileName: string; fbatchSize, width, height: integer): boolean;
{ Write a series of random ifs to a file }
var
  i: integer;
  F: TextFile;
  b, RandFile, xml: string;
  cp, cp1: TControlPoint;
begin
  b := IntToStr(fbatchSize);
  inc(G_Seed);
  RandSeed := G_Seed;

  cp := TControlPoint.Create;
  cp.Width := width;
  cp.Height := height;

  try
    LogWrite('INFO|Starting random batch', 'general.log');

    AssignFile(F, fileName);
    OpenFile := fileName;
    ReWrite(F);
    WriteLn(F, '<Flames>');
    for i := 0 to fbatchSize - 1 do
    begin
      LogWrite('INFO|Writing flame ' + IntToStr(i + 1) + ' into random batch', 'general.log');

      inc(RandomIndex);
      RandSeed := G_Seed;
      cmap_index := random(NRCMAPS);
      inc(G_Seed);
      RandSeed := G_Seed;

      LogWrite('INFO|Randomizing flame', 'general.log');

      cp1 := cp.Clone;
      RandomFlame(cp, 0);
      cp1.name := RandomDate + '-' + IntToStr(RandomIndex);

      LogWrite('INFO|Adding flame to file', 'general.log');

      SaveCpToXmlCompatible(xml, cp1);
      Write(F, xml);

      cp1.Destroy;
    end;

    LogWrite('INFO|Completing random batch', 'general.log');

    Write(F, '</Flames>');
    CloseFile(F);

    Result := true;
  except
    on EInOutError do
      Result := false;
  end;

  cp.Destroy;
end;

function IsRegisteredVariation(name: string): boolean;
var i, count: integer; vname: string; xf: txform;
begin
xf := txform.Create;
xf.Destroy;
  count:=NrVar;
  for i:=0 to count - 1 do
  begin
    vname := VarNames(i);
    if (lowercase(vname) = lowercase(name)) then
    begin
      Result := true;
      exit;
    end;
  end;
  Result := false;
end;
function IsRegisteredVariable(name: string): boolean;
var i, count: integer;
begin
  count:=GetNrVariableNames;
  for i:=0 to count - 1 do
  begin
    if (LowerCase(GetVariableNameAt(i)) = LowerCase(name)) then
    begin
      Result := true;
      exit;
    end;
  end;
  Result := false;
end;
function InternalSetVar(xf: TXForm; k: String; v: Double): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (k = 'linear3D') then begin
    xf.SetVariation(0, v);
    Result := True;
  end else if (IsRegisteredVariation(k)) then begin
    for i := 0 to NRVAR - 1 do begin
      if lowercase(varnames(i)) = lowercase(k) then begin
        xf.SetVariation(i, v);
        Result := true;
        break;
      end;
    end;
  end else if (IsRegisteredVariable(k)) then begin
    xf.SetVariable(k, v);
    Result := True;
  end;
end;

procedure SetName(v: String); begin G_Flame.name := v; end;
procedure SetVibrancy(v: Double); begin G_Flame.vibrancy := v; end;
procedure SetBrightness(v: Double); begin G_Flame.brightness := v; end;
procedure SetGamma(v: Double); begin G_Flame.gamma := v; end;
procedure SetGammaThreshold(v: Double); begin G_Flame.gamma_threshold := v; end;
procedure SetOversample(v: Integer); begin G_Flame.spatial_oversample := v; end;
procedure SetFilter(v: Double); begin G_Flame.spatial_filter_radius := v; end;
procedure SetZoom(v: Double); begin G_Flame.zoom := v; end;
procedure SetScale(v: Double); begin G_Flame.pixels_per_unit := v; end;
procedure SetQuality(v: Double); begin G_Flame.sample_density := v; end;
procedure SetAngle(v: Double); begin G_Flame.fangle := v; end;
procedure SetRotate(v: Double); begin G_Flame.fangle := -PI * v / 180.0; end;
procedure SetCamPitch(v: Double); begin G_Flame.cameraPitch := v; end;
procedure SetCamYaw(v: Double); begin G_Flame.cameraYaw := v; end;
procedure SetCamPerspective(v: Double); begin G_Flame.cameraPersp := v; end;
procedure SetCamDist(v: Double); begin
  G_Flame.cameraPersp := v;
  if G_Flame.cameraPersp = 0 then
    G_Flame.cameraPersp := EPS;
  G_Flame.cameraPersp := 1 / G_Flame.cameraPersp;
end;
procedure SetCamZPos(v: Double); begin G_Flame.cameraZpos := v; end;
procedure SetCamDof(v: Double); begin G_Flame.cameraDOF := v; end;
procedure SetEstimatorRadius(v: Double); begin G_Flame.estimator := v; end;
procedure SetEstimatorMinimum(v: Double); begin G_Flame.estimator_min := v; end;
procedure SetEstimatorCurve(v: Double); begin G_Flame.estimator_curve := v; end;
procedure SetEnableDE(v: Boolean); begin G_Flame.enable_de := v; end;
procedure SetCenter(v1, v2: Double); begin
  G_Flame.center[0] := v1;
  G_Flame.center[1] := v2;
end;
procedure SetSize(v1, v2: Double); begin
  G_Flame.width := Round(v1);
  G_Flame.height := Round(v2);
end;
procedure SetBackground(v1, v2, v3: Integer); begin
  G_Flame.background[0] := v1;
  G_Flame.background[1] := v2;
  G_Flame.background[2] := v3;
end;
procedure SetSoloXForm(v: Integer); begin G_Flame.soloXform := v; end;
procedure SetCMapPos(i, r, g, b: Integer);
begin
  G_Flame.cmap[i][0] := r;
  G_Flame.cmap[i][1] := g;
  G_Flame.cmap[i][2] := b;
end;

procedure SetFinalEnabled(v: Boolean); begin G_Flame.finalXformEnabled := v; end;
procedure SetFinalCoefs(i: Integer; v: Double); begin G_Flame.finalXform.c[i div 2][i mod 2] := v; end;
procedure SetFinalPost(i: Integer; v: Double); begin G_Flame.finalXform.p[i div 2][i mod 2] := v; end;
procedure SetFinalColor(v: Double); begin G_Flame.finalXform.color := v; end;
procedure SetFinalVarColor(v: Double); begin G_Flame.finalXform.vc := v; end;
function  SetFinalVar(k: String; v: Double): Boolean; begin Result := InternalSetVar(G_Flame.finalXform, k, v); end;

procedure SetXName(i: Integer; v: String); begin G_Flame.xform[i].TransformName := v; end;
procedure SetXWeight(i: Integer; v: Double); begin G_Flame.xform[i].density := v; end;
procedure SetXColorSpeed(i: Integer; v: Double); begin G_Flame.xform[i].symmetry := v; end;
procedure SetXChaos(i, j: Integer; v: Double); begin G_Flame.xform[i].modWeights[j] := v; end;
procedure SetXOpacity(i: Integer; v: Double); begin G_Flame.xform[i].transOpacity := v; end;
procedure SetXCoefs(i, j: Integer; v: Double); begin G_Flame.xform[i].c[j div 2][j mod 2] := v; end;
procedure SetXPost(i, j: Integer; v: Double); begin G_Flame.xform[i].p[j div 2][j mod 2] := v; end;
procedure SetXColor(i: Integer; v: Double); begin G_Flame.xform[i].color := v; end;
procedure SetXVarColor(i: Integer; v: Double); begin G_Flame.xform[i].vc := v; end;
function  SetXVar(i: Integer; k: String; v: Double): boolean; begin Result := InternalSetVar(G_Flame.xform[i], k, v); end;

procedure Reset;
var
  I, J: Integer;
  V: Double;
begin
  if G_Flame <> nil then
    G_Flame.Destroy;
  G_Flame := TControlPoint.Create;

  LogWrite('INFO|FLI::SetName', 'general.log'); SetName('untitled');
  LogWrite('INFO|FLI::SetVibrancy', 'general.log'); SetVibrancy(1);
  LogWrite('INFO|FLI::SetBrightness', 'general.log'); SetBrightness(4);
  LogWrite('INFO|FLI::SetGamma', 'general.log'); SetGamma(4);
  LogWrite('INFO|FLI::SetGammaThreshold', 'general.log'); SetGammaThreshold(0);
  LogWrite('INFO|FLI::SetOversample', 'general.log'); SetOversample(1);
  LogWrite('INFO|FLI::SetFilter', 'general.log'); SetFilter(0.5);
  LogWrite('INFO|FLI::SetZoom', 'general.log'); SetZoom(0);
  LogWrite('INFO|FLI::SetScale', 'general.log'); SetScale(25);
  LogWrite('INFO|FLI::SetQuality', 'general.log'); SetQuality(5);
  LogWrite('INFO|FLI::SetAngle', 'general.log'); SetAngle(0);
  LogWrite('INFO|FLI::SetCamPitch', 'general.log'); SetCamPitch(0);
  LogWrite('INFO|FLI::SetCamYaw', 'general.log'); SetCamYaw(0);
  LogWrite('INFO|FLI::SetCamPerspective', 'general.log'); SetCamPerspective(0);
  LogWrite('INFO|FLI::SetCamZPos', 'general.log'); SetCamZPos(0);
  LogWrite('INFO|FLI::SetCamDof', 'general.log'); SetCamDof(0);
  LogWrite('INFO|FLI::SetEstimatorRadius', 'general.log'); SetEstimatorRadius(0);
  LogWrite('INFO|FLI::SetEstimatorMinimum', 'general.log'); SetEstimatorMinimum(0);
  LogWrite('INFO|FLI::SetEstimatorCurve', 'general.log'); SetEstimatorCurve(0);
  LogWrite('INFO|FLI::SetEnableDE', 'general.log'); SetEnableDE(false);
  LogWrite('INFO|FLI::SetCenter', 'general.log'); SetCenter(0, 0);
  LogWrite('INFO|FLI::SetSize', 'general.log'); SetSize(512, 384);
  LogWrite('INFO|FLI::SetBackground', 'general.log'); SetBackground(0, 0, 0);
  LogWrite('INFO|FLI::SetSoloXForm', 'general.log'); SetSoloXForm(-1);

  LogWrite('INFO|FLI::SetCMapPos', 'general.log');
  for I := 0 to 255 do SetCMapPos(i, i, i, i);
  for I := 0 to NXFORMS - 1 do
  begin
    SetXName(i, '');
    SetXWeight(i, 0);
    SetXColorSpeed(i, 0);
    SetXOpacity(i, 1);
    SetXCoefs(i, 0, 1);
    SetXCoefs(i, 1, 0);
    SetXCoefs(i, 2, 0);
    SetXCoefs(i, 3, 1);
    SetXCoefs(i, 4, 0);
    SetXCoefs(i, 5, 0);
    SetXPost(i, 0, 1);
    SetXPost(i, 1, 0);
    SetXPost(i, 2, 0);
    SetXPost(i, 3, 1);
    SetXPost(i, 4, 0);
    SetXPost(i, 5, 0);
    SetXColor(i, 0);
    SetXVarColor(i, 1);
    for j := 0 to NrVar - 1 do G_Flame.xform[i].SetVariation(j, 0);
    for j := 0 to GetNrVariableNames - 1 do
    begin
      V := 0;
      G_Flame.xform[i].SetVariable(GetVariableNameAt(j), v);
    end;
    for j := 0 to NXFORMS - 1 do SetXChaos(i, j, 1);
  end;

  if G_Flame.finalXform = nil then
    G_Flame.finalXform := TXForm.Create;

  LogWrite('INFO|FLI::SetFinalEnabled', 'general.log'); SetFinalEnabled(false);
  LogWrite('INFO|FLI::SetFinalCoefs', 'general.log'); SetFinalCoefs(0, 1);
  LogWrite('INFO|FLI::SetFinalCoefs', 'general.log'); SetFinalCoefs(1, 0);
  LogWrite('INFO|FLI::SetFinalCoefs', 'general.log'); SetFinalCoefs(2, 0);
  LogWrite('INFO|FLI::SetFinalCoefs', 'general.log'); SetFinalCoefs(3, 1);
  LogWrite('INFO|FLI::SetFinalCoefs', 'general.log'); SetFinalCoefs(4, 0);
  LogWrite('INFO|FLI::SetFinalCoefs', 'general.log'); SetFinalCoefs(5, 0);
  LogWrite('INFO|FLI::SetFinalPost', 'general.log'); SetFinalPost(0, 1);
  LogWrite('INFO|FLI::SetFinalPost', 'general.log'); SetFinalPost(1, 0);
  LogWrite('INFO|FLI::SetFinalPost', 'general.log'); SetFinalPost(2, 0);
  LogWrite('INFO|FLI::SetFinalPost', 'general.log'); SetFinalPost(3, 1);
  LogWrite('INFO|FLI::SetFinalPost', 'general.log'); SetFinalPost(4, 0);
  LogWrite('INFO|FLI::SetFinalPost', 'general.log'); SetFinalPost(5, 0);
  LogWrite('INFO|FLI::SetFinalColor', 'general.log'); SetFinalColor(0);
  LogWrite('INFO|FLI::SetFinalVarColor', 'general.log'); SetFinalVarColor(1);

  LogWrite('INFO|FLI::SetFinalVariation', 'general.log');
  for j := 0 to NrVar - 1 do G_Flame.finalXform.SetVariation(j, 0);

  G_Flame.finalXform.symmetry := 1;

  LogWrite('INFO|FLI::SetFinalVariable', 'general.log');
  for j := 0 to GetNrVariableNames - 1 do
  begin
    V := 0;
    G_Flame.finalXform.SetVariable(GetVariableNameAt(j), v);
  end;

end;
procedure ApplyFinal;
begin
  G_Flame.useFinalXform := true;
  G_Flame.xform[G_Flame.NumXForms] := G_Flame.finalXform;
  G_Flame.finalXform.color := 0;
  G_Flame.finalXform.symmetry := 1;
end;
function FlameToString: String;
begin
  SaveCpToXmlCompatible(Result, G_Flame);
end;

end.
