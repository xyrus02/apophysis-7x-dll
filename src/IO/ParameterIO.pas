unit ParameterIO;

interface
  uses Global, SysUtils, StrUtils, ControlPoint, XForm, cmap,
       XFormMan, PerlRegEx, RegexHelper, Classes, Diagnostics;

function IsRegisteredVariation(name: string): boolean;
function IsRegisteredVariable(name: string): boolean;

procedure LoadPaletteFromXmlCompatible(xml: string; var cp: TControlPoint);
procedure LoadXFormFromXmlCompatible(xml: string; isFinalXForm: boolean; var xf: TXForm; var enabled: boolean);
function LoadCpFromXmlCompatible(xml: string; var cp: TControlPoint; var statusOutput: string): boolean;
function SaveCpToXmlCompatible(var xml: string; const cp1: TControlPoint): boolean;

implementation

(* *************************** Validation functions ***************************** *)
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

(* ***************************** Loading functions ******************************* *)
function LoadCpFromXmlCompatible(xml: string; var cp: TControlPoint; var statusOutput: string): boolean;
const
  re_flame    : string = '<flame(.*?)>(.*?)</flame>';
  re_xform    : string = '<((?:final)?xform)(.*?)/>';
  re_palette  : string = '<palette(.*?)>([a-f0-9\s]+)</palette>';
  re_attrib   : string = '([0-9a-z_]+)="(.*?)"';
  re_strtoken : string = '([a-z0-9_]+)';
var
  flame_attribs   : string;
  flame_content   : string;
  xform_type      : string;
  xform_attribs   : string;
  palette_attribs : string;
  palette_content : string;

  find_attribs : TPerlRegEx;
  found_attrib : boolean;
  attrib_name  : string;
  attrib_match : string;

  find_xforms : TPerlRegEx;
  found_xform : boolean;
  xform_index : integer;

  find_strtokens : TPerlRegEx;
  found_strtoken : boolean;
  strtoken_index : integer;
  strtoken_value : string;

  find_palette : TPerlRegEx;

  temp2i : T2Int;
  temp2f : T2Float;
  temprgb : TRGB;

  dummy: boolean;
  attrib_success: boolean;
begin
  find_strtokens := TPerlRegEx.Create(nil);
  find_attribs := TPerlRegEx.Create(nil);
  find_xforms := TPerlRegEx.Create(nil);
  find_palette := TPerlRegEx.Create(nil);

  find_attribs.RegEx := re_attrib;
  find_strtokens.RegEx := re_strtoken;
  find_xforms.RegEx := re_xform;
  find_palette.RegEx := re_palette;

  find_attribs.Options := [preSingleLine, preCaseless];
  find_strtokens.Options := [preSingleLine, preCaseless];
  find_xforms.Options := [preSingleLine, preCaseless];
  find_palette.Options := [preSingleLine, preCaseless];

  flame_attribs := GetStringPart(xml, re_flame, 1, '');
  flame_content := GetStringPart(xml, re_flame, 2, '');

  find_attribs.Subject := flame_attribs;
  found_attrib := find_attribs.Match;
  
  while found_attrib do begin
    attrib_match := find_attribs.MatchedExpression;
    attrib_name := Lowercase(find_attribs.SubExpressions[1]);
    attrib_success := true;

    if attrib_name = 'name' then
      cp.name := GetStringPart(attrib_match, re_attrib, 2, '')
    else if attrib_name = 'vibrancy' then
      cp.vibrancy := GetFloatPart(attrib_match, re_attrib, 2, defVibrancy)
    else if attrib_name = 'brightness' then
      cp.brightness := GetFloatPart(attrib_match, re_attrib, 2, defBrightness)
    else if attrib_name = 'gamma' then
      cp.gamma := GetFloatPart(attrib_match, re_attrib, 2, defGamma)
    else if attrib_name = 'gamma_threshold' then
      cp.gamma_threshold := GetFloatPart(attrib_match, re_attrib, 2, defGammaThreshold)
    else if attrib_name = 'oversample' then
      cp.spatial_oversample := GetIntPart(attrib_match, re_attrib, 2, defOversample)
    else if attrib_name = 'filter' then
      cp.spatial_filter_radius := GetFloatPart(attrib_match, re_attrib, 2, defFilterRadius)
    else if attrib_name = 'zoom' then
      cp.zoom := GetFloatPart(attrib_match, re_attrib, 2, 0)
    else if attrib_name = 'scale' then
      cp.pixels_per_unit := GetFloatPart(attrib_match, re_attrib, 2, 25)
    else if attrib_name = 'quality' then
      cp.sample_density := GetFloatPart(attrib_match, re_attrib, 2, 5)
    else if attrib_name = 'angle' then
      cp.fangle := GetFloatPart(attrib_match, re_attrib, 2, 0)
    else if attrib_name = 'rotate' then // angle = -pi*x/180
      cp.vibrancy := -PI * GetFloatPart(attrib_match, re_attrib, 2, 0) / 180
    else if attrib_name = 'cam_pitch' then
      cp.cameraPitch := GetFloatPart(attrib_match, re_attrib, 2, 0)
    else if attrib_name = 'cam_yaw' then
      cp.cameraYaw := GetFloatPart(attrib_match, re_attrib, 2, 0)
    else if attrib_name = 'cam_perspective' then
      cp.cameraPersp := GetFloatPart(attrib_match, re_attrib, 2, 1)
    else if attrib_name = 'cam_dist' then  // perspective = 1/x
      begin
        cp.cameraPersp := GetFloatPart(attrib_match, re_attrib, 2, 1);
        if cp.cameraPersp = 0 then
          cp.cameraPersp := EPS;
        cp.cameraPersp := 1 / cp.cameraPersp;
      end
    else if attrib_name = 'cam_zpos' then
      cp.cameraZpos := GetFloatPart(attrib_match, re_attrib, 2, 0)
    else if attrib_name = 'cam_dof' then
      cp.cameraDOF := GetFloatPart(attrib_match, re_attrib, 2, 0)

    else if attrib_name = 'estimator_radius' then
      cp.estimator := GetFloatPart(attrib_match, re_attrib, 2, 0)
    else if attrib_name = 'estimator_minimum' then
      cp.estimator_min := GetFloatPart(attrib_match, re_attrib, 2, 0)
    else if attrib_name = 'estimator_curve' then
      cp.estimator_curve := GetFloatPart(attrib_match, re_attrib, 2, 0)
    else if attrib_name = 'enable_de' then
      cp.enable_de := GetBoolPart(attrib_match, re_attrib, 2, false)

    else if attrib_name = 'center' then
      begin
        temp2f := Get2FloatPart(attrib_match, re_attrib, 2, 0);
        cp.center[0] := temp2f.f1; cp.center[1] := temp2f.f2;
      end
    else if attrib_name = 'size' then
      begin
        temp2i := Get2IntPart(attrib_match, re_attrib, 2, 0);
        cp.Width := temp2i.i1; cp.Height := temp2i.i2;
      end
    else if attrib_name = 'background' then
      begin
        temprgb := GetRGBPart(attrib_match, re_attrib, 2, 0);
        cp.background[0] := temprgb.r;
        cp.background[1] := temprgb.g;
        cp.background[2] := temprgb.b;
        LogWrite('INFO|Read background property: [' +
          IntToStr(cp.background[0]) + ' ' +
          IntToStr(cp.background[1]) + ' ' +
          IntToStr(cp.background[2]) + ']', 'parser.log');
      end

    else if attrib_name = 'soloxform' then
      cp.soloXform := GetIntPart(attrib_match, re_attrib, 2, 0)
    else if attrib_name = 'version' then
      begin
      end
    else if attrib_name = 'plugins' then
      begin
        (*cp.used_plugins.Clear;
        find_strtokens.Subject := GetStringPart(attrib_match, re_attrib, 2, '');
        found_strtoken := find_strtokens.Match;
        strtoken_index := 0;
        while found_strtoken do begin
          strtoken_value := find_strtokens.MatchedExpression;
          cp.used_plugins.Add(strtoken_value);
          Inc(strtoken_index);
          found_strtoken := find_strtokens.MatchAgain;
        end; *)
      end
    else
      begin
        LogWrite('WARNING|' +'Unable to make sense of attribute "flame.' + attrib_name + '"', 'parser.log');
        attrib_success := false;
      end;

      if attrib_success then
        LogWrite('INFO|' +'Sucessfully interpreted attribute "flame.' + attrib_name + '" ("' +
          GetStringPart(attrib_match, re_attrib, 2, '') + '")', 'parser.log');
    
    found_attrib := find_attribs.MatchAgain;
  end;

  if LimitVibrancy and (cp.vibrancy > 1) then
    cp.vibrancy := 1;
  cp.cmapindex := -1;

  find_xforms.Subject := flame_content;
  found_xform := find_xforms.Match;
  xform_index := 0;
  cp.finalXformEnabled := false;

  while found_xform do begin
    xform_type := find_xforms.SubExpressions[1];
    xform_attribs := find_xforms.SubExpressions[2];
    if (LowerCase(xform_type) = 'xform') then begin
      LoadXFormFromXmlCompatible(find_xforms.MatchedExpression,
        false, cp.xform[xform_index], cp.finalXformEnabled);
      xform_index := xform_index + 1;
    end else begin
      cp.finalXform := Txform.Create;
      LoadXFormFromXmlCompatible(find_xforms.MatchedExpression,
        true, cp.finalXform, dummy);
      cp.finalXformEnabled := true;
      cp.useFinalXform := true;
      xform_index := xform_index + 1;
      cp.xform[cp.NumXForms] := cp.finalXform;
    end;
    found_xform := find_xforms.MatchAgain;
  end;

  find_palette.Subject := xml;
  if (find_palette.Match) then
    LoadPaletteFromXmlCompatible(find_palette.MatchedExpression, cp);

  find_strtokens.Free;
  find_attribs.Free;
  find_xforms.Free;
  find_palette.Free;
end;
procedure LoadPaletteFromXmlCompatible(xml: string; var cp: TControlPoint);
const
  re_palette: string = '<palette(.*?)>([a-f0-9\s]+)</palette>';
  re_attrib : string = '([0-9a-z_]+)="(.*?)"';
var
  i, pos, len, count: integer; c: char;
  data, attr, hexdata, format: string;
  alpha: boolean;

  find_attribs : TPerlRegEx;
  found_attrib : boolean;
  attrib_name  : string;
  attrib_match : string;
  attrib_success : Boolean;
function HexChar(c: Char): Byte;
  begin
    case c of
      '0'..'9':  Result := (Byte(c) - Byte('0'));
      'a'..'f':  Result := (Byte(c) - Byte('a')) + 10;
      'A'..'F':  Result := (Byte(c) - Byte('A')) + 10;
    else
      Result := 0;
    end;
  end;
begin
  hexdata := GetStringPart(xml, re_palette, 2, '');
  attr := GetStringPart(xml, re_palette, 1, '');

  find_attribs := TPerlRegEx.Create(nil);
  find_attribs.RegEx := re_attrib;
  find_attribs.Options := [preSingleLine, preCaseless];
  find_attribs.Subject := attr;
  found_attrib := find_attribs.Match;
  
  while found_attrib do begin
    attrib_match := find_attribs.MatchedExpression;
    attrib_name := Lowercase(find_attribs.SubExpressions[1]);
    attrib_success := true;

    if (attrib_name = 'count') then
      count := GetIntPart(attrib_match, re_attrib, 2, 256)
    else if (attrib_name = 'format') then
      format := GetStringPart(attrib_match, re_attrib, 2, 'RGB')
    else attrib_success := false;

    if (attrib_success) then
      LogWrite('INFO|' +'Sucessfully interpreted attribute "palette.' + attrib_name + '"', 'parser.log')
    else
      LogWrite('WARNING|' +'Unable to make sense of attribute "palette.' + attrib_name + '"', 'parser.log');

    found_attrib := find_attribs.MatchAgain;
  end;

  find_attribs.Free;

  alpha := (lowercase(format) = 'rgba');
  data := '';
  
  for i := 1 to Length(hexdata) do
  begin
    c := hexdata[i];
    if c in ['0'..'9']+['A'..'F']+['a'..'f'] then data := data + c;
  end;

  if alpha then len := count * 8
  else len := count * 6;

  for i := 0 to Count-1 do begin
    if alpha then pos := i * 8 + 2
    else pos := i * 6;
    cp.cmap[i][0] := 16 * HexChar(Data[pos + 1]) + HexChar(Data[pos + 2]);
    cp.cmap[i][1] := 16 * HexChar(Data[pos + 3]) + HexChar(Data[pos + 4]);
    cp.cmap[i][2] := 16 * HexChar(Data[pos + 5]) + HexChar(Data[pos + 6]);
  end;
end;
procedure LoadXFormFromXmlCompatible(xml: string; isFinalXForm: boolean; var xf: TXForm; var enabled: boolean);
const
  re_attrib : string = '([0-9a-z_]+)="(.*?)"';
  re_xform  : string = '<((?:final)?xform)(.*?)/>';
  re_coefs  : string = '([\d.eE+-]+)\s+([\d.eE+-]+)\s+([\d.eE+-]+)\s+([\d.eE+-]+)\s+([\d.eE+-]+)\s+([\d.eE+-]+)';
var
  xform_attribs: string;
  find_attribs : TPerlRegEx;
  found_attrib : boolean;
  attrib_name  : string;
  attrib_match : string;
  token_part   : string;
  i, j         : integer;
  d            : double;
  t            : TStringList;
  v_set        : Boolean;
  attrib_success: Boolean;
begin
  enabled := true;
  xform_attribs := GetStringPart(xml, re_xform, 2, '');

  find_attribs := TPerlRegEx.Create(nil);
  find_attribs.RegEx := re_attrib;
  find_attribs.Options := [preSingleLine, preCaseless];
  find_attribs.Subject := xform_attribs;
  found_attrib := find_attribs.Match;

  for i := 0 to NRVAR-1 do
    xf.SetVariation(i, 0);
  
  while found_attrib do begin
    attrib_match := find_attribs.MatchedExpression;
    attrib_name := (find_attribs.SubExpressions[1]);
    attrib_success := true;

    if (attrib_name = 'enabled') and isFinalXform then
      enabled := GetBoolPart(attrib_match, re_attrib, 2, true)
    else if (attrib_name = 'weight') and (not isFinalXform) then
      xf.density := GetFloatPart(attrib_match, re_attrib, 2, 0.5)
    else if (attrib_name = 'symmetry') and (not isFinalXform) then
      xf.symmetry := GetFloatPart(attrib_match, re_attrib, 2, 0)
    else if (attrib_name = 'color_speed') and (not isFinalXform) then
      xf.symmetry := GetFloatPart(attrib_match, re_attrib, 2, 0)
    else if (attrib_name = 'chaos') and (not isFinalXform) then
      begin
        token_part := GetStringPart(attrib_match, re_attrib, 2, '');
        if token_part <> '' then
          begin
            t := TStringList.Create;
            GetTokens(token_part, t);
            for i := 0 to t.Count-1 do
              xf.modWeights[i] := Abs(StrToFloat(t[i]));
            t.Destroy;
          end;
      end
    else if (attrib_name = 'opacity') and (not isFinalXform) then
      xf.transOpacity := GetFloatPart(attrib_match, re_attrib, 2, 1)
    else if (attrib_name = 'name') and (not isFinalXform) then
      xf.TransformName := GetStringPart(attrib_match, re_attrib, 2, '')
    else if (attrib_name = 'plotmode') and (not isFinalXform) then
      xf.transOpacity := StrToFloat(IfThen(LowerCase(GetStringPart(attrib_match, re_attrib, 2, '')) = 'off', '0', '1'))
    else if (attrib_name = 'coefs') then
      begin
        token_part := GetStringPart(attrib_match, re_attrib, 2, '1 0 0 1 0 0');
        xf.c[0][0] := GetFloatPart(token_part, re_coefs, 1, 1);
        xf.c[0][1] := GetFloatPart(token_part, re_coefs, 2, 0);
        xf.c[1][0] := GetFloatPart(token_part, re_coefs, 3, 0);
        xf.c[1][1] := GetFloatPart(token_part, re_coefs, 4, 1);
        xf.c[2][0] := GetFloatPart(token_part, re_coefs, 5, 0);
        xf.c[2][1] := GetFloatPart(token_part, re_coefs, 6, 0);
      end
    else if (attrib_name = 'post') then
      begin
        token_part := GetStringPart(attrib_match, re_attrib, 2, '1 0 0 1 0 0');
        xf.p[0][0] := GetFloatPart(token_part, re_coefs, 1, 1);
        xf.p[0][1] := GetFloatPart(token_part, re_coefs, 2, 0);
        xf.p[1][0] := GetFloatPart(token_part, re_coefs, 3, 0);
        xf.p[1][1] := GetFloatPart(token_part, re_coefs, 4, 1);
        xf.p[2][0] := GetFloatPart(token_part, re_coefs, 5, 0);
        xf.p[2][1] := GetFloatPart(token_part, re_coefs, 6, 0);
      end
    else if (attrib_name = 'color') then
      xf.color := GetFloatPart(attrib_match, re_attrib, 2, 0)
    else if (attrib_name = 'var_color') then
      xf.vc := GetFloatPart(attrib_match, re_attrib, 2, 1)
    else if ((attrib_name = 'symmetry') or (attrib_name = 'weight') or
             (attrib_name = 'color_speed') or (attrib_name = 'chaos') or
             (attrib_name = 'opacity') or (attrib_name = 'name') or
             (attrib_name = 'plotmode')) and (isFinalXForm) then
      begin
        //EmitWarning('Malformed attribute "xform.' + attrib_name + '" - ignoring');
        //LogWrite('WARNING|' +'Malformed attribute "xform.' + attrib_name + '" - ignoring', 'parser.log');
        attrib_success := false;
      end
    else begin
      if (attrib_name = 'linear3D') then begin
        xf.SetVariation(0, GetFloatPart(attrib_match, re_attrib, 2, 0));
      end else if (IsRegisteredVariation(attrib_name)) then begin
        for i := 0 to NRVAR - 1 do begin
          if lowercase(varnames(i)) = lowercase(attrib_name) then begin
            xf.SetVariation(i, GetFloatPart(attrib_match, re_attrib, 2, 0));
            v_set := true;
            break;
          end;
        end;
        if v_set then LogWrite('INFO|' +'Sucessfully interpreted attribute "xform.' + attrib_name + '" as a variation', 'parser.log')
        else if (IsRegisteredVariable(attrib_name)) then begin
          d := GetFloatPart(attrib_match, re_attrib, 2, 0);
          xf.SetVariable(attrib_name, d);
          LogWrite('INFO|' +'Sucessfully interpreted attribute "xform.' + attrib_name + '" as a variable', 'parser.log');
        end else begin
          LogWrite('WARNING|' +'Unable to make sense of attribute "xform.' + attrib_name + '"', 'parser.log');
        end;
      end else if (IsRegisteredVariable(attrib_name)) then begin
        d := GetFloatPart(attrib_match, re_attrib, 2, 0);
        xf.SetVariable(attrib_name, d);
        LogWrite('INFO|' +'Sucessfully interpreted attribute "xform.' + attrib_name + '" as a variable', 'parser.log');
      end else begin
        LogWrite('WARNING|' +'Unable to make sense of attribute "xform.' + attrib_name + '"', 'parser.log');
      end;
      attrib_success := false;
    end;

    if (attrib_success) then
      LogWrite('INFO|' +'Sucessfully interpreted attribute "xform.' + attrib_name + '"', 'parser.log');

    found_attrib := find_attribs.MatchAgain;
  end;

  if (isFinalXform) then begin
    xf.symmetry := 1;
    xf.color := 0;
  end;

  find_attribs.Free;
end;

// Replace...
function SaveCpToXmlCompatible(var xml: string; const cp1: TControlPoint): boolean;
function ColorToXmlCompact(cp1: TControlPoint): string;
var
  i: integer;
begin
  Result := '   <palette count="256" format="RGB">';
  for i := 0 to 255 do  begin
    if ((i and 7) = 0) then Result := Result + #13#10 + '      ';
    Result := Result + IntToHex(cp1.cmap[i, 0],2)
                     + IntToHex(cp1.cmap[i, 1],2)
                     + IntToHex(cp1.cmap[i, 2],2);
  end;
  Result := Result + #13#10 + '   </palette>';
end;
var
  t, i{, j}: integer;
  FileList: TStringList;
  x, y: double;
  parameters: string;
  str: string;
begin
  FileList := TStringList.create;
  x := cp1.center[0];
  y := cp1.center[1];

//  if cp1.cmapindex >= 0 then pal := pal + 'gradient="' + IntToStr(cp1.cmapindex) + '" ';

  try
    parameters := 'version="Apophysis 7X" ';
    if cp1.time <> 0 then
      parameters := parameters + format('time="%g" ', [cp1.time]);

    parameters := parameters +
      'size="' + IntToStr(cp1.width) + ' ' + IntToStr(cp1.height) +
      format('" center="%g %g" ', [x, y]) +
      format('scale="%g" ', [cp1.pixels_per_unit]);

    if cp1.FAngle <> 0 then
      parameters := parameters + format('angle="%g" ', [cp1.FAngle]) +
                                 format('rotate="%g" ', [-180 * cp1.FAngle/Pi]);
    if cp1.zoom <> 0 then
      parameters := parameters + format('zoom="%g" ', [cp1.zoom]);

// 3d
    if cp1.cameraPitch <> 0 then
      parameters := parameters + format('cam_pitch="%g" ', [cp1.cameraPitch]);
    if cp1.cameraYaw <> 0 then
      parameters := parameters + format('cam_yaw="%g" ', [cp1.cameraYaw]);
    if cp1.cameraPersp <> 0 then
      parameters := parameters + format('cam_perspective="%g" ', [cp1.cameraPersp]);
    if cp1.cameraZpos <> 0 then
      parameters := parameters + format('cam_zpos="%g" ', [cp1.cameraZpos]);
    if cp1.cameraDOF <> 0 then
      parameters := parameters + format('cam_dof="%g" ', [cp1.cameraDOF]);
//
    parameters := parameters + format(
             'oversample="%d" filter="%g" quality="%g" ',
             [cp1.spatial_oversample,
              cp1.spatial_filter_radius,
              cp1.sample_density]
             );
    if cp1.nbatches <> 1 then parameters := parameters + 'batches="' + IntToStr(cp1.nbatches) + '" ';

    parameters := parameters +
      format('background="%g %g %g" ', [cp1.background[0] / 255, cp1.background[1] / 255, cp1.background[2] / 255]) +
      format('brightness="%g" ', [cp1.brightness]) +
      format('gamma="%g" ', [cp1.gamma]);

    if cp1.vibrancy <> 1 then
      parameters := parameters + format('vibrancy="%g" ', [cp1.vibrancy]);

    if cp1.gamma_threshold <> 0 then
      parameters := parameters + format('gamma_threshold="%g" ', [cp1.gamma_threshold]);

    if cp1.soloXform >= 0 then
      parameters := parameters + format('soloxform="%d" ', [cp1.soloXform]);

    parameters := parameters +
      format('estimator_radius="%g" ', [cp1.estimator]) +
      format('estimator_minimum="%g" ', [cp1.estimator_min]) +
      format('estimator_curve="%g" ', [cp1.estimator_curve]);
    if (cp1.enable_de) then
      parameters := parameters + ('enable_de="1" ')
    else parameters := parameters + ('enable_de="0" ');

    str := '';
    for i := 0 to cp1.used_plugins.Count-1 do begin
      str := str + cp1.used_plugins[i];
      if (i = cp1.used_plugins.Count-1) then break;
      str := str + ' ';
    end;
    parameters := parameters + format('plugins="%s" ', [str]);

    FileList.Add('<flame name="' + cp1.name + '" ' + parameters + '>');
   { Write transform parameters }
    t := cp1.NumXForms;
    for i := 0 to t - 1 do
      FileList.Add(cp1.xform[i].ToXMLString);
    if cp1.HasFinalXForm then
    begin
      // 'enabled' flag disabled in this release
      FileList.Add(cp1.xform[t].FinalToXMLString(cp1.finalXformEnabled));
    end;

    { Write palette data }
    //if exporting or OldPaletteFormat then
    //  FileList.Add(ColorToXml(cp1))
    //else
      FileList.Add(ColorToXmlCompact(cp1));

    FileList.Add('</flame>');
    xml := FileList.text;
    result := true;
  finally
    FileList.free
  end;
end;

end.
