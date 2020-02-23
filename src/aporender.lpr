// aporender.dpr
// (c) 2011 Georg Kiehne

library aporender;

uses
  Forms,
  Dialogs,
  SysUtils,
  Graphics,
  Windows,
  Interfaces,
  Binary in 'IO\Binary.pas',
  Translation in 'Core\Translation.pas',
  RegexHelper in 'System\RegexHelper.pas',
  RexExpr in 'System\RegExpr.pas',
  Global in 'Core\Global.pas',
  MissingPlugin in 'IO\MissingPlugin.pas',
  Regstry in 'IO\Regstry.pas',
  ParameterIO in 'IO\ParameterIO.pas',
  Bezier in 'Core\Bezier.pas',
  RndFlame in 'Flame\RndFlame.pas',
  ControlPoint in 'Flame\ControlPoint.pas',
  cmapdata in 'ColorMap\cmapdata.pas',
  cmap in 'ColorMap\cmap.pas',
  GradientHlpr in 'ColorMap\GradientHlpr.pas',
  XFormMan in 'Core\XFormMan.pas',
  XForm in 'Flame\XForm.pas',
  BaseVariation in 'Core\BaseVariation.pas',
  RenderingCommon in 'Renderer\RenderingCommon.pas',
  RenderingInterface in 'Renderer\RenderingInterface.pas',
  RenderingImplementation in 'Renderer\RenderingImplementation.pas',
  BucketFillerThread in 'Renderer\BucketFillerThread.pas',
  ImageMaker in 'Renderer\ImageMaker.pas',
  varHemisphere in 'Variations\varHemisphere.pas',
  varLog in 'Variations\varLog.pas',
  varPolar2 in 'Variations\varPolar2.pas',
  varRings2 in 'Variations\varRings2.pas',
  varFan2 in 'Variations\varFan2.pas',
  varCross in 'Variations\varCross.pas',
  varWedge in 'Variations\varWedge.pas',
  varEpispiral in 'Variations\varEpispiral.pas',
  varBwraps in 'Variations\varBwraps.pas',
  varPDJ in 'Variations\varPDJ.pas',
  varJuliaN in 'Variations\varJuliaN.pas',
  varJuliaScope in 'Variations\varJuliaScope.pas',
  varJulia3Djf in 'Variations\varJulia3Djf.pas',
  varJulia3Dz in 'Variations\varJulia3Dz.pas',
  varCurl in 'Variations\varCurl.pas',
  varCurl3D in 'Variations\varCurl3D.pas',
  varRadialBlur in 'Variations\varRadialBlur.pas',
  varBlurCircle in 'Variations\varBlurCircle.pas',
  varBlurZoom in 'Variations\varBlurZoom.pas',
  varBlurPixelize in 'Variations\varBlurPixelize.pas',
  varFalloff2 in 'Variations\varFalloff2.pas',
  varRectangles in 'Variations\varRectangles.pas',
  varSplits in 'Variations\varSplits.pas',
  varSeparation in 'Variations\varSeparation.pas',
  varBipolar in 'Variations\varBipolar.pas',
  varLoonie in 'Variations\varLoonie.pas',
  varEscher in 'Variations\varEscher.pas',
  varScry in 'Variations\varScry.pas',
  varNGon in 'Variations\varNGon.pas',
  varFoci in 'Variations\varFoci.pas',
  varLazysusan in 'Variations\varLazysusan.pas',
  varMobius in 'Variations\varMobius.pas',
  varCrop in 'Variations\varCrop.pas',
  varElliptic in 'Variations\varElliptic.pas',
  varWaves2 in 'Variations\varWaves2.pas',
  varAuger in 'Variations\varAuger.pas',
  varPreSpherical in 'Variations\varPreSpherical.pas',
  varPreSinusoidal in 'Variations\varPreSinusoidal.pas',
  varPreDisc in 'Variations\varPreDisc.pas',
  varPreBwraps in 'Variations\varPreBwraps.pas',
  varPreCrop in 'Variations\varPreCrop.pas',
  varPreFalloff2 in 'Variations\varPreFalloff2.pas',
  varPostBwraps in 'Variations\varPostBwraps.pas',
  varPostCurl in 'Variations\varPostCurl.pas',
  varPostCurl3D in 'Variations\varPostCurl3D.pas',
  varPostCrop in 'Variations\varPostCrop.pas',
  varPostFalloff2 in 'Variations\varPostFalloff2.pas',
  varGenericPlugin in 'Variations\varGenericPlugin.pas',
  Logging in 'Core\Logging.pas',
  Main in 'Main.pas',
  Params in 'Params.pas';

//implementation
  procedure ParametersUpdateDependencies; stdcall; export;
  begin
    InternalUpdateParameterDependencies;
  end;
  procedure ParametersSetParameterString(input : PChar); stdcall; export;
  begin
    InternalSetParameterString(input);
  end;
  procedure ParametersSetLogSavePathString(input: PChar); stdcall; export;
  begin
    InternalSetLogSavePathString(input);
  end;
  procedure ParametersSetBufferSavePathString(input: PChar); stdcall; export;
  begin
    InternalSetBufferSavePathString(input);
  end;
  procedure ParametersSetOutputDimensions(x, y : integer); stdcall; export;
  begin
    InternalSetOutputDimensions(x, y);
  end;
  procedure ParametersSetSamplingParameters(os : integer; fr: double); stdcall; export;
  begin
    InternalSetSamplingParameters(os, fr);
  end;
  procedure ParametersSetSamplesPerPixel(input : double); stdcall; export;
  begin
    InternalSetSamplesPerPixel(input);
  end;
  procedure ParametersSetVibrancy(input: double); stdcall; export;
  begin
    InternalSetVibrancy(input);
  end;
  procedure ParametersSetImagePaths(image, alpha: PChar); stdcall; export;
  begin
    InternalSetImageSavePaths(image, alpha);
  end;

  procedure EventsSetOnOperationChangeCallback(input : Pointer); stdcall; export;
  begin
    InternalSetOnOperationChangeCallback(input);
  end;
  procedure EventsSetOnProgressCallback(input : Pointer); stdcall; export;
  begin
    InternalSetOnProgressCallback(input);
  end;
  procedure EventsSetOnRequestBufferCallback(input : Pointer); stdcall; export;
  begin
    InternalSetOnRequestBufferCallback(input);
  end;
  procedure EventsSetOnLogCallback(input : Pointer); stdcall; export;
  begin
    InternalSetOnLogCallback(input);
  end;

  procedure ApophysisSetThreadingLevel(nthreads: integer); stdcall; export;
  begin
    InternalSetThreadingLevel(nthreads);
  end;
  procedure ApophysisInitializePlugin(directory, name: PChar); stdcall; export;
  begin
    InternalInitializePlugin(directory, name);
  end;
  procedure ApophysisInitializeLibrary; stdcall; export;
  begin
    InternalInitializeLibrary;
  end;
  procedure ApophysisDestroyLibrary; stdcall; export;
  begin
    InternalDestroyLibrary;
  end;
  procedure ApophysisSetLogEnabled(input: integer); stdcall; export;
  begin
    InternalSetLogEnabled(input);
  end;

  function ApophysisStartRenderingProcessAndWait(dc: HDC): integer; stdcall; export;
  begin
    Result := InternalStartRenderingProcessAndWait(dc);
  end;
  function ApophysisStartSamplingProcessAndWait(dc: HDC): integer; stdcall; export;
  begin
    Result := InternalStartSamplingProcessAndWait(dc);
  end;
  function ApophysisStartSamplingCustomBufferAndWait(dc: HDC): integer; stdcall; export;
  begin
    Result := InternalStartSamplingCustomBufferAndWait(dc);
  end;
  function ApophysisStartSlimRenderingProcessAndWait(dc: HDC): integer; stdcall; export;
  begin
    Result := InternalStartSlimRenderingProcessAndWait(dc);
  end;
  procedure ApophysisCancelRenderingProcess; stdcall; export;
  begin
    InternalCancelRenderingProcess;
  end;

  function ApophysisGetRegisteredNameCount: integer; stdcall; export;
  begin
    Result := InternalGetRegisteredNameCount;
  end;
  function ApophysisGetRegisteredNameAt(index: integer; var buf: string): integer; stdcall; export;
  var str: string;
  //var chars: PChar;
  begin
    str := '';
    Result := InternalGetRegisteredNameAt(index, str);  //chars := PChar(str);
    Buf := str; //Move(chars, buf, Result * SizeOf(Char));
  end;
  function ApophysisGetRegisteredAttribCount: integer; stdcall; export;
  begin
    Result := InternalGetRegisteredAttribCount;
  end;
  function ApophysisGetRegisteredAttribAt(index: integer; var buf: string): integer; stdcall; export;
  var str: string;
  //var chars: PChar;
  begin
    str := '';
    Result := InternalGetRegisteredAttribAt(index, str);  //chars := PChar(str);
    Buf := str; //Move(chars, buf, Result * SizeOf(Char));
  end;

  procedure FLName(v: PChar); stdcall; export; begin Params.SetName(String(v)); end;
  procedure FLBrightness(v: Double); stdcall; export; begin Params.SetBrightness(v); end;
  procedure FLVibrancy(v: Double); stdcall; export; begin Params.SetVibrancy(v); end;
  procedure FLGamma(v: Double); stdcall; export; begin Params.SetGamma(v); end;
  procedure FLGammaThreshold(v: Double); stdcall; export; begin Params.SetGammaThreshold(v); end;
  procedure FLOversample(v: Integer); stdcall; export; begin Params.SetOversample(v); end;
  procedure FLFilter(v: Double); stdcall; export; begin Params.SetFilter(v); end;
  procedure FLZoom(v: Double); stdcall; export; begin Params.SetZoom(v); end;
  procedure FLScale(v: Double); stdcall; export; begin Params.SetScale(v); end;
  procedure FLQuality(v: Double); stdcall; export; begin Params.SetQuality(v); end;
  procedure FLAngle(v: Double); stdcall; export; begin Params.SetAngle(v); end;
  procedure FLRotate(v: Double); stdcall; export; begin Params.SetRotate(v); end;
  procedure FLCamPitch(v: Double); stdcall; export; begin Params.SetCamPitch(v); end;
  procedure FLCamYaw(v: Double); stdcall; export; begin Params.SetCamYaw(v); end;
  procedure FLCamPerspective(v: Double); stdcall; export; begin Params.SetCamPerspective(v); end;
  procedure FLCamDist(v: Double); stdcall; export; begin Params.SetCamDist(v); end;
  procedure FLCamZPos(v: Double); stdcall; export; begin Params.SetCamZPos(v); end;
  procedure FLCamDof(v: Double); stdcall; export; begin Params.SetCamDof(v); end;
  procedure FLEstimatorRadius(v: Double); stdcall; export; begin Params.SetEstimatorRadius(v); end;
  procedure FLEstimatorMinimum(v: Double); stdcall; export; begin Params.SetEstimatorMinimum(v); end;
  procedure FLEstimatorCurve(v: Double); stdcall; export; begin Params.SetEstimatorCurve(v); end;
  procedure FLEnableDE(v: Integer); stdcall; export; begin Params.SetEnableDE(v <> 0); end;
  procedure FLCenter(v1, v2: Double); stdcall; export; begin Params.SetCenter(v1, v2); end;
  procedure FLSize(v1, v2: Double); stdcall; export; begin Params.SetSize(v1, v2); end;
  procedure FLBackground(v1, v2, v3: Integer); stdcall; export; begin Params.SetBackground(v1, v2, v3); end;
  procedure FLSoloXForm(v: Integer); stdcall; export; begin Params.SetSoloXForm(v); end;
  procedure FLCMap(i, r, g, b: integer); stdcall; export; begin Params.SetCMapPos(i, r, g, b); end;

  procedure FLFinalEnabled(v: Integer); stdcall; export; begin Params.SetFinalEnabled(v <> 0); end;
  procedure FLFinalCoefs(i: Integer; v: Double); stdcall; export; begin Params.SetFinalCoefs(i, v); end;
  procedure FLFinalPost(i: Integer; v: Double); stdcall; export; begin Params.SetFinalPost(i, v); end;
  procedure FLFinalColor(v: Double); stdcall; export; begin Params.SetFinalColor(v); end;
  procedure FLFinalVarColor(v: Double); stdcall; export; begin Params.SetFinalVarColor(v); end;
  function  FLFinalVar(k: PChar; v: Double): Boolean; stdcall; export; begin Result := Params.SetFinalVar(String(k), v); end;

  procedure FLXName(i: Integer; v: PChar); stdcall; export; begin Params.SetXName(i, String(v)); end;
  procedure FLXWeight(i: Integer; v: Double); stdcall; export; begin Params.SetXWeight(i, v); end;
  procedure FLXColorSpeed(i: Integer; v: Double); stdcall; export; begin Params.SetXColorSpeed(i, v); end;
  procedure FLXChaos(i, j: Integer; v: Double); stdcall; export; begin Params.SetXChaos(i, j, v); end;
  procedure FLXOpacity(i: Integer; v: Double); stdcall; export; begin Params.SetXOpacity(i, v); end;
  procedure FLXCoefs(i, j: Integer; v: Double); stdcall; export; begin Params.SetXCoefs(i, j, v); end;
  procedure FLXPost(i, j: Integer; v: Double); stdcall; export; begin Params.SetXPost(i, j, v); end;
  procedure FLXColor(i: Integer; v: Double); stdcall; export; begin Params.SetXColor(i, v); end;
  procedure FLXVarColor(i: Integer; v: Double); stdcall; export; begin Params.SetXVarColor(i, v); end;
  function  FLXVar(i: Integer; k: PChar; v: Double): Boolean; stdcall; export; begin Result := Params.SetXVar(i, String(k), v); end;

  procedure FLReset; stdcall; export; begin Params.Reset; end;
  procedure FLApplyFinal; stdcall; export; begin Params.ApplyFinal; end;

  function  FLToString(var buf: string): integer; stdcall; export; begin
    Buf := Params.FlameToString;
    Result := Length(Buf);
  end;
  function  FLRandomToFile(fn: string; batchSize, width, height: integer): integer; stdcall; export; var r : boolean; begin
    r := Params.WriteBatchTo(fn, batchSize, width, height);
    if r then Result := 1
    else Result := 0;
  end;

exports
  ApophysisStartRenderingProcessAndWait,
  ApophysisStartSamplingProcessAndWait,
  ApophysisStartSamplingCustomBufferAndWait,
  ApophysisStartSlimRenderingProcessAndWait,
  ApophysisCancelRenderingProcess,
  ApophysisSetThreadingLevel,
  ApophysisInitializePlugin,
  ApophysisInitializeLibrary,
  ApophysisDestroyLibrary,
  ApophysisGetRegisteredNameCount,
  ApophysisGetRegisteredNameAt,
  ApophysisGetRegisteredAttribCount,
  ApophysisGetRegisteredAttribAt,
  ApophysisSetLogEnabled,

  ParametersUpdateDependencies,
  ParametersSetParameterString,
  ParametersSetBufferSavePathString,
  ParametersSetLogSavePathString,
  ParametersSetOutputDimensions,
  ParametersSetSamplingParameters,
  ParametersSetSamplesPerPixel,
  ParametersSetVibrancy,
  ParametersSetImagePaths,

  EventsSetOnOperationChangeCallback,
  EventsSetOnProgressCallback,
  EventsSetOnRequestBufferCallback,
  EventsSetOnLogCallback,

  FLName,
  FLVibrancy,
  FLBrightness,
  FLGamma,
  FLGammaThreshold,
  FLOversample,
  FLFilter,
  FLZoom,
  FLScale,
  FLQuality,
  FLAngle,
  FLRotate,
  FLCamPitch,
  FLCamYaw,
  FLCamPerspective,
  FLCamDist,
  FLCamZPos,
  FLCamDof,
  FLEstimatorRadius,
  FLEstimatorMinimum,
  FLEstimatorCurve,
  FLEnableDE,
  FLCenter,
  FLSize,
  FLBackground,
  FLSoloXForm,
  FLCMap,

  FLFinalEnabled,
  FLFinalCoefs,
  FLFinalPost,
  FLFinalColor,
  FLFinalVarColor,
  FLFinalVar,

  FLXName,
  FLXWeight,
  FLXColorSpeed,
  FLXChaos,
  FLXOpacity,
  FLXCoefs,
  FLXPost,
  FLXColor,
  FLXVarColor,
  FLXVar,

  FLReset,
  FLApplyFinal,
  FLToString,
  FLRandomToFile;
begin end.


