// aporender.dpr
// (c) 2011 Georg Kiehne

library aporender;

uses
  SysUtils,
  Classes,
  Windows,
  Graphics,
  Forms,
  Math,
  AsmRandom in 'System\AsmRandom.pas',
  GradientHlpr in 'ColorMap\GradientHlpr.pas',
  cmap in 'ColorMap\cmap.pas',
  cmapdata in 'ColorMap\cmapdata.pas',
  XFormMan in 'Core\XFormMan.pas',
  BaseVariation in 'Core\BaseVariation.pas',
  Global in 'Core\Global.pas',
  NativeXml in 'System\NativeXml.pas',
  NativeXmlAppend in 'System\NativeXmlAppend.pas',
  NativeXmlObjectStorage in 'System\NativeXmlObjectStorage.pas',
  RegexHelper in 'System\RegexHelper.pas',
  Translation in 'Core\Translation.pas',
  XForm in 'Flame\XForm.pas',
  ControlPoint in 'Flame\ControlPoint.pas',
  RndFlame in 'Flame\RndFlame.pas',
  Regstry in 'IO\Regstry.pas',
  Binary in 'IO\Binary.pas',
  Hibernation in 'IO\Hibernation.pas',
  RenderingCommon in 'Renderer\RenderingCommon.pas',
  BucketFillerThread in 'Renderer\BucketFillerThread.pas',
  ImageMaker in 'Renderer\ImageMaker.pas',
  RenderingImplementation in 'Renderer\RenderingImplementation.pas',
  RenderingInterface in 'Renderer\RenderingInterface.pas',

  varHemisphere in 'Variations\varHemisphere.pas',
  varLog in 'Variations\varLog.pas',
  varPolar2 in 'Variations\varPolar2.pas',
  varRings2 in 'Variations\varRings2.pas',
  varFan2 in 'Variations\varFan2.pas',
  varCross in 'Variations\varCross.pas',
  varWedge in 'Variations\varWedge.pas',
{*} varEpispiral in 'Variations\varEpispiral.pas',
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
  // falloff2
  varRectangles in 'Variations\varRectangles.pas',
  varSplits in 'Variations\varSplits.pas',
  // separation
  // bipolar
  // loonie
  // escher
  // scry
  // ngon
  // foci
  // lazysusan
  // spher
  // mobius
  // flux
  // shredrad
  varElliptic in 'Variations\varElliptic.pas',
  // waves2
  // auger
  // glynnsim2
  varPreSpherical in 'Variations\varPreSpherical.pas',
  varPreSinusoidal in 'Variations\varPreSinusoidal.pas',
  varPreDisc in 'Variations\varPreDisc.pas',
  varPreBwraps in 'Variations\varPreBwraps.pas',
  // pre_crop
  varPostBwraps in 'Variations\varPostBwraps.pas',
  varPostCurl in 'Variations\varPostCurl.pas',
  // post_crop
  varGenericPlugin in 'Variations\varGenericPlugin.pas',

  ParameterIO in 'IO\ParameterIO.pas',
  MissingPlugin in 'IO\MissingPlugin.pas',
  Diagnostics in 'Core\Diagnostics.pas',
  Main in 'Main.pas';

//implementation
  procedure ParametersSetParameterString(input : PChar); stdcall; export;
  begin
    InternalSetParameterString(input);
  end;
  procedure ParametersSetPluginSearchPathString(input: PChar); stdcall; export;
  begin
    InternalSetPluginSearchPathString(input);
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

  function ApophysisGetRegisteredNameCount: integer; stdcall; export;
  begin
    Result := InternalGetRegisteredNameCount;
  end;
  function ApophysisGetRegisteredNameAt(index: integer): PChar; stdcall; export;
  begin
    Result := InternalGetRegisteredNameAt(index);
  end;
  function ApophysisGetRegisteredAttribCount: integer; stdcall; export;
  begin
    Result := InternalGetRegisteredAttribCount;
  end;
  function ApophysisGetRegisteredAttribAt(index: integer): PChar; stdcall; export;
  begin
    Result := InternalGetRegisteredAttribAt(index);
  end;

exports
  ApophysisStartRenderingProcessAndWait,
  ApophysisStartSamplingProcessAndWait,
  ApophysisStartSamplingCustomBufferAndWait,
  ApophysisSetThreadingLevel,
  ApophysisInitializePlugin,
  ApophysisInitializeLibrary,
  ApophysisDestroyLibrary,
  ApophysisGetRegisteredNameCount,
  ApophysisGetRegisteredNameAt,
  ApophysisGetRegisteredAttribCount,
  ApophysisGetRegisteredAttribAt,
  
  ParametersSetParameterString,
  ParametersSetBufferSavePathString,
  ParametersSetPluginSearchPathString,
  ParametersSetLogSavePathString,
  ParametersSetOutputDimensions,
  ParametersSetSamplingParameters,
  ParametersSetSamplesPerPixel,
  ParametersSetVibrancy,
  ParametersSetImagePaths,

  EventsSetOnOperationChangeCallback,
  EventsSetOnProgressCallback,
  EventsSetOnRequestBufferCallback;
  
begin end.

