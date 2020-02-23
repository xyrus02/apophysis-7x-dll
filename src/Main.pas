unit Main;

interface
  uses
    SysUtils, Classes, Windows, Graphics, Math, RenderingInterface,
    ControlPoint, Global, RenderingCommon, RenderingImplementation,
    ImageMaker, Logging, ParameterIO, Translation,
    varGenericPlugin;

  procedure InternalUpdateParameterDependencies; stdcall;

  procedure InternalSetThreadingLevel(nthreads: integer); stdcall;
  procedure InternalSetParameterString(input : PChar); stdcall;
  procedure InternalSetBufferSavePathString(str: PChar); stdcall;
  procedure InternalSetLogEnabled(input: integer); stdcall;
  procedure InternalSetLogSavePathString(str: PChar); stdcall;
  procedure InternalSetOutputDimensions(x, y : integer); stdcall;
  procedure InternalSetSamplingParameters(os : integer; fr: double); stdcall;
  procedure InternalSetVibrancy(input : double); stdcall;
  procedure InternalSetSamplesPerPixel(input : double); stdcall;
  procedure InternalSetImageSavePaths(image, alpha: PChar); stdcall;

  procedure InternalSetOnOperationChangeCallback(input : Pointer); stdcall;
  procedure InternalSetOnProgressCallback(input : Pointer); stdcall;
  procedure InternalSetOnRequestBufferCallback(input: Pointer); stdcall;
  procedure InternalSetOnLogCallback(input: Pointer); stdcall;

  procedure InternalInitializePlugin(directory, filename: PChar); stdcall;
  procedure InternalInitializeLibrary; stdcall;
  procedure InternalDestroyLibrary; stdcall;

  function InternalGetRegisteredNameCount: integer; stdcall;
  function InternalGetRegisteredNameAt(index: integer; var str: string): integer; stdcall;

  function InternalGetRegisteredAttribCount: integer; stdcall;
  function InternalGetRegisteredAttribAt(index: integer; var str: string): integer; stdcall;

  function InternalStartRenderingProcessAndWait(dc: HDC): integer; stdcall;
  function InternalStartSamplingProcessAndWait(dc: HDC): integer; stdcall;
  function InternalStartSamplingCustomBufferAndWait(dc: HDC): integer; stdcall;
  function InternalStartSlimRenderingProcessAndWait(dc: HDC): integer; stdcall;
  procedure InternalCancelRenderingProcess; stdcall;

type
  TAsyncProgressCallback = procedure(progress: double; slice, nrslices, batch, nrbatches: integer); stdcall;
  TAsyncBufferRequestCallback = procedure(tgt: pointer; x, y: integer); stdcall;
  TAsyncOperationCallback = procedure(operation: integer); stdcall;
  TEventHandlers = class
    public
      procedure OnProgress(prog: double);
      procedure OnOperation(op: integer);
      procedure OnRequestBuffer(tgt: pointer; x, y: integer);
  end;

var
  G_Renderer: TRenderer;
  G_Flame: TControlPoint;
  G_Bitmap : TBitmap;
  G_ImageMaker : TImageMaker;
  G_Handler : TEventHandlers;
  G_Log : TStringList;
  G_Vars: TStringList;
  G_Attribs: TStringList;

  G_BufferPath : String;
  G_LogPath : String;
  G_ImagePath : String;
  G_AlphaPath : String;

  G_SizeX : integer;
  G_SizeY : integer;

  G_SamplesPerPixel : double;
  G_Oversample : integer;
  G_FilterRadius : double;
  G_Vibrancy : double;
  G_SetVibrancy : boolean;

  G_OnProgress : TAsyncProgressCallback;
  G_OnOperation : TAsyncOperationCallback;
  G_OnReqBuffer : TAsyncBufferRequestCallback;

  G_Seed : integer;

implementation
  uses XForm, XFormMan;

  procedure TEventHandlers.OnProgress(prog: double);
  begin
    if (@G_OnProgress <> nil) then
      G_OnProgress(prog,
        G_Renderer.Slice, G_Renderer.NrSlices,
        G_Renderer.Batch, G_Renderer.NrBatches);
  end;
  procedure TEventHandlers.OnOperation(op: integer);
  begin
    if (@G_OnOperation <> nil) then
      G_OnOperation(op);
  end;
  procedure TEventHandlers.OnRequestBuffer(tgt: pointer; x, y: integer);
  begin
    if (@G_OnReqBuffer <> nil) then
      G_OnReqBuffer(tgt, x, y);
  end;
////////////////////////////////////////////////////////////////////////////////////////
  procedure InternalSetThreadingLevel(nthreads: integer); stdcall;
  begin
    G_Renderer.NrThreads := nthreads;
  end;
  procedure InternalSetOutputDimensions(x, y : integer); stdcall;
  begin
    G_SizeX := x;
    G_SizeY := y;
  end;
  procedure InternalSetSamplingParameters(os : integer; fr: double); stdcall;
  begin
    G_Oversample := os;
    G_FilterRadius := fr;
  end;
  procedure InternalSetSamplesPerPixel(input : double); stdcall;
  begin
    G_SamplesPerPixel := input;
  end;
  procedure InternalSetOnOperationChangeCallback(input : Pointer); stdcall;
  begin
    Pointer(G_OnOperation) := input;
  end;
  procedure InternalSetOnProgressCallback(input : Pointer); stdcall;
  begin
    Pointer(G_OnProgress) := input;
  end;
  procedure InternalSetOnRequestBufferCallback(input: Pointer); stdcall;
  begin
    Pointer(G_OnReqBuffer) := input;
  end;
  procedure InternalSetOnLogCallback(input: Pointer); stdcall;
  begin
    Pointer(G_OnLog) := input;
  end;
  procedure InternalSetBufferSavePathString(str: PChar); stdcall;
  begin
    if (str = nil) then G_BufferPath := ''
    else G_BufferPath := str;
  end;
  procedure InternalSetLogSavePathString(str: PChar); stdcall;
  begin
    if (str = nil) then G_LogPath := '.'
    else G_LogPath := str;
    LogSetPath(str);
  end;
  procedure InternalSetLogEnabled(input: integer); stdcall;
  begin
    G_DisableLog := (input = 0);
  end;
  procedure InternalSetImageSavePaths(image, alpha: PChar); stdcall;
  begin
    if (image = nil) then G_ImagePath := ''
    else G_ImagePath := image;
    if (alpha = nil) then G_AlphaPath := ''
    else G_AlphaPath := alpha;
  end;
  procedure InternalSetVibrancy(input: double); stdcall;
  begin
    G_SetVibrancy := true;
    G_Vibrancy := input;
  end;
  procedure InternalSetParameterString(input : PChar); stdcall;
  var
    status: string;
    w, h, r : Double;
    temp_sl : TStringList;
  begin
    FormatSettings.DecimalSeparator := '.';

    LogWrite('INFO|Reading parameters', 'general.log');
    if (G_Flame <> nil) then G_Flame.Destroy;
    G_Flame := TControlPoint.Create;
    LoadCpFromXmlCompatible(input, G_Flame, status);

    InternalUpdateParameterDependencies;
  end;
  procedure InternalUpdateParameterDependencies; stdcall;
  begin
    if G_SizeX <= 0 then G_SizeX := G_Flame.Width;
    if G_SizeY <= 0 then G_SizeY := G_Flame.Height;
    if G_Oversample <= 0 then G_Oversample := G_Flame.spatial_oversample;

    if (G_Flame <> nil) then
    begin
      G_Flame.AdjustScale(G_SizeX, G_SizeY);
      G_Flame.Width := G_SizeX;
      G_Flame.Height := G_SizeY;
      G_Flame.spatial_oversample := G_Oversample;
      G_Flame.spatial_filter_radius := G_FilterRadius;
      G_Flame.sample_density := G_SamplesPerPixel;
      G_Flame.transparency := false;
    end;

    if (G_Renderer <> nil) then G_Renderer.SetCP(G_Flame);
    if (G_ImageMaker <> nil) then G_ImageMaker.SetCP(G_Flame);
  end;
////////////////////////////////////////////////////////////////////////////////////////
  procedure InternalInitializePlugin(directory, filename: PChar); stdcall;
  begin
    InitializePlugin(directory, filename);
  end;
  procedure InternalInitializeLibrary; stdcall;
  begin
    FormatSettings.DecimalSeparator := '.';

    LogWrite('INFO|Loading string tables', 'general.log');
    LoadEnglish;
    //InitializePlugins(G_PluginPath);

    G_Log := TStringList.Create;

    G_Seed := 123456;

    LogWrite('INFO|Initializing engine (' + APP_NAME + ' ' + APP_VERSION + ')', 'general.log');
    G_Bitmap := TBitmap.Create;
    G_Renderer := TRenderer.Create;
    G_Flame := TControlPoint.Create;

    G_Bitmap.PixelFormat := pf24bit;
    G_Bitmap.HandleType := bmDIB;
    G_Bitmap.Width := G_SizeX;
    G_Bitmap.Height := G_SizeY;
    G_Bitmap.Canvas.Brush.Color := $000000;

    G_Handler := TEventHandlers.Create;
    G_ImageMaker := TImageMaker.Create;
  end;
  procedure InternalDestroyLibrary; stdcall;
  begin
    if (G_Log <> nil) then
      LogWrite('INFO|Terminating engine', 'general.log');

    if (G_Renderer <> nil) then
      G_Renderer.Free; G_Renderer := nil;

    if (G_Flame <> nil) then
      G_Flame.Free; G_Flame := nil;

    if (G_Bitmap <> nil) then
      G_Bitmap.Free; G_Bitmap := nil;

    if (G_Log <> nil) then
      G_Log.Free; G_Log := nil;

    if (G_ImageMaker <> nil) then
      G_ImageMaker.Free; G_ImageMaker := nil;

    if (G_Vars <> nil) then
      G_Vars.Free; G_Vars := nil;

    if (G_Attribs <> nil) then
      G_Attribs.Free; G_Attribs := nil;

    G_SetVibrancy := false;
  end;
  function InternalGetRegisteredNameCount: integer; stdcall;
  var
    i, count: integer;
    xf: txform;
  begin
    if (G_Vars = nil) then begin
      G_Vars := TStringList.Create;

      xf := txform.Create;
      xf.Destroy;

      count:=NrVar;
      for i:=0 to count - 1 do
        G_Vars.Add(varnames(i));

      count:=GetNrVariableNames;
      for i:=0 to count - 1 do
        G_Vars.Add(GetVariableNameAt(i))
    end;

    Result := G_Vars.Count;
  end;
  function InternalGetRegisteredNameAt(index: integer; var str: string): integer; stdcall;
  var count : integer;
  begin
    count := InternalGetRegisteredNameCount;

    if (index < 0) or (index >= count) then str := ''
    else str := G_Vars.Strings[index];

    Result := Length(str);
  end;
  function InternalGetRegisteredAttribCount: integer; stdcall;
  var
    i, count: integer;
    xf: txform;
  begin
    if (G_Attribs = nil) then begin
      G_Attribs := TStringList.Create;

      G_Attribs.Add('weight');
      G_Attribs.Add('color');
      G_Attribs.Add('symmetry');
      G_Attribs.Add('color_speed');
      G_Attribs.Add('coefs');
      G_Attribs.Add('chaos');
      G_Attribs.Add('plotmode');
      G_Attribs.Add('opacity');
      G_Attribs.Add('post');
      G_Attribs.Add('var');
      G_Attribs.Add('var1');
      G_Attribs.Add('var_color');
      G_Attribs.Add('name');
      G_Attribs.Add('linear3D');
    end;

    Result := G_Attribs.Count;
  end;
  function InternalGetRegisteredAttribAt(index: integer; var str: string): integer; stdcall;
  var count : integer;
  begin
    count := InternalGetRegisteredAttribCount;

    if (index < 0) or (index >= count) then str := ''
    else str := G_Attribs.Strings[index];

    Result := Length(str);
  end;
////////////////////////////////////////////////////////////////////////////////////////
  procedure PaintToDc(dc: HDC; bmp: TBitmap);
  var
    cvr: TRect;
  begin
    cvr.Left := 0; cvr.Top := 0; cvr.Right := G_SizeX; cvr.Bottom := G_SizeY;
    G_Bitmap.Canvas.FillRect(cvr);
    G_Bitmap.Canvas.Draw(
      round(G_SizeX / 2 - G_Flame.Width / 2),
      round(G_SizeY / 2 - G_Flame.Height / 2),
      bmp);
    if dc <> 0 then begin
      //LogWrite('INFO|Painting image to target DC', 'render.log');
      BitBlt(dc, 0, 0, G_SizeX, G_SizeY, G_Bitmap.Canvas.Handle, 0, 0, $CC0020 { ROP_SRCCOPY });
    end else begin
      //LogWrite('INFO|Skipping DC output', 'render.log');
    end;

    if (G_ImagePath <> '') then begin
      //LogWrite('INFO|Saving image to target file ("' + G_ImagePath + '")', 'render.log');
      bmp{G_Bitmap}.SaveToFile(G_ImagePath);
    end else begin
      //LogWrite('INFO|Skipping file output', 'render.log');
    end;

    LogWrite('INFO|' + IntToStr(G_SizeX * G_SizeY * 4) + ' bytes written to output stream', 'render.log');
  end;

  procedure InternalCancelRenderingProcess; stdcall;
  begin
    G_Renderer.Stop;
  end;
  function InternalStartSlimRenderingProcessAndWait(dc: HDC): integer; stdcall;
  var
    cvr: TRect;
    bmp: TBitmap;
    bbltres: LongBool;
  begin
    cvr.Left := 0; cvr.Top := 0; cvr.Right := G_SizeX; cvr.Bottom := G_SizeY;
    if (G_SetVibrancy) then G_Flame.vibrancy := 1
    else G_Flame.vibrancy := G_Vibrancy;

    try
      G_Renderer.SlimRender;

      bmp := G_Renderer.GetImage;
      if (dc <> 0) then
        bbltres := BitBlt(dc, 0, 0, G_SizeX, G_SizeY, bmp.Canvas.Handle, 0, 0, $CC0020)
      else bbltres := true;
      if not bbltres then
        raise Exception.Create(SysErrorMessage(GetLastError));
    except on exc : Exception do
      begin
        LogWrite('ERROR|' + exc.Message, 'general.log');
        LogWrite('ERROR|Internal error while rendering', 'general.log');
        Result := 1;
        Exit;
      end;
    end;
  end;

  function InternalStartRenderingProcessAndWait(dc: HDC): integer; stdcall;
  var
    cvr : TRect;
  begin
    cvr.Left := 0; cvr.Top := 0; cvr.Right := G_SizeX; cvr.Bottom := G_SizeY;
    if (G_SetVibrancy) then G_Flame.vibrancy := 1
    else G_Flame.vibrancy := G_Vibrancy;

    G_Renderer.OnProgress := @G_Handler.OnProgress;
    G_Renderer.OnOperation := @G_Handler.OnOperation;
    G_Renderer.BufferPath := G_BufferPath;

    try
      G_Renderer.Render;
    except on exc : Exception do
      begin
        LogWrite('ERROR|' + exc.Message, 'general.log');
        LogWrite('ERROR|Internal error while rendering', 'general.log');
        Result := 1;
        Exit;
      end;
    end;

    //{$ifdef ALPHA}
    //  PaintToDcAlpha(dc, G_Renderer.GetTransparentImage);
    //{$else}
      PaintToDc(dc, G_Renderer.GetImage);
    //{$endif}

    Result := 0;
  end;
  function InternalStartSamplingProcessAndWait(dc: HDC): integer; stdcall;
  var
    cvr : TRect;
  begin
    cvr.Left := 0; cvr.Top := 0; cvr.Right := G_SizeX; cvr.Bottom := G_SizeY;

    try
      G_Renderer.IntermediateSample(G_ImageMaker);
    except on exc : Exception do
      begin
        LogWrite('ERROR|' + exc.Message, 'general.log');
        LogWrite('ERROR|Internal error while saving image', 'general.log');
        Result := 1;
        Exit;
      end;
    end;

    {$ifdef ALPHA}
      PaintToDcAlpha(dc, G_ImageMaker.GetTransparentImage);
    {$else}
      PaintToDc(dc, G_ImageMaker.GetImage);
    {$endif}

    Result := 0;
  end;
  function InternalStartSamplingCustomBufferAndWait(dc: HDC): integer; stdcall;
  var
    cvr : TRect;
  begin
    cvr.Left := 0; cvr.Top := 0; cvr.Right := G_SizeX; cvr.Bottom := G_SizeY;

    try
      //LogWrite('WARNING|Due to limitations of this version, vibrancy is set to "1"', 'general.log');
      if (G_SetVibrancy) then G_Flame.vibrancy := 1
      else G_Flame.vibrancy := G_Vibrancy;
      
      G_Renderer.OnProgress := @G_Handler.OnProgress;
      G_Renderer.OnOperation := @G_Handler.OnOperation;
      G_Renderer.CopyBufferCallback := @G_Handler.OnRequestBuffer;
      //G_Renderer.BufferPath := '';

      G_Renderer.ProcessBuffer(G_SamplesPerPixel);
    except on exc : Exception do
      begin
        LogWrite('ERROR|' + exc.Message, 'general.log');
        LogWrite('ERROR|Internal error while saving image', 'general.log');
        Result := 1;
        Exit;
      end;
    end;

    {$ifdef ALPHA}
      PaintToDcAlpha(dc, G_Renderer.GetTransparentImage);
    {$else}
      PaintToDc(dc, G_Renderer.GetImage);
    {$endif}
    Result := 0;
  end;
end.
