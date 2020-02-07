unit MissingPlugin;

interface
  uses Windows, Global, Classes, ComCtrls, SysUtils,
       ControlPoint, Translation;
  const RegisteredAttributes : array[0..12] of string = (
    'weight', 'color', 'symmetry', 'color_speed', 'coefs', 'chaos',
    'plotmode', 'opacity', 'post', 'var', 'var1', 'var_color',
    'name'
  );
  var  MissingPluginList : TStringList;
       Parsing : boolean;
       ErrorMessageString : string;

  procedure BeginParsing;
  procedure CheckAttribute(attr:string);
  function EndParsing(cp : TControlPoint; var statusPanelText : string): boolean;
  procedure AnnoyUser;
implementation

  procedure BeginParsing;
  begin
    MissingPluginList := TStringList.Create;
  end;

  procedure CheckAttribute(attr:string);
  var i : integer;
  begin
    for i := 0 to Length(RegisteredAttributes)-1 do
      if attr=RegisteredAttributes[i] then exit;
        
    if MissingPluginList.IndexOf(attr) < 0 then
      MissingPluginList.Add(attr);
  end;

  function EndParsing(cp : TControlPoint; var statusPanelText : string): boolean;
  var str, str2 : string; i : integer; newl : TStringList;
  begin
    str2 := TextByKey('main-status-variationsorvariables');
    if (cp.used_plugins.Count > 0) then begin
      newl := TStringList.Create;
      for i := 0 to MissingPluginList.Count - 1 do begin
        if cp.used_plugins.IndexOf(MissingPluginList[i]) >= 0 then
          newl.Add(MissingPluginList[i]);
      end;
      str2 := TextByKey('main-status-plugins');
      MissingPluginList.Free;
      MissingPluginList := newl;
    end;

    if MissingPluginList.Count > 0 then begin
      for i := 0 to MissingPluginList.Count - 1 do
        str := str + #13#10 + '  - ' + MissingPluginList[i];
      ErrorMessageString := Format('Plugins missing in %s:', [cp.name, str2]) + str;
      //WriteLn('ERROR: ' + ErrorMessageString);
      Result := false;
    end else begin
      ErrorMessageString := '';
      Result := true;
    end;
    MissingPluginList.Free;
  end;

  procedure AnnoyUser;
  begin
    if (ErrorMessageString = '') or (not WarnOnMissingPlugin) then exit;
  end;
end.
