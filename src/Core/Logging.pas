unit Logging;

interface
  uses Classes, SysUtils, Windows;

  procedure LogSetPath(const dir: string);
  procedure LogWrite(const line, fileName: string);

type
  TAsyncLogCallback = procedure(fileName, msg: string); stdcall;

var
  G_SavePath: string;
  G_DisableLog: boolean;
  G_OnLog : TAsyncLogCallback;

implementation

  procedure LogSetPath(const dir: string);
  begin
    G_Savepath := dir;
  end;

  procedure LogWrite(const line, fileName: string);
  var
    lines: TStringList;
  begin
    if (G_DisableLog) then
      Exit;

    if (G_SavePath <> '') then
    begin

      lines := TStringList.Create;
      if (FileExists(G_Savepath + '\' + fileName)) then
        lines.LoadFromFile(G_Savepath + '\' + fileName);
      lines.Add(TimeToStr(NOW) + '|' + line);
      lines.SaveToFile(G_Savepath + '\' + fileName);
      lines.Destroy;
    end;

    if (@G_OnLog <> nil) then
    begin
      G_OnLog(fileName, line);
    end;

  end;

end.
