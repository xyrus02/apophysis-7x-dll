unit Diagnostics;

interface
  uses Classes, SysUtils, Windows;

  procedure LogSetPath(const dir: string);
  procedure LogWrite(const line, fileName: string);
  procedure LogPurge(const fileName: string);

var
  G_SavePath: string;

implementation

  procedure LogSetPath(const dir: string);
  begin
    G_Savepath := dir;
  end;

  procedure LogWrite(const line, fileName: string);
  var
    lines: TStringList;
  begin
    //Windows.MessageBox(0, PAnsiChar(G_SavePath + ' - ' + line), PAnsiChar(fileName), MB_OK);
    lines := TStringList.Create;
    if (FileExists(G_Savepath + '\' + fileName)) then
      lines.LoadFromFile(G_Savepath + '\' + fileName);
    lines.Add(TimeToStr(NOW) + '|' + line);
    lines.SaveToFile(G_Savepath + '\' + fileName);
    lines.Destroy;
  end;

  procedure LogPurge(const fileName: string);
  var
    lines: TStringList;
  begin
    lines := TStringList.Create;
    lines.SaveToFile(G_Savepath + '\' + fileName);
    lines.Destroy;
  end;

end.
