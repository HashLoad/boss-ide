unit Boss.Modules.PackageProcessor;


interface

uses
  System.IniFiles, System.Classes, System.SysUtils;

type
  TBossPackageProcessor = class
  private
    FDataFile: TStringList;

    function GetBplList(ARootPath: string): TArray<string>;
    function GetBinList(ARootPath: string): TArray<string>;

    function GetDataCachePath: string;

    constructor Create;
  public
    procedure LoadBpls(AProjectPath: string);
    procedure LoadTools(AProjectPath: string);
    procedure UnloadOlds;

    class Procedure OnActiveProjectChanged(AProject: string);
    class function GetInstance: TBossPackageProcessor;
  end;

implementation

uses
  System.IOUtils, Providers.Consts, Boss.IDE.Installer, Providers.Message, Vcl.Dialogs, ToolsAPI,
  Boss.IDE.OpenToolApi.Tools, Winapi.ShellAPI, Winapi.Windows, Vcl.Menus, Boss.EventWrapper;

{ TBossPackageProcessor }

var
  _Instance: TBossPackageProcessor;

constructor TBossPackageProcessor.Create;
begin
  FDataFile := TStringList.Create();

  if FileExists(GetDataCachePath) then
    FDataFile.LoadFromFile(GetDataCachePath);

  UnloadOlds;
end;

function TBossPackageProcessor.GetBinList(ARootPath: string): TArray<string>;
begin
  if not DirectoryExists(ARootPath + C_BIN_FOLDER) then
    Exit();

  Result := TDirectory.GetFiles(ARootPath + C_BIN_FOLDER, '*.exe')
end;

function TBossPackageProcessor.GetBplList(ARootPath: string): TArray<string>;
begin
  if not DirectoryExists(ARootPath + C_BPL_FOLDER) then
    Exit();

  Result := TDirectory.GetFiles(ARootPath + C_BPL_FOLDER, '*.bpl')
end;

function TBossPackageProcessor.GetDataCachePath: string;
begin
  Result := GetEnvironmentVariable('HOMEDRIVE') + GetEnvironmentVariable('HOMEPATH') + TPath.DirectorySeparatorChar +
    C_BOSS_CACHE_FOLDER + TPath.DirectorySeparatorChar + C_DATA_FILE;
end;


class function TBossPackageProcessor.GetInstance: TBossPackageProcessor;
begin
  if not Assigned(_Instance) then
    _Instance := TBossPackageProcessor.Create;
  Result := _Instance;
end;

procedure PackageInfoProc(const Name: string; NameType: TNameType; Flags: Byte; Param: Pointer);
begin

end;

procedure TBossPackageProcessor.LoadBpls(AProjectPath: string);
var
  LBpls: TArray<string>;
  LBpl: string;
  LFlag: Integer;
  LHnd: NativeUInt;
begin
  LBpls := GetBplList(AProjectPath);
  for LBpl in LBpls do
  begin
    try
      LHnd := LoadPackage(LBpl);
      GetPackageInfo(LHnd, nil, LFlag, PackageInfoProc);
      UnloadPackage(LHnd);
    except
      TProviderMessage.GetInstance.WriteLn('Failed to get info of ' + LBpl);
      Continue;
    end;

    if not(LFlag and pfRunOnly = pfRunOnly) and TBossIDEInstaller.InstallBpl(LBpl) then
    begin
      FDataFile.Add(LBpl);
      TProviderMessage.GetInstance.WriteLn('Instaled: ' + LBpl);
    end;
  end;
  FDataFile.SaveToFile(GetDataCachePath);
end;

procedure TBossPackageProcessor.LoadTools(AProjectPath: string);
var
  LBins: TArray<string>;
  LBin, LBinName: string;
  LFlag: Integer;
  LHnd: NativeUInt;
  LMenu: TMenuItem;
  LMenuItem: TMenuItem;
begin
  LMenu := NativeServices.MainMenu.Items.Find('Tools');
  LBins := GetBinList(AProjectPath);

  NativeServices.MenuBeginUpdate;
  try
    for LBin in LBins do
    begin
      LBinName := ExtractFileName(LBin);
      LMenuItem := TMenuItem.Create(NativeServices.MainMenu);
      LMenuItem.Caption := Providers.Consts.C_BOSS_TAG + ' ' + LBinName;
      LMenuItem.OnClick := GetOpenEvent(LBin);
      LMenuItem.Name := 'boss_' + LBinName.Replace('.', '_');
      LMenuItem.Hint := LBin;
      LMenu.Add(LMenuItem);
    end;
  finally
    NativeServices.MenuEndUpdate;
  end;
  FDataFile.SaveToFile(GetDataCachePath);
end;

class procedure TBossPackageProcessor.OnActiveProjectChanged(AProject: string);
begin
  TProviderMessage.GetInstance.Clear;
  TProviderMessage.GetInstance.WriteLn('Loading packages from project ' + AProject);

  GetInstance.UnloadOlds;
  GetInstance.LoadBpls(ExtractFilePath(AProject) + C_MODULES_FOLDER);
  GetInstance.LoadTools(ExtractFilePath(AProject) + C_MODULES_FOLDER);
end;

procedure TBossPackageProcessor.UnloadOlds;
var
  LBpl: string;
  LBin, LBinName: string;
  LFlag: Integer;
  LHnd: NativeUInt;
  LMenu: TMenuItem;
  LMenuItem: TMenuItem;
  LIndex: Integer;
begin
  for LBpl in FDataFile do
  begin
    TBossIDEInstaller.RemoveBpl(LBpl);
    TProviderMessage.GetInstance.WriteLn('Removed: ' + LBpl);
  end;

  LMenu := NativeServices.MainMenu.Items.Find('Tools');

  NativeServices.MenuBeginUpdate;
  try
    for LIndex := 0 to LMenu.Count - 1 do
    begin
      LMenuItem := LMenu.Items[LIndex];
      if LMenuItem.Caption.StartsWith(C_BOSS_TAG) then
      begin
        LMenu.Remove(LMenuItem);
        LMenuItem.Free;
      end;
    end;
  finally
    NativeServices.MenuEndUpdate;
  end;

  FDataFile.Clear;
  FDataFile.SaveToFile(GetDataCachePath);
end;

initialization

finalization

_Instance.Free;

end.

