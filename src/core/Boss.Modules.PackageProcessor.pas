unit Boss.Modules.PackageProcessor;

interface

uses
  System.IniFiles, System.Classes;

type
  TBossPackageProcessor = class
  private
    FDataFile: TStringList;

    function GetBplList(ARootPath: string): TArray<string>;

    function GetDataCachePath: string;
    constructor Create;
  public
    procedure LoadBpls(AProjectPath: string);
    procedure UnloadOlds;

    class Procedure OnActiveProjectChanged(AProject: string);
    class function GetInstance: TBossPackageProcessor;
  end;

implementation

uses
  System.IOUtils, Providers.Consts, Boss.IDE.Installer, System.SysUtils, Providers.Message, Vcl.Dialogs;

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

function TBossPackageProcessor.GetBplList(ARootPath: string): TArray<string>;
begin
  if not DirectoryExists(ARootPath + C_BPL_FOLDER) then
    Exit();

  Result := TDirectory.GetFiles(ARootPath + C_BPL_FOLDER, '*.bpl')
end;

function TBossPackageProcessor.GetDataCachePath: string;
begin
  Result := TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + '..' + TPath.DirectorySeparatorChar +
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
  LBpl, LBplFullPath: string;
  LFlag: Integer;
  LHnd: NativeUInt;
begin
  TProviderMessage
    .GetInstance
    .Clear;

  UnloadOlds;

  LBpls := GetBplList(AProjectPath);
  for LBpl in LBpls do
  begin
    try
      LHnd := LoadPackage(LBpl);
      GetPackageInfo(LHnd, nil, LFlag, PackageInfoProc);
      UnloadPackage(LHnd);
    except
      LFlag := 0;
    end;

    if not(LFlag and pfRunOnly = pfRunOnly) and TBossIDEInstaller.InstallBpl(LBpl) then
    begin
      FDataFile.Add(LBpl);

      TProviderMessage
        .GetInstance
        .WriteLn('Instaled: ' + LBpl);
    end;
  end;
  FDataFile.SaveToFile(GetDataCachePath);
end;

class procedure TBossPackageProcessor.OnActiveProjectChanged(AProject: string);
begin
  GetInstance.LoadBpls(ExtractFilePath(AProject) + C_MODULES_FOLDER);
end;

procedure TBossPackageProcessor.UnloadOlds;
var
  LBpl: string;
begin
  for LBpl in FDataFile do
  begin
    TBossIDEInstaller.RemoveBpl(LBpl);
    TProviderMessage
      .GetInstance
      .WriteLn('Removed: ' + LBpl);

  end;

  FDataFile.Clear;
  FDataFile.SaveToFile(GetDataCachePath);
end;

initialization
finalization

_Instance.Free;

end.
