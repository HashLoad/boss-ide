unit Boss.IDE.BossInstall;

interface

uses
  ToolsAPI, Vcl.ActnList, System.Classes;

type

  TMenuNotifierBossInstall = class(TNotifierObject, IOTANotifier, IOTAProjectMenuItemCreatorNotifier)
  public
    function CanHandle(const Ident: string): Boolean;
    procedure AddMenu(const Project: IOTAProject; const IdentList: TStrings;
      const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
  end;

  TButtonBossInstall = class(TNotifierObject, IOTALocalMenu, IOTAProjectManagerMenu)
  private
    FCaption: string;
    FChecked: Boolean;
    FEnabled: Boolean;
    FHelpContext: Integer;
    FName: string;
    FParent: string;
    FPosition: Integer;
    FVerb: string;
    FOnExecute: TNotifyEvent;

    FProject: IOTAProject;
  public
    function GetCaption: string;
    function GetChecked: Boolean;
    function GetEnabled: Boolean;
    function GetHelpContext: Integer;
    function GetName: string;
    function GetParent: string;
    function GetPosition: Integer;
    function GetVerb: string;
    procedure SetCaption(const Value: string);
    procedure SetChecked(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetHelpContext(Value: Integer);
    procedure SetName(const Value: string);
    procedure SetParent(const Value: string);
    procedure SetPosition(Value: Integer);
    procedure SetVerb(const Value: string);
    property Caption: string read GetCaption write SetCaption;
    property Checked: Boolean read GetChecked write SetChecked;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property HelpContext: Integer read GetHelpContext write SetHelpContext;
    property Name: string read GetName write SetName;
    property Parent: string read GetParent write SetParent;
    property Position: Integer read GetPosition write SetPosition;
    property Verb: string read GetVerb write SetVerb;
    property OnExute: TNotifyEvent read FOnExecute write FOnExecute;

    // IOTAProjectManagerMenu
    function GetIsMultiSelectable: Boolean;
    procedure SetIsMultiSelectable(Value: Boolean);
    procedure Execute(const MenuContextList: IInterfaceList); overload;
    function PreExecute(const MenuContextList: IInterfaceList): Boolean;
    function PostExecute(const MenuContextList: IInterfaceList): Boolean;
    property IsMultiSelectable: Boolean read GetIsMultiSelectable write SetIsMultiSelectable;
    constructor Create(aProject: IOTAProject);
  end;

implementation

uses
  System.SysUtils, CnOTAUtils, Boss.Modules.PackageProcessor, Boss.Commands;

constructor TButtonBossInstall.Create(aProject: IOTAProject);
begin
  Self.Caption := 'Boss install';
  Self.Enabled := True;
  Self.Position := pmmpInstall + 10;

  FProject := aProject;
end;

procedure TButtonBossInstall.Execute(const MenuContextList: IInterfaceList);
begin

  if CnOtaGetCurrentProject = FProject then
    TBossPackageProcessor
      .GetInstance
      .UnloadOlds;

  RunBossInstall(ExtractFilePath(FProject.FileName), CnOtaGetCurrentProject = FProject);
end;

function TButtonBossInstall.GetCaption: string;
begin
  Result := FCaption;
end;

function TButtonBossInstall.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TButtonBossInstall.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TButtonBossInstall.GetHelpContext: Integer;
begin
  Result := FHelpContext;
end;

function TButtonBossInstall.GetIsMultiSelectable: Boolean;
begin
  Result := False;
end;

function TButtonBossInstall.GetName: string;
begin
  Result := FName;
end;

function TButtonBossInstall.GetParent: string;
begin
  Result := FParent;
end;

function TButtonBossInstall.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TButtonBossInstall.GetVerb: string;
begin
  Result := FVerb;
end;

function TButtonBossInstall.PostExecute(const MenuContextList: IInterfaceList): Boolean;
begin
  Result := True;
end;

function TButtonBossInstall.PreExecute(const MenuContextList: IInterfaceList): Boolean;
begin
  Result := True;
end;

procedure TButtonBossInstall.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TButtonBossInstall.SetChecked(Value: Boolean);
begin
  FChecked := Value;
end;

procedure TButtonBossInstall.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TButtonBossInstall.SetHelpContext(Value: Integer);
begin
  FHelpContext := Value;
end;

procedure TButtonBossInstall.SetIsMultiSelectable(Value: Boolean);
begin
  IsMultiSelectable := Value;
end;

procedure TButtonBossInstall.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TButtonBossInstall.SetParent(const Value: string);
begin
  FParent := Value;
end;

procedure TButtonBossInstall.SetPosition(Value: Integer);
begin
  FPosition := Value;
end;

procedure TButtonBossInstall.SetVerb(const Value: string);
begin
  FVerb := Value;
end;

{ TMenuNotifierBossInstall }

procedure TMenuNotifierBossInstall.AddMenu(const Project: IOTAProject; const IdentList: TStrings;
  const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
begin
  if CanHandle(Project.ApplicationType) and (not IsMultiSelect) and Assigned(Project) and
    (IdentList.IndexOf(sProjectContainer) <> -1) and Assigned(ProjectManagerMenuList) then
  begin
    ProjectManagerMenuList.Add(TButtonBossInstall.Create(Project));
  end;
end;

function TMenuNotifierBossInstall.CanHandle(const Ident: string): Boolean;
begin
  Result := (Ident = sApplication) or (Ident = sConsole) or (Ident = sPackage);
end;

end.
