unit Providers.Message;

interface

uses
  ToolsAPI;

type
  TProviderMessage = class
    FService: IOTAMessageServices;
    FGroup: IOTAMessageGroup;
  public
    class function GetInstance: TProviderMessage;
    Procedure Initialize(AService: IOTAMessageServices);
    procedure WriteLn(ALine: string);
    procedure Clear;
  end;

implementation

var
  FInstance: TProviderMessage;

procedure TProviderMessage.Clear;
begin
  FService.ClearMessageGroup(FGroup);
end;

class function TProviderMessage.GetInstance: TProviderMessage;
begin
  if not Assigned(FInstance) then
    FInstance := TProviderMessage.Create;
  Result := FInstance;
end;

procedure TProviderMessage.Initialize(AService: IOTAMessageServices);
begin
  FService := AService;
  FGroup := FService.AddMessageGroup('Boss');

  FGroup.AutoScroll := True;
  FGroup.CanClose := False;
  FService.AddTitleMessage('Boss initialized..', FGroup);
end;

procedure TProviderMessage.WriteLn(ALine: string);
begin
  FService.ShowMessageView(FGroup);
  FService.AddTitleMessage(ALine, FGroup);
end;

end.
