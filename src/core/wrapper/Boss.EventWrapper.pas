unit Boss.EventWrapper;

interface

uses
  System.Classes, System.SysUtils;

type
  TNotifyEventWrapper = class(TComponent)
  private
    FPath: string;
  public
    constructor Create(APath: String);
  published
    procedure Event(Sender: TObject);
  end;

function GetOpenEvent(APath: String): TNotifyEvent;

implementation

uses
  Winapi.ShellAPI, Boss.Ide.OpenToolApi.Tools, Winapi.Windows;

constructor TNotifyEventWrapper.Create(APath: String);
begin
  inherited Create(nil);
  FPath := APath;
end;

procedure TNotifyEventWrapper.Event(Sender: TObject);
begin
  ShellExecute(NativeServices.MainMenu.Handle, 'open', PChar(FPath), nil, nil, SW_SHOWNORMAL);
end;

function GetOpenEvent(APath: String): TNotifyEvent;
begin
  Result := TNotifyEventWrapper.Create(APath).Event;
end;

end.
