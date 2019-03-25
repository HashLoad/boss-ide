unit Providers.Logo;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls;

type
  TDataModuleLogo = class(TDataModule)
    ImageList: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModuleLogo: TDataModuleLogo;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
