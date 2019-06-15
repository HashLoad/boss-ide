unit Providers.Logo;

interface

uses System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls;

type
  TDataModuleLogo = class(TDataModule)
    ImageList: TImageList;
  end;

var
  DataModuleLogo: TDataModuleLogo;

implementation

{$R *.dfm}

end.
