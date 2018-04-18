program OrologioStrano;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Globe in 'Globe.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form1);
  Application.Run;
end.
