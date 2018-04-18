unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
    Button1: TButton;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DisegnaQuadranteOrario(unita: integer; cx,cy,dx,dy: integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.DisegnaQuadranteOrario(unita, cx, cy, dx, dy: integer);
var
    qdr,tri,pent: integer;
    x1,y1,x2,y2,x3,y3: integer;
    px: array [0..4] of integer;
    py: array [0..4] of integer;
    i,ang: integer;
begin
  qdr := unita div 15;
  tri := (unita mod 15) div 5;
  pent := (unita mod 5);

  Image1.Canvas.MoveTo(cx+0,cy-dy);
  if qdr = 0 then Image1.Canvas.Pen.Color := clBlue else Image1.Canvas.Pen.Color := clSilver;
  Image1.Canvas.LineTo(cx+dx,cy+0);
  if qdr = 1 then Image1.Canvas.Pen.Color := clBlue else Image1.Canvas.Pen.Color := clSilver;
  Image1.Canvas.LineTo(cx+0,cy+dy);
  if qdr = 2 then Image1.Canvas.Pen.Color := clBlue else Image1.Canvas.Pen.Color := clSilver;
  Image1.Canvas.LineTo(cx-dx,cy+0);
  if qdr = 3 then Image1.Canvas.Pen.Color := clBlue else Image1.Canvas.Pen.Color := clSilver;
  Image1.Canvas.LineTo(cx+0,cy-dy);
  // disegno del triangolo con il vertice verso il lato colorato blue
  case qdr of
  0:
     begin
       x1 := cx+(dx div 2); y1 := cy - (dy div 2);
       x2 := cx + 0; y2 := cy + dy;
       x3 := cx - dx; y3 := cy + 0;
     end;
  1:
     begin
       x1 := cx + (dx div 2); y1 := cy + (dy div 2);
       x2 := cx - dx; y2 := cy + 0;
       x3 := cx + 0 ; y3 := cy - dy;
     end;
  2:
     begin
       x1 := cx - (dx div 2); y1 := cy + (dy div 2);
       x2 := cx + 0; y2 := cy - dy;
       x3 := cx + dx; y3 := cy + 0;
     end;
  3:
     begin
       x1 := cx - (dx div 2); y1 := cy - (dy div 2);
       x2 := cx + dx; y2 := cy + 0;
       x3 := cx + 0; y3 := cy + dy;
     end;
  end;
  //
  Image1.Canvas.MoveTo(x1,y1);
  if tri = 0 then Image1.Canvas.Pen.Color := clGreen else Image1.Canvas.Pen.Color := clSilver;
  Image1.Canvas.LineTo(x2,y2);
  if tri = 1 then Image1.Canvas.Pen.Color := clGreen else Image1.Canvas.Pen.Color := clSilver;
  Image1.Canvas.LineTo(x3,y3);
  if tri = 2 then Image1.Canvas.Pen.Color := clGreen else Image1.Canvas.Pen.Color := clSilver;
  Image1.Canvas.LineTo(x1,y1);
  // disegno del pentagono inscritto nel triangolo 72° per ogni vertice del pentagono
  for i := 0 to 4 do
  begin
    ang := 180 - 45 - (qdr * 90) + (i * 72);
    // px[i] := trunc(75 * sin((2*pi)/360*ang)+180-((x1-180) div 2));
    // py[i] := trunc(75 * cos((2*pi)/360*ang)+180-((y1-180) div 2));
    px[i] := trunc((75/180)*dx * sin((2*pi)/360*ang)+cx-((x1-cx) div 2));
    py[i] := trunc((75/180)*dy * cos((2*pi)/360*ang)+cy-((y1-cy) div 2));
  end;
  Image1.Canvas.MoveTo(px[0],py[0]);
  if pent = 0 then Image1.Canvas.Pen.Color := clRed else Image1.Canvas.Pen.Color := clSilver;
  Image1.Canvas.LineTo(px[4],py[4]);
  if pent = 1 then Image1.Canvas.Pen.Color := clRed else Image1.Canvas.Pen.Color := clSilver;
  Image1.Canvas.LineTo(px[3],py[3]);
  if pent = 2 then Image1.Canvas.Pen.Color := clRed else Image1.Canvas.Pen.Color := clSilver;
  Image1.Canvas.LineTo(px[2],py[2]);
  if pent = 3 then Image1.Canvas.Pen.Color := clRed else Image1.Canvas.Pen.Color := clSilver;
  Image1.Canvas.LineTo(px[1],py[1]);
  if pent = 4 then Image1.Canvas.Pen.Color := clRed else Image1.Canvas.Pen.Color := clSilver;
  Image1.Canvas.LineTo(px[0],py[0]);

end;

procedure TForm1.Timer1Timer(Sender: TObject);
var hh,mm,secondi,ms: word;
    cx,cy,dx,dy: integer;
    cx1,cy1,dx1,dy1: integer;
    testo: string;
    orario: TDateTime;
begin
  // disegnare un rombo quadrato, un triangolo equilatero, un pentagono regolare
  // con uno dei lati colorato in base ai secondi
  // 60" = 4 * 15" + 3 * 5" + 5 * 1"
  orario := now;
  DecodeTime(orario,hh,mm,secondi,ms);

  Image1.Canvas.Brush.Color := clYellow;
  Image1.Canvas.FillRect(Image1.ClientRect);
  Image1.Canvas.Pen.Width := 2;
  //
  cx1 := 180;
  cy1 := 180;
  dx1 := 180;
  dy1 := 180;

  dx := dx1 div 3;
  dy := dy1 div 3;
  // scala
  cx := cx1 - (cx1 div 2);
  cy := cy1 + (cy1 div 2);


  DisegnaQuadranteOrario(secondi,cx,cy,dx,dy);
  // Analogamente per i minuti
  cx := cx1 + (cx1 div 2);
  cy := cy1 - (cy1 div 2);
  DisegnaQuadranteOrario(mm,cx,cy,dx,dy);

  // separazione quadranti per le ore
  cx := cx1 - (cx1 div 2);
  cy := cy1 - (cy1 div 2);
  DisegnaQuadranteOrario((hh*5+(mm div 5))mod 60,cx,cy,dx,dy);

  // in base a mattino, pomeriggio mette un disegno al 4 quadrante
  cx := cx1 + (cx1 div 2);
  cy := cy1 + (cy1 div 2);
  if hh div 12 = 0 then
  begin
    Image1.Canvas.Pen.Color := clBlack;
    testo := 'AM';
    Image1.Canvas.Font.Size := 10;
  end
  else
  begin
    Image1.Canvas.Pen.Color := clBlack;
    testo := 'PM';
    Image1.Canvas.Font.Size := 10;
  end;
  Image1.Canvas.TextOut(cx,cy,testo);
  // scrivo l'orario digitale al centro
  Image1.Canvas.Pen.Color := clBlack;
  testo := FormatDateTime('hh:mm:ss',orario);
  Image1.Canvas.Font.Size := 10;
  cx := cx1 - (Image1.Canvas.TextWidth(testo) div 2);
  cy := cy1 - (Image1.Canvas.TextHeight(testo) div 2);
  Image1.Canvas.TextOut(cx,cy,testo);
end;

end.
