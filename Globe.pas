unit Globe;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, Math,
  GDIPAPI, GDIPOBJ;

type
  TForm2 = class(TForm)
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure PaintBox1Paint(Sender: TObject);
  private
    FBmp: TBitmap;
    FPen: TGPPen;
    procedure GDIMultipleColorsDirect;
    procedure GDIMultipleColors;
    procedure GDIThreeColors;
    procedure GDIPlusDualLinewidths;
    procedure GDIPlusMultipleLinewidths;
  public
    A: Integer; //Alfa, rotation round X axis
    B: Integer; //Beta, rotation round Y axis
    C: TPoint;  //Center
    R: Integer; //Radius
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

const
  LineColorFore = $00552B00;
  LineColorMiddle = $00AA957F;
  LineColorBack = $00FFDFBF;
  BackColor = clWhite;
  LineWidthFore = 4.5;
  LineWidthBack = 1.5;
  Precision = 10; //Should be even!

type
  TCycle = 0..Precision - 1;

  TPoint3D = record
    X: Double;
    Y: Double;
    Z: Double;
  end;

function Sphere(Phi, Lambda: Double): TPoint3D;
begin
  Result.X := Cos(Phi) * Sin(Lambda);
  Result.Y := Sin(Phi);
  Result.Z := Cos(Phi) * Cos(Lambda);
end;

function RotateAroundX(const P: TPoint3D; Alfa: Double): TPoint3D;
begin
  Result.X := P.X;
  Result.Y := P.Y * Cos(Alfa) + P.Z * Sin(Alfa);
  Result.Z := P.Y * -Sin(Alfa) + P.Z * Cos(Alfa);
end;

function RotateAroundY(const P: TPoint3D; Beta: Double): TPoint3D;
begin
  Result.X := P.X * Cos(Beta) + P.Z * Sin(Beta);
  Result.Y := P.Y;
  Result.Z := P.X * -Sin(Beta) + P.Z * Cos(Beta);
end;

{ TForm1 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  Brush.Style := bsClear; //This is múch cheaper then DoubleBuffered := True
  FBmp := TBitmap.Create;
  FPen := TGPPen.Create(ColorRefToARGB(ColorToRGB(clBlack)));
  A := 35;
  B := 25;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FPen.Free;
  FBmp.Free;
end;

procedure TForm2.FormResize(Sender: TObject);
begin
  C.X := PaintBox1.ClientWidth div 2;
  C.Y := PaintBox1.ClientHeight div 2;
  R := Min(C.X, C.Y) - 10;
  FBmp.Width := PaintBox1.ClientWidth;
  FBmp.Height := PaintBox1.ClientHeight;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  A := A + 2;
  B := B + 1;
  PaintBox1.Invalidate;
end;

procedure TForm2.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Tag := Tag + 1;
  PaintBox1.Invalidate;
end;

procedure TForm2.PaintBox1Paint(Sender: TObject);
begin
  case Tag mod 5 of
    0: GDIMultipleColorsDirect;
    1: GDIMultipleColors;
    2: GDIThreeColors;
    3: GDIPlusDualLinewidths;
    4: GDIPlusMultipleLinewidths;
  end;
end;

procedure TForm2.GDIPlusMultipleLinewidths;
var
  Lines: array of TPointFDynArray;
  PointCount: Integer;
  LineCount: Integer;
  Drawing: TGPGraphics;
  Alfa: Double;
  Beta: Double;
  Cycle: TCycle;
  Phi: Integer;
  Lambda: Integer;
  P: TPoint3D;
  Filter: TCycle;
  PrevFilter: TCycle;
  I: Integer;

  procedure ResetLines;
  begin
    SetLength(Lines, 0);
    LineCount := 0;
    PointCount := 0;
  end;

  procedure FinishLastLine;
  begin
    if PointCount < 2 then
      Dec(LineCount)
    else
      SetLength(Lines[LineCount - 1], PointCount);
  end;

  procedure NewLine;
  begin
    if LineCount > 0 then
      FinishLastLine;
    SetLength(Lines, LineCount + 1);
    SetLength(Lines[LineCount], 361);
    Inc(LineCount);
    PointCount := 0;
  end;

  procedure AddPoint(X, Y: Single);
  begin
    Lines[LineCount - 1][PointCount] := MakePoint(X, Y);
    Inc(PointCount);
  end;

  function CycleFromZ(Z: Single): TCycle;
  begin
    Result := Round((Z + 1) / 2 * High(TCycle));
  end;

  function CycleToLineWidth(ACycle: TCycle): Single;
  begin
    Result := LineWidthBack +
      (LineWidthFore - LineWidthBack) * (ACycle / High(TCycle));
  end;

  function CycleToLineColor(ACycle: TCycle): TGPColor;
  begin
    if ACycle <= (High(TCycle) div 2) then
      Result := ColorRefToARGB(ColorToRGB(LineColorBack))
    else
      Result := ColorRefToARGB(ColorToRGB(LineColorFore));
  end;

begin
  Drawing := TGPGraphics.Create(FBmp.Canvas.Handle);
  try
    Drawing.Clear(ColorRefToARGB(ColorToRGB(clWhite)));
    Drawing.SetSmoothingMode(SmoothingModeAntiAlias);
    Alfa := DegToRad(A);
    Beta := DegToRad(B);
    for Cycle := Low(TCycle) to High(TCycle) do
    begin
      ResetLines;
      //Latitude
      for Phi := -8 to 8 do
      begin
        NewLine;
        PrevFilter := 0;
        for Lambda := 0 to 360 do
        begin
          P := Sphere(DegToRad(Phi * 10), DegToRad(Lambda));
          P := RotateAroundX(P, Alfa);
          P := RotateAroundY(P, Beta);
          Filter := CycleFromZ(P.Z);
          if Filter <> PrevFilter then
          begin
            AddPoint(C.X + P.X * R, C.Y + P.Y * R);
            NewLine;
          end;
          if Filter = Cycle then
            AddPoint(C.X + P.X * R, C.Y + P.Y * R);
          PrevFilter := Filter;
        end;
      end;
      //Longitude
      for Lambda := 0 to 17 do
      begin
        NewLine;
        PrevFilter := 0;
        for Phi := 0 to 360 do
        begin
          P := Sphere(DegToRad(Phi), DegToRad(Lambda * 10));
          P := RotateAroundX(P, Alfa);
          P := RotateAroundY(P, Beta);
          Filter := CycleFromZ(P.Z);
          if Filter <> PrevFilter then
          begin
            AddPoint(C.X + P.X * R, C.Y + P.Y * R);
            NewLine;
          end;
          if Filter = Cycle then
            AddPoint(C.X + P.X * R, C.Y + P.Y * R);
          PrevFilter := Filter;
        end;
      end;
      FinishLastLine;
      FPen.SetColor(CycleToLineColor(Cycle));
      FPen.SetWidth(CycleToLineWidth(Cycle));
      for I := 0 to LineCount - 1 do
        Drawing.DrawLines(FPen, PGPPointF(@(Lines[I][0])), Length(Lines[I]));
      if Cycle = (High(TCycle) div 2 + 1) then
        Drawing.DrawEllipse(FPen, C.X - R, C.Y - R, 2 * R, 2 * R);
    end;
  finally
    Drawing.Free;
  end;
  PaintBox1.Canvas.Draw(0, 0, FBmp);
end;

procedure TForm2.GDIPlusDualLinewidths;
const
  LineColors: array[Boolean] of TColor = (LineColorFore, LineColorBack);
  LineWidths: array[Boolean] of Single = (LineWidthFore, LineWidthBack);
  BackColor = clWhite;
var
  Lines: array of TPointFDynArray;
  PointCount: Integer;
  LineCount: Integer;
  Drawing: TGPGraphics;
  Alfa: Double;
  Beta: Double;
  Phi: Integer;
  Lambda: Integer;
  BackSide: Boolean;
  P: TPoint3D;
  PrevZ: Double;
  I: Integer;

  procedure ResetLines;
  begin
    SetLength(Lines, 0);
    LineCount := 0;
    PointCount := 0;
  end;

  procedure FinishLastLine;
  begin
    if PointCount < 2 then
      Dec(LineCount)
    else
      SetLength(Lines[LineCount - 1], PointCount);
  end;

  procedure NewLine;
  begin
    if LineCount > 0 then
      FinishLastLine;
    SetLength(Lines, LineCount + 1);
    SetLength(Lines[LineCount], 361);
    Inc(LineCount);
    PointCount := 0;
  end;

  procedure AddPoint(X, Y: Single);
  begin
    Lines[LineCount - 1][PointCount] := MakePoint(X, Y);
    Inc(PointCount);
  end;

begin
  Drawing := TGPGraphics.Create(FBmp.Canvas.Handle);
  try
    Drawing.Clear(ColorRefToARGB(ColorToRGB(clWhite)));
    Drawing.SetSmoothingMode(SmoothingModeAntiAlias);
    Alfa := DegToRad(A);
    Beta := DegToRad(B);
    for BackSide := True downto False do
    begin
      ResetLines;
      //Latitude
      for Phi := -8 to 8 do
      begin
        NewLine;
        PrevZ := 0;
        for Lambda := 0 to 360 do
        begin
          P := Sphere(DegToRad(Phi * 10), DegToRad(Lambda));
          P := RotateAroundX(P, Alfa);
          P := RotateAroundY(P, Beta);
          if Sign(P.Z) <> Sign(PrevZ) then
            NewLine;
          if (BackSide and (P.Z < 0)) or (not BackSide and (P.Z >= 0)) then
            AddPoint(C.X + P.X * R, C.Y + P.Y * R);
          PrevZ := P.Z;
        end;
      end;
      //Longitude
      for Lambda := 0 to 17 do
      begin
        NewLine;
        PrevZ := 0;
        for Phi := 0 to 360 do
        begin
          P := Sphere(DegToRad(Phi), DegToRad(Lambda * 10));
          P := RotateAroundX(P, Alfa);
          P := RotateAroundY(P, Beta);
          if Sign(P.Z) <> Sign(PrevZ) then
            NewLine;
          if (BackSide and (P.Z < 0)) or (not BackSide and (P.Z >= 0)) then
            AddPoint(C.X + P.X * R, C.Y + P.Y * R);
          PrevZ := P.Z;
        end;
      end;
      FinishLastLine;
      FPen.SetColor(ColorRefToARGB(ColorToRGB(LineColors[BackSide])));
      FPen.SetWidth(LineWidths[BackSide]);
      for I := 0 to LineCount - 1 do
        Drawing.DrawLines(FPen, PGPPointF(@(Lines[I][0])), Length(Lines[I]));
    end;
    Drawing.DrawEllipse(FPen, C.X - R, C.Y - R, 2 * R, 2 * R);
  finally
    Drawing.Free;
  end;
  PaintBox1.Canvas.Draw(0, 0, FBmp);
end;

procedure TForm2.GDIThreeColors;
const
  LineColors: array[TValueSign] of TColor = (LineColorBack, LineColorMiddle,
    LineColorFore);
  LineWidths: array[TValueSign] of Integer = (2, 4, 2);
var
  Lines: array of array of TPoint;
  PointCount: Integer;
  LineCount: Integer;
  Alfa: Double;
  Beta: Double;
  Phi: Integer;
  Lambda: Integer;
  BackSide: Boolean;
  P: TPoint3D;
  PrevZ: Double;
  I: TValueSign;
  J: Integer;

  procedure ResetLines;
  begin
    SetLength(Lines, 0);
    LineCount := 0;
    PointCount := 0;
  end;

  procedure FinishLastLine;
  begin
    if PointCount < 2 then
      Dec(LineCount)
    else
      SetLength(Lines[LineCount - 1], PointCount);
  end;

  procedure NewLine;
  begin
    if LineCount > 0 then
      FinishLastLine;
    SetLength(Lines, LineCount + 1);
    SetLength(Lines[LineCount], 361);
    Inc(LineCount);
    PointCount := 0;
  end;

  procedure AddPoint(APoint: TPoint); overload;
  var
    Last: TPoint;
  begin
    if PointCount > 0 then
    begin
      Last := Lines[LineCount - 1][PointCount - 1];
      if (APoint.X = Last.X) and (APoint.Y = Last.Y) then
        Exit;
    end;
    Lines[LineCount - 1][PointCount] := APoint;
    Inc(PointCount);
  end;

  procedure AddPoint(X, Y: Integer); overload;
  begin
    AddPoint(Point(X, Y));
  end;

begin
  FBmp.Canvas.Brush.Color := BackColor;
  FBmp.Canvas.FillRect(Rect(0, 0, FBmp.Width, FBmp.Height));
  Alfa := DegToRad(A);
  Beta := DegToRad(B);
  for BackSide := True downto False do
  begin
    ResetLines;
    //Latitude
    for Phi := -8 to 8 do
    begin
      NewLine;
      PrevZ := 0;
      for Lambda := 0 to 360 do
      begin
        P := Sphere(DegToRad(Phi * 10), DegToRad(Lambda));
        P := RotateAroundX(P, Alfa);
        P := RotateAroundY(P, Beta);
        if Sign(P.Z) <> Sign(PrevZ) then
          NewLine;
        if (BackSide and (P.Z < 0)) or (not BackSide and (P.Z >= 0)) then
          AddPoint(Round(C.X + P.X * R), Round(C.Y + P.Y * R));
        PrevZ := P.Z;
      end;
    end;
    //Longitude
    for Lambda := 0 to 17 do
    begin
      NewLine;
      PrevZ := 0;
      for Phi := 0 to 360 do
      begin
        P := Sphere(DegToRad(Phi), DegToRad(Lambda * 10));
        P := RotateAroundX(P, Alfa);
        P := RotateAroundY(P, Beta);
        if Sign(P.Z) <> Sign(PrevZ) then
          NewLine;
        if (BackSide and (P.Z < 0)) or (not BackSide and (P.Z >= 0)) then
          AddPoint(Round(C.X + P.X * R), Round(C.Y + P.Y * R));
        PrevZ := P.Z;
      end;
    end;
    FinishLastLine;
    if BackSide then
    begin
      FBmp.Canvas.Pen.Color := LineColors[-1];
      FBmp.Canvas.Pen.Width := LineWidths[-1];
      for J := 0 to LineCount - 1 do
        FBmp.Canvas.Polyline(Lines[J]);
    end
    else
      for I := 0 to 1 do
      begin
        FBmp.Canvas.Pen.Color := LineColors[I];
        FBmp.Canvas.Pen.Width := LineWidths[I];
        for J := 0 to LineCount - 1 do
          FBmp.Canvas.Polyline(Lines[J])
      end
  end;
  FBmp.Canvas.Brush.Style := bsClear;
  FBmp.Canvas.Ellipse(C.X - R, C.Y - R, C.X + R, C.Y + R);
  PaintBox1.Canvas.Draw(0, 0, FBmp);
end;

procedure TForm2.GDIMultipleColors;
var
  Alfa: Double;
  Beta: Double;
  Phi: Integer;
  Lambda: Integer;
  P: TPoint3D;
  Backside: Boolean;

  function ColorFromZ(Z: Single): TColorRef;
  var
    R: Integer;
    G: Integer;
    B: Integer;
  begin
    Z := (Z + 1) / 2;
    R := GetRValue(LineColorFore) - GetRValue(LineColorBack);
    R := GetRValue(LineColorBack) + Round(Z * R);
    G := GetGValue(LineColorFore) - GetGValue(LineColorBack);
    G := GetGValue(LineColorBack) + Round(Z * G);
    B := GetBValue(LineColorFore) - GetBValue(LineColorBack);
    B := GetBValue(LineColorBack) + Round(Z * B);
    Result := RGB(R, G, B);
  end;

begin
  FBmp.Canvas.Pen.Width := 2;
  FBmp.Canvas.Brush.Color := BackColor;
  FBmp.Canvas.FillRect(PaintBox1.ClientRect);
  Alfa := DegToRad(A);
  Beta := DegToRad(B);
  for Backside := True downto False do
  begin
    if not BackSide then
      FBmp.Canvas.Pen.Width := 3;
    //Latitude
    for Phi := -8 to 8 do
      for Lambda := 0 to 360 do
      begin
        P := Sphere(DegToRad(Phi * 10), DegToRad(Lambda));
        P := RotateAroundX(P, Alfa);
        P := RotateAroundY(P, Beta);
        if (Lambda = 0) or (Backside and (P.Z >= 0)) or
          (not Backside and (P.Z < 0)) then
            FBmp.Canvas.MoveTo(C.X + Round(P.X * R), C.Y + Round(P.Y * R))
        else
        begin
          FBmp.Canvas.Pen.Color := ColorFromZ(P.Z);
          FBmp.Canvas.LineTo(C.X + Round(P.X * R), C.Y + Round(P.Y * R));
        end;
      end;
    //Longitude
    for Lambda := 0 to 17 do
      for Phi := 0 to 360 do
      begin
        P := Sphere(DegToRad(Phi), DegToRad(Lambda * 10));
        P := RotateAroundX(P, Alfa);
        P := RotateAroundY(P, Beta);
        if (Phi = 0) or (Backside and (P.Z >= 0)) or
          (not Backside and (P.Z < 0)) then
            FBmp.Canvas.MoveTo(C.X + Round(P.X * R), C.Y + Round(P.Y * R))
        else
        begin
          FBmp.Canvas.Pen.Color := ColorFromZ(P.Z);
          FBmp.Canvas.LineTo(C.X + Round(P.X * R), C.Y + Round(P.Y * R));
        end;
      end;
  end;
  PaintBox1.Canvas.Draw(0, 0, FBmp);
end;

procedure TForm2.GDIMultipleColorsDirect;
var
  Alfa: Double;
  Beta: Double;
  Phi: Integer;
  Lambda: Integer;
  P: TPoint3D;
  Backside: Boolean;

  function ColorFromZ(Z: Single): TColorRef;
  var
    R: Integer;
    G: Integer;
    B: Integer;
  begin
    Z := (Z + 1) / 2;
    R := GetRValue(LineColorFore) - GetRValue(LineColorBack);
    R := GetRValue(LineColorBack) + Round(Z * R);
    G := GetGValue(LineColorFore) - GetGValue(LineColorBack);
    G := GetGValue(LineColorBack) + Round(Z * G);
    B := GetBValue(LineColorFore) - GetBValue(LineColorBack);
    B := GetBValue(LineColorBack) + Round(Z * B);
    Result := RGB(R, G, B);
  end;

begin
  PaintBox1.Canvas.Pen.Width := 2;
  PaintBox1.Canvas.Brush.Color := BackColor;
  PaintBox1.Canvas.FillRect(PaintBox1.ClientRect);
  Alfa := DegToRad(A);
  Beta := DegToRad(B);
  for Backside := True downto False do
  begin
    if not BackSide then
      PaintBox1.Canvas.Pen.Width := 3;
    //Latitude
    for Phi := -8 to 8 do
      for Lambda := 0 to 360 do
      begin
        P := Sphere(DegToRad(Phi * 10), DegToRad(Lambda));
        P := RotateAroundX(P, Alfa);
        P := RotateAroundY(P, Beta);
        if (Lambda = 0) or (Backside and (P.Z >= 0)) or
          (not Backside and (P.Z < 0)) then
            PaintBox1.Canvas.MoveTo(C.X + Round(P.X * R), C.Y + Round(P.Y * R))
        else
        begin
          PaintBox1.Canvas.Pen.Color := ColorFromZ(P.Z);
          PaintBox1.Canvas.LineTo(C.X + Round(P.X * R), C.Y + Round(P.Y * R));
        end;
      end;
    //Longitude
    for Lambda := 0 to 17 do
      for Phi := 0 to 360 do
      begin
        P := Sphere(DegToRad(Phi), DegToRad(Lambda * 10));
        P := RotateAroundX(P, Alfa);
        P := RotateAroundY(P, Beta);
        if (Phi = 0) or (Backside and (P.Z >= 0)) or
          (not Backside and (P.Z < 0)) then
            PaintBox1.Canvas.MoveTo(C.X + Round(P.X * R), C.Y + Round(P.Y * R))
        else
        begin
          PaintBox1.Canvas.Pen.Color := ColorFromZ(P.Z);
          PaintBox1.Canvas.LineTo(C.X + Round(P.X * R), C.Y + Round(P.Y * R));
        end;
      end;
  end;
end;

end.
