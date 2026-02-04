{*******************************************************************************
  SkiaAudioVisualizer
********************************************************************************
  A high-performance, hardware-accelerated audio visualizer for Delphi FMX.
  Utilizing Skia4Delphi for rendering.
  Key Features:
  - Multiple Visualization Modes: Spectrum (with Peaks), Circle, Waveform, Bass Rain.
  - Dynamic Backgrounds: Modular and stable.
*******************************************************************************}
{ Skia-Audio-Visualizer v0.2                                                   }
{ by Lara Miriam Tamy Reschke                                                  }
{                                                                              }
{------------------------------------------------------------------------------}
{
 ----Latest Changes
   v 0.2:
   - Added slower falling Peaks to Spectrum
   - Added new TSkBackgroundType = btGradientBlobs, btSolidDark, btSolidBlack
   - Added property FBarColor
}

unit uSkiaAVisualizer;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Math,
  System.Generics.Collections, System.UITypes, System.SyncObjs, FMX.Types,
  FMX.Controls, FMX.Skia, System.Skia;

const
  MAX_FFT_DATA = 1024;

type
  TFFTData = array[0..MAX_FFT_DATA - 1] of Single;

  TSkVisualType = (vtSpectrum, vtCircle, vtWave, vtColorDrops);

  TSkBackgroundType = (btGradientBlobs, btSolidDark, btSolidBlack);

  // Interfaces
  ISkVisualizerEffect = interface
    ['{A1B2C3D4-E5F6-4789-0011-223344556677}']
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor);
  end;

  ISkBackgroundEffect = interface
    ['{B2C3D4E5-F6A7-4789-0022-334455667788}']
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Time: Double; const Data: TFFTData; const Sensitivity: Single; const AccentColor: TAlphaColor);
  end;

  TSkiaAVisualizer = class(TSkCustomControl)
  private
    FThread: TThread;
    FActive: Boolean;
    FLock: TCriticalSection;
    FTime: Double;
    FVisualType: TSkVisualType;
    FBackgroundType: TSkBackgroundType;
    FCurrentEffect: ISkVisualizerEffect;
    FCurrentBackground: ISkBackgroundEffect;
    FAudioData: TFFTData;
    FUseExternalData: Boolean;
    FSensitivity: Single;
    FTargetFPS: Integer;
    FAccentColor: TAlphaColor;
    FBarColor: TAlphaColor;
    FPrevWidth: Single;
    FPrevHeight: Single;

    procedure SetTargetFPS(const Value: Integer);
    procedure SetAccentColor(const Value: TAlphaColor);
    procedure SetBarColor(const Value: TAlphaColor);
    procedure UpdateLogic(DeltaSec: Double);
    procedure SafeInvalidate;
    procedure StartThread;
    procedure StopThread;
    procedure SetVisualType(const Value: TSkVisualType);
    procedure SetBackgroundType(const Value: TSkBackgroundType);
    procedure CreateEffect;
    procedure CreateBackground;
    procedure DrawDynamicBackground(const ACanvas: ISkCanvas; const ADest: TRectF; const Time: Double; const Data: TFFTData; const Sensitivity: Single; const AccentColor: TAlphaColor);

  protected
    procedure Resize; override;
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property TargetFPS: Integer read FTargetFPS write SetTargetFPS;
    property VisualType: TSkVisualType read FVisualType write SetVisualType;
    property BackgroundType: TSkBackgroundType read FBackgroundType write SetBackgroundType;
    property UseExternalData: Boolean read FUseExternalData write FUseExternalData;
    property Sensitivity: Single read FSensitivity write FSensitivity;
    property AccentColor: TAlphaColor read FAccentColor write SetAccentColor; // Used for Peaks/Highlights
    property BarColor: TAlphaColor read FBarColor write SetBarColor; // Used for Bars

    procedure UpdateAudioData(const NewData: TFFTData);
  end;

implementation

{==============================================================================
  BACKGROUND EFFECTS CLASSES
==============================================================================}

type
  TSkBgGradientBlobs = class(TInterfacedObject, ISkBackgroundEffect)
  public
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Time: Double; const Data: TFFTData; const Sensitivity: Single; const AccentColor: TAlphaColor);
  end;

procedure TSkBgGradientBlobs.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Time: Double; const Data: TFFTData; const Sensitivity: Single; const AccentColor: TAlphaColor);
var
  BgPaint: ISkPaint;
  Gradient: ISkShader;
  I: Integer;
  R, Offset1, Offset2: Single;
  Center: TPointF;
  Colors: TArray<TAlphaColor>;
  Positions: TArray<Single>;
  Color1, Color2, Color3: TAlphaColor;
  BassAvg: Single;
begin
  BassAvg := 0;
  if Length(Data) > 0 then
    for I := 0 to Min(9, High(Data)) do
      BassAvg := BassAvg + Data[I];
  BassAvg := BassAvg / Min(10, Length(Data));

  SetLength(Colors, 2);
  SetLength(Positions, 2);
  Colors[0] := $FF050020;
  Colors[1] := $FF000000;
  Positions[0] := 0.0;
  Positions[1] := 1.0;
  Gradient := TSkShader.MakeGradientLinear(PointF(0, 0), PointF(0, ADest.Height), Colors, Positions, TSkTileMode.Clamp);
  BgPaint := TSkPaint.Create;
  BgPaint.Shader := Gradient;
  ACanvas.DrawRect(ADest, BgPaint);

  Center := ADest.CenterPoint;
  Color1 := $FF00FFAA;
  Color2 := $FFAA00FF;
  Color3 := $FFFF0055;
  BgPaint.Style := TSkPaintStyle.Fill;
  BgPaint.ImageFilter := TSkImageFilter.MakeBlur(40 + (BassAvg * 20), 40 + (BassAvg * 20));
  Offset1 := Sin(Time * 0.7) * (ADest.Height * 0.3);
  Offset2 := Cos(Time * 0.9) * (ADest.Height * 0.4);

  R := (ADest.Width * 0.6) + (Sin(Time) * 20);
  Colors[0] := Color1;
  Colors[1] := TAlphaColors.Null;
  Gradient := TSkShader.MakeGradientRadial(PointF(Center.X - (ADest.Width * 0.2), ADest.Bottom - Offset1), R, Colors, Positions, TSkTileMode.Clamp);
  BgPaint.Shader := Gradient;
  ACanvas.DrawRect(ADest, BgPaint);

  R := (ADest.Width * 0.5) + (Cos(Time * 1.2) * 20);
  Colors[0] := Color2;
  Gradient := TSkShader.MakeGradientRadial(PointF(Center.X + (ADest.Width * 0.2), ADest.Bottom - Offset2), R, Colors, Positions, TSkTileMode.Clamp);
  BgPaint.Shader := Gradient;
  ACanvas.DrawRect(ADest, BgPaint);

  R := (ADest.Width * 0.4);
  Colors[0] := Color3;
  Gradient := TSkShader.MakeGradientRadial(PointF(Center.X + (Sin(Time * 0.5) * (ADest.Width * 0.5)), ADest.Bottom - (ADest.Height * 0.2)), R, Colors, Positions, TSkTileMode.Clamp);
  BgPaint.Shader := Gradient;
  ACanvas.DrawRect(ADest, BgPaint);
end;

type
  TSkBgSolidDark = class(TInterfacedObject, ISkBackgroundEffect)
  public
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Time: Double; const Data: TFFTData; const Sensitivity: Single; const AccentColor: TAlphaColor);
  end;

procedure TSkBgSolidDark.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Time: Double; const Data: TFFTData; const Sensitivity: Single; const AccentColor: TAlphaColor);
var
  BgPaint: ISkPaint;
begin
  BgPaint := TSkPaint.Create;
  BgPaint.Style := TSkPaintStyle.Fill;
  BgPaint.ImageFilter := nil;
  BgPaint.Color := $FF050020;
  ACanvas.DrawRect(ADest, BgPaint);
end;

type
  TSkBgSolidBlack = class(TInterfacedObject, ISkBackgroundEffect)
  public
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Time: Double; const Data: TFFTData; const Sensitivity: Single; const AccentColor: TAlphaColor);
  end;

procedure TSkBgSolidBlack.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Time: Double; const Data: TFFTData; const Sensitivity: Single; const AccentColor: TAlphaColor);
var
  BgPaint: ISkPaint;
begin
  BgPaint := TSkPaint.Create;
  BgPaint.Style := TSkPaintStyle.Fill;
  BgPaint.ImageFilter := nil;
  BgPaint.Color := $FF000000;
  ACanvas.DrawRect(ADest, BgPaint);
end;


{==============================================================================
  VISUALIZATION EFFECTS CLASSES
==============================================================================}

{------------------------------------------------------------------------------
  EFFECT 1: SPECTRUM (NEON BARS + PEAK DOTS - STRONG GLOW)
------------------------------------------------------------------------------}
type
  TSkEffectSpectrum = class(TInterfacedObject, ISkVisualizerEffect)
  private
    FPeaks: array of Single;
    FInitialized: Boolean;
  public
    constructor Create;
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor);
  end;

constructor TSkEffectSpectrum.Create;
begin
  inherited Create;
  SetLength(FPeaks, 512);
  FInitialized := False;
end;

procedure TSkEffectSpectrum.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor);
var
  I: Integer;
  BarWidth: Single;
  X, Y, H, PeakY: Single;
  Paint, GlowPaint, GlowPaintPeaks, PeakPaint: ISkPaint;
  Path: ISkPathBuilder;
  R: TRectF;
  BarsToDraw: Integer;
begin
  if (ADest.Width <= 0) or (ADest.Height <= 0) then
    Exit;

  Path := TSkPathBuilder.Create;

  BarWidth := 6.0;
  BarsToDraw := Trunc(ADest.Width / BarWidth);
  if BarsToDraw > 512 then
    BarsToDraw := 512;

  if BarWidth > 0 then
    BarWidth := ADest.Width / BarsToDraw;

  if not FInitialized then
  begin
    for I := 0 to 511 do
      FPeaks[I] := 0;
    FInitialized := True;
  end;

  for I := 0 to BarsToDraw - 1 do
  begin
    H := Power(Min(1.0, Data[I]), 0.8) * ADest.Height * 0.9;

    if H > ADest.Height then
      H := ADest.Height;
    if H < 0 then
      H := 0;
    if IsNan(H) or IsInfinite(H) then
      H := 0;

    // Peak Logic
    if H > FPeaks[I] then
      FPeaks[I] := H
    else
      FPeaks[I] := FPeaks[I] - (ADest.Height * 0.003);

    if FPeaks[I] < 0 then
      FPeaks[I] := 0;

    X := I * BarWidth;
    Y := ADest.Bottom - H;

    // Draw Bar
    R := TRectF.Create(X, Y, X + BarWidth - 1.5, ADest.Bottom);
    if (R.Width > 0) and (R.Height > 0) then
      Path.AddRect(R);
  end;

  // --- STEP 1: DRAW BARS GLOW ---
  GlowPaint := TSkPaint.Create;
  GlowPaint.Style := TSkPaintStyle.Fill;
  GlowPaint.Color := BarColor;
  GlowPaint.ImageFilter := TSkImageFilter.MakeBlur(20, 20);
  GlowPaint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurStyle.Solid, 12);
  ACanvas.DrawPath(Path.Snapshot, GlowPaint);

  // --- STEP 2: DRAW SOLID BARS ---
  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := BarColor;
  ACanvas.DrawPath(Path.Snapshot, Paint);

  // --- STEP 3: DRAW PEAKS GLOW ---
  Path := TSkPathBuilder.Create;
  for I := 0 to BarsToDraw - 1 do
  begin
    PeakY := ADest.Bottom - FPeaks[I];
    X := I * BarWidth;
    R := TRectF.Create(X, PeakY - 3, X + BarWidth - 1.5, PeakY + 1);
    if (R.Width > 0) then
      Path.AddRect(R);
  end;

  GlowPaintPeaks := TSkPaint.Create;
  GlowPaintPeaks.Style := TSkPaintStyle.Fill;
  GlowPaintPeaks.Color := AccentColor;
  GlowPaintPeaks.ImageFilter := TSkImageFilter.MakeBlur(15, 15);
  ACanvas.DrawPath(Path.Snapshot, GlowPaintPeaks);

  // --- STEP 4: DRAW SOLID PEAKS ---
  PeakPaint := TSkPaint.Create;
  PeakPaint.Style := TSkPaintStyle.Fill;
  PeakPaint.Color := AccentColor;
  ACanvas.DrawPath(Path.Snapshot, PeakPaint);
end;


{------------------------------------------------------------------------------
  EFFECT 2: CIRCLE SCOPE
------------------------------------------------------------------------------}
type
  TSkEffectCircle = class(TInterfacedObject, ISkVisualizerEffect)
  public
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor);
  end;

procedure TSkEffectCircle.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor);
var
  I: Integer;
  Angle, Radius, BaseRadius: Single;
  CenterX, CenterY: Single;
  Pt: TPointF;
  StartPoint: TPointF;
  Paint, GlowPaint: ISkPaint;
  Path: ISkPathBuilder;
  DataIdx: Integer;
begin
  if (ADest.Width <= 0) or (ADest.Height <= 0) then
    Exit;
  CenterX := ADest.Left + (ADest.Width / 2);
  CenterY := ADest.Top + (ADest.Height / 2);
  BaseRadius := Min(ADest.Width, ADest.Height) * 0.15;
  Path := TSkPathBuilder.Create;

  for I := 0 to 359 do
  begin
    Angle := DegToRad(I);
    DataIdx := Round((I / 360) * MAX_FFT_DATA);
    if DataIdx >= MAX_FFT_DATA then
      DataIdx := MAX_FFT_DATA - 1;

    Radius := BaseRadius + (Data[DataIdx] * Sensitivity * Min(ADest.Width, ADest.Height) * 0.3);
    if Radius > Max(ADest.Width, ADest.Height) then
      Radius := Max(ADest.Width, ADest.Height);

    Pt.X := CenterX + Cos(Angle) * Radius;
    Pt.Y := CenterY + Sin(Angle) * Radius;

    if I = 0 then
    begin
      Path.MoveTo(Pt);
      StartPoint := Pt;
    end
    else
      Path.LineTo(Pt);
  end;
  Path.LineTo(StartPoint);

  GlowPaint := TSkPaint.Create;
  GlowPaint.Style := TSkPaintStyle.Stroke;
  GlowPaint.StrokeWidth := 4.0;
  GlowPaint.Color := AccentColor;
  GlowPaint.ImageFilter := TSkImageFilter.MakeBlur(6, 6);
  GlowPaint.StrokeCap := TSkStrokeCap.Round;
  ACanvas.DrawPath(Path.Snapshot, GlowPaint);

  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 2.0;
  Paint.Color := BarColor; // Using BarColor for the inner line
  Paint.StrokeCap := TSkStrokeCap.Round;
  ACanvas.DrawPath(Path.Snapshot, Paint);
end;

{------------------------------------------------------------------------------
  EFFECT 3: WAVEFORM
------------------------------------------------------------------------------}
type
  TSkEffectWave = class(TInterfacedObject, ISkVisualizerEffect)
  public
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor);
  end;

procedure TSkEffectWave.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor);
var
  I: Integer;
  X, Y, CenterY: Single;
  Val: Single;
  Paint: ISkPaint;
  Path: ISkPathBuilder;
  DataVal: Single;
begin
  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 2.0;
  Paint.Color := AccentColor; // Waveform uses AccentColor primarily
  Paint.AntiAlias := True;
  Paint.ImageFilter := TSkImageFilter.MakeBlur(2, 2);
  Path := TSkPathBuilder.Create;

  if ADest.Height <= 0 then
    Exit;
  if ADest.Width <= 0 then
    Exit;
  CenterY := ADest.Top + (ADest.Height / 2);

  for I := 2 to MAX_FFT_DATA - 1 do
  begin
    X := ADest.Left + (I / MAX_FFT_DATA) * ADest.Width;
    DataVal := Data[I];
    if IsNan(DataVal) or IsInfinite(DataVal) then
      DataVal := 0.0;
    Val := DataVal * Sensitivity;
    if Val > 1.0 then
      Val := 1.0;
    Val := Sin((I * 0.1) + (Time * 10.0)) * (Val * ADest.Height * 0.45);
    Y := CenterY - Val;

    if I = 2 then
      Path.MoveTo(PointF(X, Y))
    else
      Path.LineTo(PointF(X, Y));
  end;
  ACanvas.DrawPath(Path.Snapshot, Paint);
end;

{------------------------------------------------------------------------------
  EFFECT 4: CHAOTIC BASS RAIN (COLOR DROPS)
------------------------------------------------------------------------------}
type
  TSkEffectColorDrops = class(TInterfacedObject, ISkVisualizerEffect)
  public
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor);
  end;

procedure TSkEffectColorDrops.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor);
var
  I, Count: Integer;
  X, Y, Radius, BassEnergy, DropSpeed: Single;
  Paint: ISkPaint;
begin
  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.AntiAlias := True;

  BassEnergy := 0;
  for I := 0 to 19 do
    BassEnergy := BassEnergy + Data[I];
  BassEnergy := (BassEnergy / 20) * Sensitivity;
  if BassEnergy < 0 then
    BassEnergy := 0;
  if BassEnergy > 1.2 then
    BassEnergy := 1.2;

  Paint.ImageFilter := TSkImageFilter.MakeBlur(2 + (BassEnergy * 20), 2 + (BassEnergy * 20));
  DropSpeed := 100 + (BassEnergy * 300);
  Count := 50;

  for I := 0 to Count - 1 do
  begin
    X := Sin((I * 132.5) + (Time * 0.5)) * (ADest.Width * 0.45) + ADest.CenterPoint.X;
    Y := Frac((I * 93.7) - (Time * (DropSpeed * 0.005))) * ADest.Height;
    Radius := 2 + Abs(Sin(I * 45));
    Radius := Radius + (BassEnergy * 30) + (Data[I mod 64] * Sensitivity * 10);

    if BassEnergy > 0.4 then
    begin
      Paint.Color := AccentColor;
      Paint.Alpha := Round(BassEnergy * 180);
    end
    else
    begin
      Paint.Color := BarColor; // Using BarColor for normal drops
      Paint.Alpha := Round(50 + (BassEnergy * 200));
    end;
    if Radius < 1 then
      Radius := 1;
    ACanvas.DrawCircle(PointF(X, Y), Radius, Paint);
  end;
end;

{==============================================================================
  MAIN COMPONENT: TSkiaAVisualizer
==============================================================================}

procedure TSkiaAVisualizer.SetAccentColor(const Value: TAlphaColor);
begin
  if FAccentColor <> Value then
  begin
    FLock.Acquire;
    try
      FAccentColor := Value;
    finally
      FLock.Release;
    end;
    Redraw;
  end;
end;

procedure TSkiaAVisualizer.SetBarColor(const Value: TAlphaColor);
begin
  if FBarColor <> Value then
  begin
    FLock.Acquire;
    try
      FBarColor := Value;
    finally
      FLock.Release;
    end;
    Redraw;
  end;
end;

{------------------------------------------------------------------------------
  DYNAMIC BACKGROUND RENDERER (SIMPLIFIED)
------------------------------------------------------------------------------}
procedure TSkiaAVisualizer.DrawDynamicBackground(const ACanvas: ISkCanvas; const ADest: TRectF; const Time: Double; const Data: TFFTData; const Sensitivity: Single; const AccentColor: TAlphaColor);
begin
  if not Assigned(FCurrentBackground) then
    CreateBackground;

  if Assigned(FCurrentBackground) then
    FCurrentBackground.Draw(ACanvas, ADest, Time, Data, Sensitivity, AccentColor);
end;

procedure TSkiaAVisualizer.SetVisualType(const Value: TSkVisualType);
begin
  if FVisualType <> Value then
  begin
    FLock.Acquire;
    try
      FVisualType := Value;
      CreateEffect;
    finally
      FLock.Release;
    end;
    Redraw;
  end;
end;

procedure TSkiaAVisualizer.SetBackgroundType(const Value: TSkBackgroundType);
begin
  if FBackgroundType <> Value then
  begin
    FLock.Acquire;
    try
      FBackgroundType := Value;
      FCurrentBackground := nil;
    finally
      FLock.Release;
    end;
    Redraw;
  end;
end;

procedure TSkiaAVisualizer.CreateEffect;
begin
  case FVisualType of
    vtSpectrum:
      FCurrentEffect := TSkEffectSpectrum.Create;
    vtCircle:
      FCurrentEffect := TSkEffectCircle.Create;
    vtWave:
      FCurrentEffect := TSkEffectWave.Create;
    vtColorDrops:
      FCurrentEffect := TSkEffectColorDrops.Create;
  else
    FCurrentEffect := nil;
  end;
end;

procedure TSkiaAVisualizer.CreateBackground;
begin
  case FBackgroundType of
    btGradientBlobs:
      FCurrentBackground := TSkBgGradientBlobs.Create;
    btSolidDark:
      FCurrentBackground := TSkBgSolidDark.Create;
    btSolidBlack:
      FCurrentBackground := TSkBgSolidBlack.Create;
  else
    FCurrentBackground := TSkBgSolidBlack.Create;
  end;
end;

{------------------------------------------------------------------------------
  INTERNAL LOGIC & THREADING
------------------------------------------------------------------------------}
procedure TSkiaAVisualizer.UpdateLogic(DeltaSec: Double);
var
  I: Integer;
  LocalTime: Double;
begin
  if not FActive then
    Exit;
  FLock.Acquire;
  try
    FTime := FTime + DeltaSec;
    LocalTime := FTime;
    if not FUseExternalData then
    begin
      for I := 0 to MAX_FFT_DATA - 1 do
      begin
        FAudioData[I] := (Sin((I * 0.1) + (LocalTime * 5.0)) * 0.5) + 0.5;
        if (Frac(LocalTime) < 0.2) and (I < 50) then
          FAudioData[I] := 1.0;
      end;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TSkiaAVisualizer.SafeInvalidate;
begin
  if csDestroying in ComponentState then
    Exit;
  TThread.Queue(nil,
    procedure
    begin
      if not (csDestroying in ComponentState) and Assigned(Self) then
      begin
        Self.Redraw;
      end;
    end);
end;

procedure TSkiaAVisualizer.StartThread;
begin
  if Assigned(FThread) then
    Exit;
  FThread := TThread.CreateAnonymousThread(
    procedure
    var
      LastTime, NowTime, DeltaMS: Cardinal;
      SleepTime: Integer;
    begin
      LastTime := TThread.GetTickCount;
      while not TThread.CheckTerminated do
      begin
        NowTime := TThread.GetTickCount;
        DeltaMS := NowTime - LastTime;
        if DeltaMS = 0 then
          DeltaMS := 1;
        LastTime := NowTime;

        if FActive then
        begin
          UpdateLogic(DeltaMS / 1000);
          SafeInvalidate;
        end;

        if FTargetFPS > 0 then
          SleepTime := Round(1000 / FTargetFPS)
        else
          SleepTime := 16;
        Sleep(SleepTime);
      end;
    end);
  FThread.FreeOnTerminate := True;
  FThread.Start;
end;

procedure TSkiaAVisualizer.SetTargetFPS(const Value: Integer);
begin
  if FTargetFPS <> Value then
  begin
    FTargetFPS := Value;
  end;
end;

procedure TSkiaAVisualizer.StopThread;
begin
  FActive := False;
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    Sleep(50);
  end;
end;

{------------------------------------------------------------------------------
  CONSTRUCTOR / DESTRUCTOR
------------------------------------------------------------------------------}
constructor TSkiaAVisualizer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  Align := TAlignLayout.Client;
  HitTest := True;
  FTargetFPS := 60;
  FActive := True;
  FTime := 0;
  FVisualType := vtSpectrum;
  FBackgroundType := btGradientBlobs;
  FUseExternalData := False;
  FSensitivity := 1.5;
  FAccentColor := $FF00FFFF; // Default Accent (Bright Cyan)
  FBarColor := $FF008080;    // Default Bar (Dark Teal)
  FPrevWidth := 0;
  FPrevHeight := 0;
  FillChar(FAudioData, SizeOf(FAudioData), 0);
  CreateEffect;
  CreateBackground;
  StartThread;
end;

destructor TSkiaAVisualizer.Destroy;
begin
  StopThread;
  FreeAndNil(FLock);
  inherited;
end;

{------------------------------------------------------------------------------
  OVERRIDE METHODS
------------------------------------------------------------------------------}
procedure TSkiaAVisualizer.Resize;
begin
  inherited;
  if (FPrevWidth <> Width) or (FPrevHeight <> Height) then
  begin
    FPrevWidth := Width;
    FPrevHeight := Height;
    if not (csDestroying in ComponentState) then
      Redraw;
  end;
end;

procedure TSkiaAVisualizer.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  DataCopy: TFFTData;
  EffectCopy: ISkVisualizerEffect;
  BgCopy: ISkBackgroundEffect;
  SensitivityCopy: Single;
  AccentCopy: TAlphaColor;
  BarCopy: TAlphaColor;
begin
  DrawDynamicBackground(ACanvas, ADest, FTime, FAudioData, FSensitivity, FAccentColor);

  FLock.Acquire;
  try
    DataCopy := FAudioData;
    EffectCopy := FCurrentEffect;
    SensitivityCopy := FSensitivity;
    AccentCopy := FAccentColor;
    BarCopy := FBarColor;
  finally
    FLock.Release;
  end;

  if Assigned(EffectCopy) then
    EffectCopy.Draw(ACanvas, ADest, DataCopy, FTime, SensitivityCopy, AccentCopy, BarCopy);
end;

procedure TSkiaAVisualizer.UpdateAudioData(const NewData: TFFTData);
begin
  FLock.Acquire;
  try
    FAudioData := NewData;
  finally
    FLock.Release;
  end;
end;

end.
