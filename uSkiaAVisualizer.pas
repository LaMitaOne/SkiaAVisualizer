{*******************************************************************************
  SkiaAudioVisualizer
********************************************************************************
  A high-performance, hardware-accelerated audio visualizer for Delphi FMX.
  Utilizing Skia4Delphi for rendering.

  Key Features:
  - Multiple Visualization Modes: Spectrum, Circle, Waveform, and Bass Rain.
  - Dynamic Backgrounds: Animated gradient blobs reacting to audio type.
*******************************************************************************}

{ Skia-Audio-Visualizer v0.1                                                   }
{ by Lara Miriam Tamy Reschke                                                  }
{                                                                              }
{------------------------------------------------------------------------------}

{
 ----Latest Changes
   v 0.1: First Release
}

unit uSkiaAVisualizer;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Math, System.Generics.Collections,
  System.UITypes, System.SyncObjs, FMX.Types, FMX.Controls, FMX.Skia, System.Skia;

const
  MAX_FFT_DATA = 512;

type
  TFFTData = array[0..MAX_FFT_DATA - 1] of Single;
  TSkVisualType = (vtSpectrum, vtCircle, vtWave, vtColorDrops);

  ISkVisualizerEffect = interface
    ['{A1B2C3D4-E5F6-4789-0011-223344556677}']
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor);
  end;

  TSkiaAVisualizer = class(TSkCustomControl)
  private
    FThread: TThread;
    FActive: Boolean;
    FLock: TCriticalSection;
    FTime: Double;
    FVisualType: TSkVisualType;
    FCurrentEffect: ISkVisualizerEffect;
    FAudioData: TFFTData;
    FUseExternalData: Boolean;
    FSensitivity: Single;
    FTargetFPS: Integer;
    FAccentColor: TAlphaColor;
    FPrevWidth: Single;
    FPrevHeight: Single;

    procedure SetTargetFPS(const Value: Integer);
    procedure SetAccentColor(const Value: TAlphaColor);
    procedure UpdateLogic(DeltaSec: Double);
    procedure SafeInvalidate;
    procedure StartThread;
    procedure StopThread;
    procedure SetVisualType(const Value: TSkVisualType);
    procedure CreateEffect;
    procedure DrawDynamicBackground(const ACanvas: ISkCanvas; const ADest: TRectF; const Time: Double);
  protected
    procedure Resize; override;
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property TargetFPS: Integer read FTargetFPS write SetTargetFPS;
    property VisualType: TSkVisualType read FVisualType write SetVisualType;
    property UseExternalData: Boolean read FUseExternalData write FUseExternalData;
    property Sensitivity: Single read FSensitivity write FSensitivity;
    property AccentColor: TAlphaColor read FAccentColor write SetAccentColor;
    procedure UpdateAudioData(const NewData: TFFTData);
  end;

implementation

{==============================================================================
  VISUALIZATION EFFECTS
==============================================================================}

{------------------------------------------------------------------------------
  EFFECT 1: SPECTRUM (NEON BARS)
  Renders frequency data as vertical bars. Includes overflow protection for
  high-DPI displays to prevent freezing.
------------------------------------------------------------------------------}
type
  TSkEffectSpectrum = class(TInterfacedObject, ISkVisualizerEffect)
  public
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor);
  end;

procedure TSkEffectSpectrum.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor);
var
  I: Integer;
  BarWidth: Single;
  X, Y, H: Single;
  Paint, GlowPaint: ISkPaint;
  Path: ISkPathBuilder;
  R: TRectF;
begin
  // 1. Safety Check
  if (ADest.Width <= 0) or (ADest.Height <= 0) then Exit;

  Path := TSkPathBuilder.Create;
  BarWidth := ADest.Width / (MAX_FFT_DATA div 2);

  // 2. Iterate through frequency bins
  for I := 0 to (MAX_FFT_DATA div 2) - 1 do
  begin
    // Calculate height with exponential scaling for better visuals
    H := Power(Data[I] * Sensitivity, 0.8) * ADest.Height * 0.9;

    // Clamp height to prevent drawing outside destination rect (Fixes freeze on resize)
    if H > ADest.Height then H := ADest.Height;
    if H < 0 then H := 0;

    X := I * BarWidth;
    Y := ADest.Bottom - H;

    // Define rectangle for the bar
    R := TRectF.Create(X, Y, X + BarWidth - 1.0, ADest.Bottom);

    // Only add if reasonable size (Prevents drawing infinitely thin lines that crash Skia)
    if (R.Width > 0) and (R.Height > 0) then
      Path.AddRect(R);
  end;

  // 3. Draw Glow
  GlowPaint := TSkPaint.Create;
  GlowPaint.Style := TSkPaintStyle.Fill;
  GlowPaint.Color := AccentColor;
  GlowPaint.ImageFilter := TSkImageFilter.MakeBlur(8, 8);
  GlowPaint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurStyle.Solid, 5);
  ACanvas.DrawPath(Path.Snapshot, GlowPaint);

  // 4. Draw Core (White center)
  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := TAlphaColors.White;
  ACanvas.DrawPath(Path.Snapshot, Paint);
end;

{------------------------------------------------------------------------------
  EFFECT 2: CIRCLE SCOPE
  Renders frequency data radially around a center point.
------------------------------------------------------------------------------}
type
  TSkEffectCircle = class(TInterfacedObject, ISkVisualizerEffect)
  public
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor);
  end;

procedure TSkEffectCircle.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor);
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
  if (ADest.Width <= 0) or (ADest.Height <= 0) then Exit;

  CenterX := ADest.Left + (ADest.Width / 2);
  CenterY := ADest.Top + (ADest.Height / 2);

  // Base Radius relative to smallest dimension to prevent overflow on wide screens
  BaseRadius := Min(ADest.Width, ADest.Height) * 0.15;

  Path := TSkPathBuilder.Create;

  for I := 0 to 359 do
  begin
    Angle := DegToRad(I);

    // Map 0..359 degrees to 0..MAX_FFT_DATA safely
    DataIdx := Round((I / 360) * MAX_FFT_DATA);
    if DataIdx >= MAX_FFT_DATA then DataIdx := MAX_FFT_DATA - 1;

    // Calculate radius based on audio data, clamped to screen size
    Radius := BaseRadius + (Data[DataIdx] * Sensitivity * Min(ADest.Width, ADest.Height) * 0.3);
    if Radius > Max(ADest.Width, ADest.Height) then Radius := Max(ADest.Width, ADest.Height);

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
  Paint.Color := TAlphaColors.White;
  Paint.StrokeCap := TSkStrokeCap.Round;
  ACanvas.DrawPath(Path.Snapshot, Paint);
end;

{------------------------------------------------------------------------------
  EFFECT 3: WAVEFORM
  Renders a sine-wave modulated by the FFT data. Centered on screen.
------------------------------------------------------------------------------}
type
  TSkEffectWave = class(TInterfacedObject, ISkVisualizerEffect)
  public
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor);
  end;

procedure TSkEffectWave.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor);
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
  Paint.Color := AccentColor;
  Paint.AntiAlias := True;
  Paint.ImageFilter := TSkImageFilter.MakeBlur(2, 2); // Subtle blur for glow

  Path := TSkPathBuilder.Create;

  if ADest.Height <= 0 then Exit;
  if ADest.Width <= 0 then Exit;

  CenterY := ADest.Top + (ADest.Height / 2);

  for I := 2 to MAX_FFT_DATA - 1 do
  begin
    if MAX_FFT_DATA > 1 then
      X := ADest.Left + (I / MAX_FFT_DATA) * ADest.Width
    else
      X := ADest.Left;

    DataVal := Data[I];
    if IsNan(DataVal) or IsInfinite(DataVal) then
      DataVal := 0.0;

    Val := DataVal * Sensitivity;

    // Clamp input to prevent massive spikes
    if Val > 1.0 then Val := 1.0;

    // Generate a wave: Sine carrier modulated by FFT amplitude
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
  Rain drops that react to low frequencies (Bass).
------------------------------------------------------------------------------}
type
  TSkEffectColorDrops = class(TInterfacedObject, ISkVisualizerEffect)
  public
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor);
  end;

procedure TSkEffectColorDrops.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor);
var
  I, Count: Integer;
  X, Y, Radius, BassEnergy, DropSpeed: Single;
  Paint: ISkPaint;
begin
  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.AntiAlias := True;

  // Calculate Bass Energy from the first 20 bins
  BassEnergy := 0;
  for I := 0 to 19 do
    BassEnergy := BassEnergy + Data[I];
  BassEnergy := (BassEnergy / 20) * Sensitivity;

  if BassEnergy < 0 then BassEnergy := 0;
  if BassEnergy > 1.2 then BassEnergy := 1.2;

  // Adjust blur based on intensity
  Paint.ImageFilter := TSkImageFilter.MakeBlur(2 + (BassEnergy * 20), 2 + (BassEnergy * 20));
  DropSpeed := 100 + (BassEnergy * 300);
  Count := 50;

  for I := 0 to Count - 1 do
  begin
    // Deterministic random positions
    X := Sin((I * 132.5) + (Time * 0.5)) * (ADest.Width * 0.45) + ADest.CenterPoint.X;
    Y := Frac((I * 93.7) - (Time * (DropSpeed * 0.005))) * ADest.Height;

    Radius := 2 + Abs(Sin(I * 45));
    Radius := Radius + (BassEnergy * 30) + (Data[I mod 64] * Sensitivity * 10);

    if BassEnergy > 0.4 then
    begin
      Paint.Color := TAlphaColors.White;
      Paint.Alpha := Round(BassEnergy * 180);
    end
    else
    begin
      Paint.Color := AccentColor;
      Paint.Alpha := Round(50 + (BassEnergy * 200));
    end;

    if Radius < 1 then Radius := 1;
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

{------------------------------------------------------------------------------
  DYNAMIC BACKGROUND RENDERER
  Draws animated gradient blobs based on the selected visualizer mode.
------------------------------------------------------------------------------}
procedure TSkiaAVisualizer.DrawDynamicBackground(const ACanvas: ISkCanvas; const ADest: TRectF; const Time: Double);
var
  BgPaint: ISkPaint;
  Gradient: ISkShader;
  I, J: Integer;
  X, Y, R, Offset1, Offset2: Single;
  Center: TPointF;
  Path: ISkPathBuilder;
  Color1, Color2, Color3: TAlphaColor;
  Colors: TArray<TAlphaColor>;
  Positions: TArray<Single>;
begin
  // 1. Draw Base Background (Dark Gradient)
  SetLength(Colors, 2);
  SetLength(Positions, 2);

  Colors[0] := $FF050020;
  Colors[1] := $FF000000;
  Positions[0] := 0.0;
  Positions[1] := 1.0;

  Gradient := TSkShader.MakeGradientLinear(
    PointF(0, 0),
    PointF(0, ADest.Height),
    Colors,
    Positions,
    TSkTileMode.Clamp
  );
  BgPaint := TSkPaint.Create;
  BgPaint.Shader := Gradient;
  ACanvas.DrawRect(ADest, BgPaint);

  // 2. Draw Dynamic Blobs based on Type
  case FVisualType of
    vtSpectrum, vtWave, vtColorDrops:
      begin
        Center := ADest.CenterPoint;
        Color1 := $FF00FFAA;
        Color2 := $FFAA00FF;
        Color3 := $FFFF0055;

        BgPaint.Style := TSkPaintStyle.Fill;
        BgPaint.ImageFilter := TSkImageFilter.MakeBlur(40, 40);

        Offset1 := Sin(Time * 0.7) * (ADest.Height * 0.3);
        Offset2 := Cos(Time * 0.9) * (ADest.Height * 0.4);

        // Blob 1
        R := (ADest.Width * 0.6) + (Sin(Time) * 20);
        Colors[0] := Color1;
        Colors[1] := TAlphaColors.Null;
        Gradient := TSkShader.MakeGradientRadial(
          PointF(Center.X - (ADest.Width * 0.2), ADest.Bottom - Offset1),
          R,
          Colors,
          Positions,
          TSkTileMode.Clamp
        );
        BgPaint.Shader := Gradient;
        ACanvas.DrawRect(ADest, BgPaint);

        // Blob 2
        R := (ADest.Width * 0.5) + (Cos(Time * 1.2) * 20);
        Colors[0] := Color2;
        Gradient := TSkShader.MakeGradientRadial(
          PointF(Center.X + (ADest.Width * 0.2), ADest.Bottom - Offset2),
          R,
          Colors,
          Positions,
          TSkTileMode.Clamp
        );
        BgPaint.Shader := Gradient;
        ACanvas.DrawRect(ADest, BgPaint);

        // Blob 3
        R := (ADest.Width * 0.4);
        Colors[0] := Color3;
        Gradient := TSkShader.MakeGradientRadial(
          PointF(Center.X + (Sin(Time * 0.5) * (ADest.Width * 0.5)), ADest.Bottom - (ADest.Height * 0.2)),
          R,
          Colors,
          Positions,
          TSkTileMode.Clamp
        );
        BgPaint.Shader := Gradient;
        ACanvas.DrawRect(ADest, BgPaint);
      end;

    vtCircle:
      begin
        Center := ADest.CenterPoint;
        R := (ADest.Width * 0.4) + (Sin(Time * 2) * 20);

        Colors[0] := TAlphaColors.Null;
        Colors[1] := $FF220044;
        Gradient := TSkShader.MakeGradientRadial(
          Center,
          R,
          Colors,
          Positions,
          TSkTileMode.Clamp
        );
        BgPaint.Shader := Gradient;
        BgPaint.Style := TSkPaintStyle.Fill;
        ACanvas.DrawRect(ADest, BgPaint);

        Path := TSkPathBuilder.Create;
        Path.AddOval(TRectF.Create(Center.X - R, Center.Y - R, Center.X + R, Center.Y + R));
        BgPaint.Shader := nil;
        BgPaint.Style := TSkPaintStyle.Stroke;
        BgPaint.Color := $FF442244;
        BgPaint.StrokeWidth := 2.0;
        ACanvas.Save;
        ACanvas.Rotate(DegToRad(Sin(Time)*10), Center.X, Center.Y);
        ACanvas.DrawPath(Path.Snapshot, BgPaint);
        ACanvas.Restore;
      end;
  end;
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

procedure TSkiaAVisualizer.CreateEffect;
begin
  case FVisualType of
    vtSpectrum:   FCurrentEffect := TSkEffectSpectrum.Create;
    vtCircle:     FCurrentEffect := TSkEffectCircle.Create;
    vtWave:       FCurrentEffect := TSkEffectWave.Create;
    vtColorDrops: FCurrentEffect := TSkEffectColorDrops.Create;
  else
    FCurrentEffect := nil;
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
  if not FActive then Exit;

  FLock.Acquire;
  try
    FTime := FTime + DeltaSec;
    LocalTime := FTime;

    // Generate Demo Data if not using external BASS input
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
  if csDestroying in ComponentState then Exit;
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
  if Assigned(FThread) then Exit;
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
        if DeltaMS = 0 then DeltaMS := 1;
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
  FUseExternalData := False;
  FSensitivity := 1.5;
  FAccentColor := $FF00FFFF;

  FPrevWidth := 0;
  FPrevHeight := 0;

  CreateEffect;
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
  // Detect resize and ensure the loop continues to paint
  if (FPrevWidth <> Width) or (FPrevHeight <> Height) then
  begin
    FPrevWidth := Width;
    FPrevHeight := Height;
    // Force a redraw immediately in the UI thread
    if not (csDestroying in ComponentState) then
      Redraw;
  end;
end;

procedure TSkiaAVisualizer.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  DataCopy: TFFTData;
  EffectCopy: ISkVisualizerEffect;
  SensitivityCopy: Single;
  AccentCopy: TAlphaColor;
begin
  // 1. Draw Background
  DrawDynamicBackground(ACanvas, ADest, FTime);

  // 2. Copy Data for Thread Safety
  FLock.Acquire;
  try
    DataCopy := FAudioData;
    EffectCopy := FCurrentEffect;
    SensitivityCopy := FSensitivity;
    AccentCopy := FAccentColor;
  finally
    FLock.Release;
  end;

  // 3. Draw Visualization
  if Assigned(EffectCopy) then
    EffectCopy.Draw(ACanvas, ADest, DataCopy, FTime, SensitivityCopy, AccentCopy);
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
