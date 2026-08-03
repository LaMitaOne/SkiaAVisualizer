{*******************************************************************************
  SkiaAudioVisualizer
********************************************************************************
  A high-performance, hardware-accelerated audio visualizer for Delphi FMX.
  Utilizing Skia4Delphi for rendering.
  Key Features:
  - Multiple Visualization Modes: Spectrum (with Peaks), Circle, Waveform, Bass Rain.
  - Dynamic Backgrounds: Modular and stable.
*******************************************************************************}
{ Skia-Audio-Visualizer v0.3                                                   }
{ by Lara Miriam Tamy Reschke                                                  }
{                                                                              }
{------------------------------------------------------------------------------}
{
 ----Latest Changes
   v 0.3:
   - Added ShowFallingPeaks property
   - Smoothed bars and peaks to eliminate flickering
   - Replaced heavy ImageFilter with hardware-accelerated MaskFilter
   - Massive performance boost, runs now smooth even at dualcore...with 120fps
     (but not at fullscreen)
   v 0.2:
   - Added slower falling Peaks to Spectrum
   - Added new TSkBackgroundType = btGradientBlobs, btSolidDark, btSolidBlack
   - Added property FBarColor
}

unit uSkiaAVisualizer;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Math, System.UITypes,
  System.SyncObjs, FMX.Types, FMX.Controls, FMX.Skia, System.Skia,
  Winapi.Windows;

const
  MAX_FFT_DATA = 1024;
  BASS_DEVICE_ENABLED = 1;
  BASS_DATA_FFT2048 = $80000003;

type
  TFFTData = array[0..MAX_FFT_DATA - 1] of Single;

  TSkVisualType = (vtSpectrum, vtCircle, vtWave, vtColorDrops);

  TSkBackgroundType = (btGradientBlobs, btSolidBlack);

  ISkVisualizerEffect = interface
    ['{A1B2C3D4-E5F6-4789-0011-223344556677}']
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor; const ShowPeaks: Boolean);
  end;

  ISkBackgroundEffect = interface
    ['{B2C3D4E5-F6A7-4789-0022-334455667788}']
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Time: Double; const Data: TFFTData; const Sensitivity: Single; const AccentColor: TAlphaColor);
  end;

  BASS_DEVICEINFO = record
    name: PAnsiChar;
    driver: PAnsiChar;
    flags: DWORD;
  end;

  TBASS_Init = function(device: Integer; freq, flags: DWORD; win: HWND; cls: Pointer): Boolean; stdcall;

  TBASS_Free = function: Boolean; stdcall;

  TBASS_RecordInit = function(device: Integer): Boolean; stdcall;

  TBASS_RecordStart = function(freq, chans, flags: DWORD; proc: Pointer; user: Pointer): DWORD; stdcall;

  TBASS_RecordGetDeviceInfo = function(device: Integer; var info: BASS_DEVICEINFO): Boolean; stdcall;

  TBASS_RecordFree = function: Boolean; stdcall;

  TBASS_ChannelStop = function(handle: DWORD): Boolean; stdcall;

  TBASS_ChannelGetData = function(handle: DWORD; buffer: Pointer; length: DWORD): DWORD; stdcall;

  TBassAudioCapture = class
  private
    FBassHandle: HMODULE;
    FRecordChannel: DWORD;
    FDeviceID: Integer;
    FInitialized: Boolean;
    FSmoothedData: TFFTData;
    BASS_Init: TBASS_Init;
    BASS_Free: TBASS_Free;
    BASS_RecordInit: TBASS_RecordInit;
    BASS_RecordStart: TBASS_RecordStart;
    BASS_RecordGetDeviceInfo: TBASS_RecordGetDeviceInfo;
    BASS_RecordFree: TBASS_RecordFree;
    BASS_ChannelStop: TBASS_ChannelStop;
    BASS_ChannelGetData: TBASS_ChannelGetData;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadLibrary: Boolean;
    procedure PopulateDevices(List: TStrings);
    function StartRecording(DeviceID: Integer; out ErrorMsg: string): Boolean;
    procedure StopRecording;
    function GetFFTData(out Data: TFFTData): Boolean;
  end;

  TSkiaAVisualizer = class(TSkCustomControl)
  private
    FThread: TThread;
    FLock: TCriticalSection;
    FTime: Double;
    FLastDrawTime: Double;
    FVisualType: TSkVisualType;
    FBackgroundType: TSkBackgroundType;
    FCurrentEffect: ISkVisualizerEffect;
    FCurrentBackground: ISkBackgroundEffect;
    FSensitivity: Single;
    FTargetFPS: Integer;
    FAccentColor: TAlphaColor;
    FBarColor: TAlphaColor;
    FMaxBars: Integer;
    FShowFallingPeaks: Boolean;
    FAudio: TBassAudioCapture;
    FAudioData: TFFTData;
    FIsAudioValid: Boolean;

    procedure SetTargetFPS(const Value: Integer);
    procedure SetAccentColor(const Value: TAlphaColor);
    procedure SetBarColor(const Value: TAlphaColor);
    procedure SetBackgroundType(const Value: TSkBackgroundType);
    procedure SetVisualType(const Value: TSkVisualType);
    procedure SetMaxBars(const Value: Integer);
    procedure SetShowFallingPeaks(const Value: Boolean);
    procedure CreateEffect;
    procedure CreateBackground;
    procedure UpdateLogic(DeltaSec: Double);
    procedure SafeInvalidate;
    procedure StartThread;
    procedure StopThread;
  protected
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ActivateRendering;

    property Audio: TBassAudioCapture read FAudio;
    property TargetFPS: Integer read FTargetFPS write SetTargetFPS;
    property VisualType: TSkVisualType read FVisualType write SetVisualType;
    property BackgroundType: TSkBackgroundType read FBackgroundType write SetBackgroundType;
    property Sensitivity: Single read FSensitivity write FSensitivity;
    property AccentColor: TAlphaColor read FAccentColor write SetAccentColor;
    property BarColor: TAlphaColor read FBarColor write SetBarColor;
    property MaxBars: Integer read FMaxBars write SetMaxBars;
    property ShowFallingPeaks: Boolean read FShowFallingPeaks write SetShowFallingPeaks;
  end;

implementation

{==============================================================================
  BACKGROUND EFFECTS
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
  // Calculate average bass energy to drive the background animation
  BassAvg := 0;
  if Length(Data) > 0 then
    for I := 0 to Min(9, High(Data)) do
      BassAvg := BassAvg + Data[I];
  BassAvg := Min(1.0, BassAvg / Min(10, Length(Data)));

  SetLength(Colors, 2);
  SetLength(Positions, 2);
  Positions[0] := 0.0;
  Positions[1] := 1.0;

  // Draw the dark base gradient
  Colors[0] := $FF050020;
  Colors[1] := $FF000000;
  Gradient := TSkShader.MakeGradientLinear(PointF(0, 0), PointF(0, ADest.Height), Colors, Positions, TSkTileMode.Clamp);
  BgPaint := TSkPaint.Create;
  BgPaint.Shader := Gradient;
  BgPaint.Style := TSkPaintStyle.Fill;
  ACanvas.DrawRect(ADest, BgPaint);

  Center := ADest.CenterPoint;
  Color1 := $FF00FFAA;
  Color2 := $FFAA00FF;
  Color3 := $FFFF0055;

  // Use MaskFilter for high-performance blur over ImageFilter
  BgPaint := TSkPaint.Create;
  BgPaint.Style := TSkPaintStyle.Fill;
  BgPaint.AntiAlias := True;
  BgPaint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurStyle.Normal, 80 + (BassAvg * 40));

  Offset1 := Sin(Time * 0.7) * (ADest.Height * 0.3);
  Offset2 := Cos(Time * 0.9) * (ADest.Height * 0.4);

  // Draw the first animated blob
  R := (ADest.Width * 0.6) + (Sin(Time) * 20);
  Colors[0] := Color1;
  Colors[1] := TAlphaColors.Null;
  Gradient := TSkShader.MakeGradientRadial(PointF(Center.X - (ADest.Width * 0.2), ADest.Bottom - Offset1), R, Colors, Positions, TSkTileMode.Clamp);
  BgPaint.Shader := Gradient;
  ACanvas.DrawCircle(PointF(Center.X - (ADest.Width * 0.2), ADest.Bottom - Offset1), R, BgPaint);

  // Draw the second animated blob
  R := (ADest.Width * 0.5) + (Cos(Time * 1.2) * 20);
  Colors[0] := Color2;
  Gradient := TSkShader.MakeGradientRadial(PointF(Center.X + (ADest.Width * 0.2), ADest.Bottom - Offset2), R, Colors, Positions, TSkTileMode.Clamp);
  BgPaint.Shader := Gradient;
  ACanvas.DrawCircle(PointF(Center.X + (ADest.Width * 0.2), ADest.Bottom - Offset2), R, BgPaint);

  // Draw the third animated blob
  R := (ADest.Width * 0.4);
  Colors[0] := Color3;
  Gradient := TSkShader.MakeGradientRadial(PointF(Center.X + (Sin(Time * 0.5) * (ADest.Width * 0.5)), ADest.Bottom - (ADest.Height * 0.2)), R, Colors, Positions, TSkTileMode.Clamp);
  BgPaint.Shader := Gradient;
  ACanvas.DrawCircle(PointF(Center.X + (Sin(Time * 0.5) * (ADest.Width * 0.5)), ADest.Bottom - (ADest.Height * 0.2)), R, BgPaint);
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
  BgPaint.Color := $FF000000;
  ACanvas.DrawRect(ADest, BgPaint);
end;

{==============================================================================
  VISUALIZER EFFECTS
==============================================================================}

type
  TSkEffectSpectrum = class(TInterfacedObject, ISkVisualizerEffect)
  private
    FPeaks: array of Single;
  public
    constructor Create;
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor; const ShowPeaks: Boolean);
  end;

constructor TSkEffectSpectrum.Create;
begin
  inherited Create;
  SetLength(FPeaks, 512);
end;

procedure TSkEffectSpectrum.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor; const ShowPeaks: Boolean);
var
  I: Integer;
  BarWidth, H, RawH, X, Y, PeakY, FFTIndex: Single;
  BarGlowPaint, BarSolidPaint, PeakGlowPaint, PeakSolidPaint: ISkPaint;
  BarPathBuilder, PeakPathBuilder: ISkPathBuilder;
  BarPath, PeakPath: ISkPath;
  R: TRectF;
  BarsToDraw: Integer;
  DeltaTime, FallSpeed: Single;
begin
  if (ADest.Width <= 0) or (ADest.Height <= 0) then
    Exit;

  BarsToDraw := 64; // Default bar count for spectrum mode
  BarWidth := ADest.Width / BarsToDraw;

  if Length(FPeaks) < BarsToDraw then
    SetLength(FPeaks, BarsToDraw);

  BarPathBuilder := TSkPathBuilder.Create;
  PeakPathBuilder := TSkPathBuilder.Create;

  DeltaTime := 0.016; // Fixed delta time for thread-based updates
  FallSpeed := ADest.Height * 0.15;

  for I := 0 to BarsToDraw - 1 do
  begin
    FFTIndex := (I / BarsToDraw) * 512;
    if FFTIndex >= MAX_FFT_DATA then
      FFTIndex := MAX_FFT_DATA - 1;

    RawH := Power(Min(1.0, Data[Trunc(FFTIndex)] * Sensitivity), 0.8) * ADest.Height * 1.0;
    if RawH > ADest.Height then
      RawH := ADest.Height;
    if RawH < 0 then
      RawH := 0;

    // Apply smoothing to bar transitions
    H := (FPeaks[I] * 0.6) + (RawH * 0.4);
    if H > ADest.Height then
      H := ADest.Height;

    X := I * BarWidth;
    Y := ADest.Bottom - H;

    if BarWidth > 5 then
      R := TRectF.Create(X, Y, X + BarWidth - 2.0, ADest.Bottom)
    else
      R := TRectF.Create(X, Y, X + BarWidth - 1.0, ADest.Bottom);

    if (R.Width > 0) and (R.Height > 0) then
      BarPathBuilder.AddRect(R);

    // Falling peaks logic
    if H > FPeaks[I] then
      FPeaks[I] := (FPeaks[I] * 0.5) + (H * 0.5)
    else
    begin
      FPeaks[I] := FPeaks[I] - (FallSpeed * DeltaTime);
      if FPeaks[I] < 0 then
        FPeaks[I] := 0;
    end;

    PeakY := ADest.Bottom - FPeaks[I];

    if ShowPeaks then
    begin
      if BarWidth > 5 then
        R := TRectF.Create(X, PeakY - 3, X + BarWidth - 2.0, PeakY + 1)
      else
        R := TRectF.Create(X, PeakY - 3, X + BarWidth - 1.0, PeakY + 1);

      if (R.Width > 0) and (R.Height > 0) then
        PeakPathBuilder.AddRect(R);
    end;
  end;

  BarPath := BarPathBuilder.Snapshot;
  PeakPath := PeakPathBuilder.Snapshot;

  // Draw bars with glow and solid fill
  if not BarPath.IsEmpty then
  begin
    BarGlowPaint := TSkPaint.Create;
    BarGlowPaint.Style := TSkPaintStyle.Fill;
    BarGlowPaint.Color := BarColor;
    BarGlowPaint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurStyle.Normal, 10);
    ACanvas.DrawPath(BarPath, BarGlowPaint);

    BarSolidPaint := TSkPaint.Create;
    BarSolidPaint.Style := TSkPaintStyle.Fill;
    BarSolidPaint.Color := BarColor;
    ACanvas.DrawPath(BarPath, BarSolidPaint);
  end;

  // Draw peaks with glow and solid fill
  if not PeakPath.IsEmpty then
  begin
    PeakGlowPaint := TSkPaint.Create;
    PeakGlowPaint.Style := TSkPaintStyle.Fill;
    PeakGlowPaint.Color := AccentColor;
    PeakGlowPaint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurStyle.Normal, 6);
    ACanvas.DrawPath(PeakPath, PeakGlowPaint);

    PeakSolidPaint := TSkPaint.Create;
    PeakSolidPaint.Style := TSkPaintStyle.Fill;
    PeakSolidPaint.Color := AccentColor;
    ACanvas.DrawPath(PeakPath, PeakSolidPaint);
  end;
end;

{------------------------------------------------------------------------------}
type
  TSkEffectCircle = class(TInterfacedObject, ISkVisualizerEffect)
  public
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor; const ShowPeaks: Boolean);
  end;

procedure TSkEffectCircle.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor; const ShowPeaks: Boolean);
var
  I: Integer;
  Angle, Radius, BaseRadius: Single;
  CenterX, CenterY: Single;
  Pt, StartPoint: TPointF;
  GlowPaint, Paint: ISkPaint;
  Path: ISkPathBuilder;
  DataIdx: Integer;
begin
  if (ADest.Width <= 0) or (ADest.Height <= 0) then
    Exit;
  CenterX := ADest.Left + (ADest.Width / 2);
  CenterY := ADest.Top + (ADest.Height / 2);
  BaseRadius := Min(ADest.Width, ADest.Height) * 0.15;
  Path := TSkPathBuilder.Create;

  // Build the circular waveform path
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

  // Draw the glowing outer circle
  GlowPaint := TSkPaint.Create;
  GlowPaint.Style := TSkPaintStyle.Stroke;
  GlowPaint.StrokeWidth := 4.0;
  GlowPaint.Color := AccentColor;
  GlowPaint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurStyle.Normal, 6);
  GlowPaint.StrokeCap := TSkStrokeCap.Round;
  ACanvas.DrawPath(Path.Snapshot, GlowPaint);

  // Draw the solid inner circle
  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 2.0;
  Paint.Color := BarColor;
  Paint.StrokeCap := TSkStrokeCap.Round;
  ACanvas.DrawPath(Path.Snapshot, Paint);
end;

{------------------------------------------------------------------------------}
type
  TSkEffectWave = class(TInterfacedObject, ISkVisualizerEffect)
  public
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor; const ShowPeaks: Boolean);
  end;

procedure TSkEffectWave.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor; const ShowPeaks: Boolean);
var
  I: Integer;
  X, Y, CenterY: Single;
  Val, DataVal: Single;
  Paint: ISkPaint;
  Path: ISkPathBuilder;
begin
  if (ADest.Width <= 0) or (ADest.Height <= 0) then
    Exit;

  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 3.0;
  Paint.Color := AccentColor;
  Paint.AntiAlias := True;
  Paint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurStyle.Normal, 3);

  Path := TSkPathBuilder.Create;
  CenterY := ADest.Top + (ADest.Height / 2);

  // Build the waveform path based on FFT data
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

{------------------------------------------------------------------------------}
type
  TSkEffectColorDrops = class(TInterfacedObject, ISkVisualizerEffect)
  public
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor; const ShowPeaks: Boolean);
  end;

procedure TSkEffectColorDrops.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const Data: TFFTData; const Time: Double; const Sensitivity: Single; const AccentColor: TAlphaColor; const BarColor: TAlphaColor; const ShowPeaks: Boolean);
var
  I, Count: Integer;
  X, Y, Radius, BassEnergy, DropSpeed: Single;
  Paint: ISkPaint;
begin
  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.AntiAlias := True;

  // Calculate bass energy to drive drop size and speed
  BassEnergy := 0;
  for I := 0 to 19 do
    BassEnergy := BassEnergy + Data[I];
  BassEnergy := (BassEnergy / 20) * Sensitivity;
  if BassEnergy < 0 then
    BassEnergy := 0;
  if BassEnergy > 1.2 then
    BassEnergy := 1.2;

  // Use MaskFilter for particle blur effects
  Paint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurStyle.Normal, 2 + (BassEnergy * 15));

  DropSpeed := 100 + (BassEnergy * 300);
  Count := 50;

  // Draw individual color drops
  for I := 0 to Count - 1 do
  begin
    X := Sin((I * 132.5) + (Time * 0.5)) * (ADest.Width * 0.45) + ADest.CenterPoint.X;
    Y := Frac((I * 93.7) - (Time * (DropSpeed * 0.005))) * ADest.Height;
    Radius := 2 + Abs(Sin(I * 45));
    Radius := Radius + (BassEnergy * 30) + (Data[I mod 64] * Sensitivity * 10);

    // Alternate colors based on bass energy
    if BassEnergy > 0.4 then
    begin
      Paint.Color := AccentColor;
      Paint.Alpha := Trunc(BassEnergy * 180);
    end
    else
    begin
      Paint.Color := BarColor;
      Paint.Alpha := Trunc(50 + (BassEnergy * 200));
    end;

    if Radius < 1 then
      Radius := 1;
    ACanvas.DrawCircle(PointF(X, Y), Radius, Paint);
  end;
end;

{==============================================================================
  BASS AUDIO
==============================================================================}

constructor TBassAudioCapture.Create;
begin
  inherited Create;
  FBassHandle := 0;
  FRecordChannel := 0;
  FDeviceID := -1;
  FInitialized := False;
  FillChar(FSmoothedData, SizeOf(FSmoothedData), 0);
end;

destructor TBassAudioCapture.Destroy;
begin
  StopRecording;
  if FBassHandle <> 0 then
  begin
    if Assigned(BASS_Free) then
      BASS_Free;
    FreeLibrary(FBassHandle);
  end;
  inherited;
end;

function TBassAudioCapture.LoadLibrary: Boolean;
begin
  FBassHandle := Winapi.Windows.LoadLibrary('bass.dll');
  Result := FBassHandle <> 0;
  if Result then
  begin
    @BASS_Init := GetProcAddress(FBassHandle, 'BASS_Init');
    @BASS_Free := GetProcAddress(FBassHandle, 'BASS_Free');
    @BASS_RecordInit := GetProcAddress(FBassHandle, 'BASS_RecordInit');
    @BASS_RecordStart := GetProcAddress(FBassHandle, 'BASS_RecordStart');
    @BASS_RecordGetDeviceInfo := GetProcAddress(FBassHandle, 'BASS_RecordGetDeviceInfo');
    @BASS_RecordFree := GetProcAddress(FBassHandle, 'BASS_RecordFree');
    @BASS_ChannelStop := GetProcAddress(FBassHandle, 'BASS_ChannelStop');
    @BASS_ChannelGetData := GetProcAddress(FBassHandle, 'BASS_ChannelGetData');

    if Assigned(BASS_Init) then
      BASS_Init(-1, 44100, 0, 0, nil);
  end;
end;

procedure TBassAudioCapture.PopulateDevices(List: TStrings);
var
  DevInfo: BASS_DEVICEINFO;
  i: Integer;
begin
  if not Assigned(BASS_RecordGetDeviceInfo) then
    Exit;
  List.Clear;
  i := 0;
  while BASS_RecordGetDeviceInfo(i, DevInfo) do
  begin
    if (DevInfo.flags and BASS_DEVICE_ENABLED) = BASS_DEVICE_ENABLED then
      List.Add(string(AnsiString(DevInfo.name)));
    Inc(i);
  end;
end;

function TBassAudioCapture.StartRecording(DeviceID: Integer; out ErrorMsg: string): Boolean;
begin
  ErrorMsg := '';
  if FRecordChannel <> 0 then
    StopRecording;
  if not Assigned(BASS_RecordInit) then
  begin
    ErrorMsg := 'BASS not loaded.';
    Exit(False);
  end;
  FDeviceID := DeviceID;
  if not BASS_RecordInit(FDeviceID) then
  begin
    ErrorMsg := 'Cannot init device.';
    Exit(False);
  end;
  FRecordChannel := BASS_RecordStart(44100, 2, 0, nil, nil);
  if FRecordChannel = 0 then
  begin
    ErrorMsg := 'Cannot start recording.';
    Exit(False);
  end;
  FInitialized := True;
  Result := True;
end;

procedure TBassAudioCapture.StopRecording;
begin
  if FRecordChannel <> 0 then
  begin
    if Assigned(BASS_ChannelStop) then
      BASS_ChannelStop(FRecordChannel);
    FRecordChannel := 0;
  end;
  if Assigned(BASS_RecordFree) then
    BASS_RecordFree;
  FInitialized := False;
end;

function TBassAudioCapture.GetFFTData(out Data: TFFTData): Boolean;
var
  RawData: array[0..MAX_FFT_DATA - 1] of Single;
  Ret: DWORD;
  i: Integer;
  SmoothingFactor: Single;
begin
  Result := False;
  if (FRecordChannel = 0) or not Assigned(BASS_ChannelGetData) then
    Exit;
  Ret := BASS_ChannelGetData(FRecordChannel, @RawData, BASS_DATA_FFT2048);
  if Ret = DWORD(-1) then
    Exit;

  // Apply smoothing to FFT data to prevent jittery visuals
  SmoothingFactor := 0.8;
  for i := 0 to MAX_FFT_DATA - 1 do
  begin
    if IsNan(RawData[i]) or IsInfinite(RawData[i]) then
      RawData[i] := 0;
    if RawData[i] > FSmoothedData[i] then
      FSmoothedData[i] := RawData[i]
    else
      FSmoothedData[i] := (FSmoothedData[i] * SmoothingFactor) + (RawData[i] * (1.0 - SmoothingFactor));
    Data[i] := FSmoothedData[i];
  end;
  Result := True;
end;

{==============================================================================
  VISUALIZER
==============================================================================}

constructor TSkiaAVisualizer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  Align := TAlignLayout.Client;
  HitTest := True;
  FThread := nil;
  FTime := 0;
  FLastDrawTime := 0;
  FVisualType := vtSpectrum;
  FBackgroundType := btGradientBlobs;
  FSensitivity := 2.5;
  FAccentColor := $FF00FFFF;
  FBarColor := $FF008080;
  FMaxBars := 64;
  FShowFallingPeaks := True;
  FillChar(FAudioData, SizeOf(FAudioData), 0);
  FAudio := TBassAudioCapture.Create;
  FAudio.LoadLibrary;
  CreateEffect;
  CreateBackground;
end;

destructor TSkiaAVisualizer.Destroy;
begin
  StopThread;
  FreeAndNil(FAudio);
  FreeAndNil(FLock);
  inherited;
end;

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
    if Assigned(FThread) then
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
    if Assigned(FThread) then
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
      CreateBackground;
    finally
      FLock.Release;
    end;
    if Assigned(FThread) then
      Redraw;
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
    if Assigned(FThread) then
      Redraw;
  end;
end;

procedure TSkiaAVisualizer.SetMaxBars(const Value: Integer);
begin
  if FMaxBars <> Value then
  begin
    FLock.Acquire;
    try
      FMaxBars := Value;
      if FMaxBars < 1 then
        FMaxBars := 1;
      if FMaxBars > MAX_FFT_DATA then
        FMaxBars := MAX_FFT_DATA;
    finally
      FLock.Release;
    end;
    if Assigned(FThread) then
      Redraw;
  end;
end;

procedure TSkiaAVisualizer.SetShowFallingPeaks(const Value: Boolean);
begin
  if FShowFallingPeaks <> Value then
  begin
    FLock.Acquire;
    try
      FShowFallingPeaks := Value;
    finally
      FLock.Release;
    end;
    if Assigned(FThread) then
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
    FCurrentEffect := TSkEffectSpectrum.Create;
  end;
end;

procedure TSkiaAVisualizer.CreateBackground;
begin
  case FBackgroundType of
    btGradientBlobs:
      FCurrentBackground := TSkBgGradientBlobs.Create;
    btSolidBlack:
      FCurrentBackground := TSkBgSolidBlack.Create;
  else
    FCurrentBackground := TSkBgSolidBlack.Create;
  end;
end;

procedure TSkiaAVisualizer.SetTargetFPS(const Value: Integer);
begin
  if FTargetFPS <> Value then
    FTargetFPS := Value;
end;

procedure TSkiaAVisualizer.UpdateLogic(DeltaSec: Double);
begin
  FIsAudioValid := FAudio.GetFFTData(FAudioData);
  FLock.Acquire;
  try
    FTime := FTime + DeltaSec;
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
        Self.Redraw;
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
        UpdateLogic(DeltaMS / 1000);
        SafeInvalidate;
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

procedure TSkiaAVisualizer.StopThread;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    Sleep(50);
    FThread := nil;
  end;
end;

procedure TSkiaAVisualizer.ActivateRendering;
begin
  StartThread;
  Redraw;
end;

procedure TSkiaAVisualizer.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  LocalData: TFFTData;
  LocalBarColor, LocalAccentColor: TAlphaColor;
  LocalSens: Single;
  LocalTime: Double;
  EffectCopy: ISkVisualizerEffect;
  BgCopy: ISkBackgroundEffect;
  LocalShowPeaks: Boolean;
begin
  if not Assigned(FThread) then
    Exit;

  // Acquire lock and copy necessary data for thread-safe rendering
  FLock.Acquire;
  try
    LocalData := FAudioData;
    LocalBarColor := FBarColor;
    LocalAccentColor := FAccentColor;
    LocalSens := FSensitivity;
    LocalTime := FTime;
    BgCopy := FCurrentBackground;
    EffectCopy := FCurrentEffect;
    LocalShowPeaks := FShowFallingPeaks;
  finally
    FLock.Release;
  end;

  // Draw the background effect
  if Assigned(BgCopy) then
    BgCopy.Draw(ACanvas, ADest, LocalTime, LocalData, LocalSens, LocalAccentColor);

  // Draw the visualizer effect
  if Assigned(EffectCopy) then
    EffectCopy.Draw(ACanvas, ADest, LocalData, LocalTime, LocalSens, LocalAccentColor, LocalBarColor, LocalShowPeaks);
end;

end.

