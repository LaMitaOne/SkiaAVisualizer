{*******************************************************************************
  SkiaAudioVisualizer
********************************************************************************
  A high-performance, hardware-accelerated audio visualizer for Delphi FMX.
  Utilizing Skia4Delphi for rendering and BASS for audio processing.

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

unit Unit9;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  FMX.Controls.Presentation, FMX.ListBox, FMX.Colors,
  Math,
  System.SyncObjs, System.AnsiStrings,
  uSkiaAVisualizer,
  Winapi.Windows, FMX.Objects;

type
  // Structure for BASS Device Info
  BASS_DEVICEINFO = record
    name: PAnsiChar;
    driver: PAnsiChar;
    flags: DWORD;
  end;

  TForm9 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { ---------------------------------------------------------------------
      UI CONTROLS
      --------------------------------------------------------------------- }
    FVis: TSkiaAVisualizer;
    FLayout: TLayout;
    FComboDevice: TComboBox;
    FComboEffect: TComboBox;
    FLabelStatus: TLabel;
    FBtnStart: TButton;
    FTrackSensitivity: TTrackBar;
    FLabelSens: TLabel;
    FTrackFPS: TTrackBar;
    FLabelFPS: TLabel;
    FColorButton: TColorButton;
    FBtnApplyColor: TButton;

    { ---------------------------------------------------------------------
      AUDIO ENGINE (BASS)
      --------------------------------------------------------------------- }
    FBassHandle: HMODULE;
    FRecordChannel: DWORD;
    FTimer: TTimer;
    FPrevFFTData: TFFTData;

    // Dynamic BASS Function Imports
    BASS_Init: function(device: Integer; freq, flags: DWORD; win: HWND; cls: Pointer): Boolean; stdcall;
    BASS_Free: function: Boolean; stdcall;
    BASS_RecordInit: function(device: Integer): Boolean; stdcall;
    BASS_RecordStart: function(freq, chans, flags: DWORD; proc: Pointer; user: Pointer): DWORD; stdcall;
    BASS_RecordGetDeviceInfo: function(device: Integer; var info: BASS_DEVICEINFO): Boolean; stdcall;
    BASS_RecordFree: function: Boolean; stdcall;
    BASS_ChannelStop: function(handle: DWORD): Boolean; stdcall;
    BASS_ChannelGetData: function(handle: DWORD; buffer: Pointer; length: DWORD): DWORD; stdcall;
    BASS_ErrorGetCode: function: Integer; stdcall;

    { ---------------------------------------------------------------------
      EVENT HANDLERS
      --------------------------------------------------------------------- }
    procedure PopulateDevices;
    procedure StartVisuals;
    procedure OnBtnStartClick(Sender: TObject);
    procedure OnComboEffectChange(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure OnSensChange(Sender: TObject);
    procedure OnFPSChange(Sender: TObject);
    procedure OnApplyColorClick(Sender: TObject);
  public
  end;

var
  Form9: TForm9;

implementation

{$R *.fmx}

const
  BASS_DEVICE_ENABLED = 1;
  BASS_DATA_FFT2048 = $80000003; // 2048 sample FFT

{==============================================================================
  FORM INITIALIZATION & SETUP
==============================================================================}

procedure TForm9.FormCreate(Sender: TObject);
begin
  // 1. General Form Setup
  Self.Fill.Color := TAlphaColors.Black;

  // 2. Initialize UI Layout
  FLayout := TLayout.Create(Self);
  FLayout.Parent := Self;
  FLayout.Align := TAlignLayout.Bottom;
  FLayout.Height := 250;
  FLayout.Margins.Bottom := 20;
  FLayout.SendToBack;

  // 3. Setup Device Selection Combo
  FComboDevice := TComboBox.Create(Self);
  FComboDevice.Parent := FLayout;
  FComboDevice.Position.Y := 5;
  FComboDevice.Align := TAlignLayout.Top;
  FComboDevice.Margins.Left := 20;
  FComboDevice.Margins.Right := 20;

  // 4. Setup Visualizer Effect Selection
  FComboEffect := TComboBox.Create(Self);
  FComboEffect.Parent := FLayout;
  FComboEffect.Position.Y := 35;
  FComboEffect.Align := TAlignLayout.Top;
  FComboEffect.Margins.Left := 20;
  FComboEffect.Margins.Right := 20;
  FComboEffect.Items.Add('Spectrum (Neon)');
  FComboEffect.Items.Add('Circle Scope');
  FComboEffect.Items.Add('Waveform');
  FComboEffect.Items.Add('Color Drops');
  FComboEffect.ItemIndex := 0;
  FComboEffect.OnChange := OnComboEffectChange;

  // 5. Setup Sensitivity Control
  FLabelSens := TLabel.Create(Self);
  FLabelSens.Parent := FLayout;
  FLabelSens.Position.Y := 65;
  FLabelSens.Text := 'Sensitivity:';
  FLabelSens.Align := TAlignLayout.Top;
  FLabelSens.Margins.Left := 20;
  FLabelSens.Width := 100;

  FTrackSensitivity := TTrackBar.Create(Self);
  FTrackSensitivity.Parent := FLayout;
  FTrackSensitivity.Position.Y := 90;
  FTrackSensitivity.Align := TAlignLayout.Top;
  FTrackSensitivity.Margins.Left := 120;
  FTrackSensitivity.Margins.Right := 20;
  FTrackSensitivity.Min := 10;
  FTrackSensitivity.Max := 100;
  FTrackSensitivity.Value := 15;
  FTrackSensitivity.Frequency := 1;
  FTrackSensitivity.OnChange := OnSensChange;

  // 6. Setup FPS Control
  FLabelFPS := TLabel.Create(Self);
  FLabelFPS.Parent := FLayout;
  FLabelFPS.Position.Y := 120;
  FLabelFPS.Text := 'FPS Limit: 60';
  FLabelFPS.Align := TAlignLayout.Top;
  FLabelFPS.Margins.Left := 20;
  FLabelFPS.Width := 150;
  FLabelFPS.Font.Style := [TFontStyle.fsBold];

  FTrackFPS := TTrackBar.Create(Self);
  FTrackFPS.Parent := FLayout;
  FTrackFPS.Position.Y := 145;
  FTrackFPS.Align := TAlignLayout.Top;
  FTrackFPS.Margins.Left := 120;
  FTrackFPS.Margins.Right := 20;
  FTrackFPS.Min := 15;
  FTrackFPS.Max := 120;
  FTrackFPS.Value := 60;
  FTrackFPS.Frequency := 5;
  FTrackFPS.OnChange := OnFPSChange;

  // 7. Setup Accent Color Picker
  with TLabel.Create(Self) do
  begin
    Parent := FLayout;
    Position.Y := 170;
    Text := 'Accent Color:';
    Align := TAlignLayout.Top;
    Margins.Left := 20;
    Width := 100;
  end;

  // 8. Setup Status Label and Start Button
  FLabelStatus := TLabel.Create(Self);
  FLabelStatus.Parent := FLayout;
  FLabelStatus.Position.Y := 220;
  FLabelStatus.Text := 'Ready';
  FLabelStatus.Align := TAlignLayout.Top;
  FLabelStatus.Margins.Left := 20;

  FBtnStart := TButton.Create(Self);
  FBtnStart.Parent := FLayout;
  FBtnStart.Position.Y := 220;
  FBtnStart.Text := 'Start Visuals';
  FBtnStart.Align := TAlignLayout.Client;
  FBtnStart.Margins.Left := 150;
  FBtnStart.Margins.Right := 20;
  FBtnStart.OnClick := OnBtnStartClick;

  // 9. Load BASS Library and Initialize Functions
  FBassHandle := LoadLibrary('bass.dll');
  if FBassHandle = 0 then
  begin
    FLabelStatus.Text := 'Error: bass.dll not found';
    Exit;
  end;

  @BASS_Init := GetProcAddress(FBassHandle, PAnsiChar('BASS_Init'));
  @BASS_Free := GetProcAddress(FBassHandle, PAnsiChar('BASS_Free'));
  @BASS_RecordInit := GetProcAddress(FBassHandle, PAnsiChar('BASS_RecordInit'));
  @BASS_RecordStart := GetProcAddress(FBassHandle, PAnsiChar('BASS_RecordStart'));
  @BASS_RecordGetDeviceInfo := GetProcAddress(FBassHandle, PAnsiChar('BASS_RecordGetDeviceInfo'));
  @BASS_RecordFree := GetProcAddress(FBassHandle, PAnsiChar('BASS_RecordFree'));
  @BASS_ChannelStop := GetProcAddress(FBassHandle, PAnsiChar('BASS_ChannelStop'));
  @BASS_ChannelGetData := GetProcAddress(FBassHandle, PAnsiChar('BASS_ChannelGetData'));
  @BASS_ErrorGetCode := GetProcAddress(FBassHandle, PAnsiChar('BASS_ErrorGetCode'));

  if Assigned(BASS_Init) then BASS_Init(-1, 44100, 0, 0, nil);
  PopulateDevices;
end;

procedure TForm9.FormDestroy(Sender: TObject);
begin
  // Clean up Timer and BASS resources
  if Assigned(FTimer) then FreeAndNil(FTimer);
  if FRecordChannel <> 0 then BASS_ChannelStop(FRecordChannel);
  if FBassHandle <> 0 then
  begin
    BASS_RecordFree;
    BASS_Free;
    FreeLibrary(FBassHandle);
  end;
  if Assigned(FVis) then FreeAndNil(FVis);
end;

{==============================================================================
  LOGIC & EVENTS
==============================================================================}

procedure TForm9.OnFPSChange(Sender: TObject);
begin
  if Assigned(FVis) then
    FVis.TargetFPS := Trunc(FTrackFPS.Value);
  if Assigned(FLabelFPS) then
    FLabelFPS.Text := 'FPS Limit: ' + IntToStr(Trunc(FTrackFPS.Value));
end;

procedure TForm9.OnSensChange(Sender: TObject);
begin
  if Assigned(FVis) then
    FVis.Sensitivity := FTrackSensitivity.Value / 10.0;
end;

procedure TForm9.OnApplyColorClick(Sender: TObject);
begin
  if Assigned(FVis) then
    FVis.AccentColor := FColorButton.Color;
end;

procedure TForm9.PopulateDevices;
var
  DevInfo: BASS_DEVICEINFO;
  i: Integer;
begin
  FComboDevice.Clear;
  i := 0;
  // Query BASS for available recording devices
  while BASS_RecordGetDeviceInfo(i, DevInfo) do
  begin
    if (DevInfo.flags and BASS_DEVICE_ENABLED) = BASS_DEVICE_ENABLED then
      FComboDevice.Items.Add(string(AnsiString(DevInfo.name)));
    Inc(i);
  end;
  if FComboDevice.Items.Count > 0 then FComboDevice.ItemIndex := 0;
end;

procedure TForm9.StartVisuals;
var
  DevIndex: Integer;
begin
  // 1. Create Visualizer Component if it doesn't exist
  if not Assigned(FVis) then
  begin
    FVis := TSkiaAVisualizer.Create(Self);
    FVis.Parent := Self;
    FVis.Align := TAlignLayout.Client;
  end;

  // 2. Configure Visualizer
  FVis.UseExternalData := True;
  OnSensChange(nil); // Apply current sensitivity
  OnComboEffectChange(nil); // Apply current effect

  // 3. Initialize BASS Recording
  if FRecordChannel <> 0 then BASS_ChannelStop(FRecordChannel);
  BASS_RecordFree;

  DevIndex := FComboDevice.ItemIndex;
  if DevIndex = -1 then DevIndex := 0;

  if not BASS_RecordInit(DevIndex) then
  begin
    FLabelStatus.Text := 'Error: Cannot init device';
    Exit;
  end;

  FRecordChannel := BASS_RecordStart(44100, 2, 0, nil, nil);
  if FRecordChannel = 0 then
  begin
    FLabelStatus.Text := 'Error: Cannot start recording';
    Exit;
  end;

  // 4. Start Data Timer
  FBtnStart.Enabled := False;
  FComboDevice.Enabled := False;
  FLabelStatus.Text := 'Running...';

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 30; // ~30ms update rate
  FTimer.OnTimer := OnTimer;
end;

procedure TForm9.OnBtnStartClick(Sender: TObject);
begin
  StartVisuals;
end;

procedure TForm9.OnComboEffectChange(Sender: TObject);
begin
  if not Assigned(FVis) then Exit;
  case FComboEffect.ItemIndex of
    0: FVis.VisualType := vtSpectrum;
    1: FVis.VisualType := vtCircle;
    2: FVis.VisualType := vtWave;
    3: FVis.VisualType := vtColorDrops;
  end;
end;

procedure TForm9.OnTimer(Sender: TObject);
var
  BASS_Raw: array[0..1023] of Single;
  Vis_Data: TFFTData;
  BASS_Ret: DWORD;
  I: Integer;
begin
  // Safety check
  if (FRecordChannel = 0) or not Assigned(FVis) then Exit;

  // Get raw FFT data from BASS
  BASS_Ret := BASS_ChannelGetData(FRecordChannel, @BASS_Raw, BASS_DATA_FFT2048);
  if BASS_Ret = DWORD(-1) then Exit;

  // Process and Smooth Data
  for I := 0 to High(Vis_Data) do
  begin
    // Validate data
    if IsNan(BASS_Raw[I]) or IsInfinite(BASS_Raw[I]) then
      BASS_Raw[I] := 0;

    if IsNan(FPrevFFTData[I]) or IsInfinite(FPrevFFTData[I]) then
      FPrevFFTData[I] := 0;

    // Average current frame with previous to smooth transitions
    Vis_Data[I] := (BASS_Raw[I] + FPrevFFTData[I]) * 0.5;
    FPrevFFTData[I] := Vis_Data[I];
  end;

  // Feed data to the Skia component
  FVis.UpdateAudioData(Vis_Data);
end;

end.
