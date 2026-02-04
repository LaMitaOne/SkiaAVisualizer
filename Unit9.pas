{*******************************************************************************
  SkiaAudioVisualizer
********************************************************************************
  A high-performance, hardware-accelerated audio visualizer for Delphi FMX.
  Utilizing Skia4Delphi for rendering and BASS for audio processing.
  Key Features:
  - Multiple Visualization Modes: Spectrum, Circle, Waveform, and Bass Rain.
  - Dynamic Backgrounds: Animated gradient blobs reacting to audio type.
*******************************************************************************}
{ Skia-Audio-Visualizer Demo Unit                                                 }
{ by Lara Miriam Tamy Reschke                                                  }
{                                                                              }
{------------------------------------------------------------------------------}


unit Unit9;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  FMX.Controls.Presentation, FMX.ListBox, FMX.Colors, Math, System.SyncObjs,
  System.AnsiStrings, uSkiaAVisualizer, Winapi.Windows, FMX.Objects;

type
  BASS_DEVICEINFO = record
    name: PAnsiChar;
    driver: PAnsiChar;
    flags: DWORD;
  end;

  TForm9 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FVis: TSkiaAVisualizer;
    FLayout: TLayout;

    FComboDevice: TComboBox;
    FComboEffect: TComboBox;
    FComboBackground: TComboBox;

    FTrackSensitivity: TTrackBar;
    FTrackFPS: TTrackBar;

    FLabelStatus: TLabel;
    FLabelSens: TLabel;
    FLabelFPS: TLabel;

    FBtnStart: TButton;
    FColorButton: TColorButton;
    FColorButton2: TColorButton; // NEW: Second Button
    FColorPicker: TColorPicker;

    FBassHandle: HMODULE;
    FRecordChannel: DWORD;
    FTimer: TTimer;
    FPrevFFTData: TFFTData;

    BASS_Init: function(device: Integer; freq, flags: DWORD; win: HWND; cls: Pointer): Boolean; stdcall;
    BASS_Free: function: Boolean; stdcall;
    BASS_RecordInit: function(device: Integer): Boolean; stdcall;
    BASS_RecordStart: function(freq, chans, flags: DWORD; proc: Pointer; user: Pointer): DWORD; stdcall;
    BASS_RecordGetDeviceInfo: function(device: Integer; var info: BASS_DEVICEINFO): Boolean; stdcall;
    BASS_RecordFree: function: Boolean; stdcall;
    BASS_ChannelStop: function(handle: DWORD): Boolean; stdcall;
    BASS_ChannelGetData: function(handle: DWORD; buffer: Pointer; length: DWORD): DWORD; stdcall;
    BASS_ErrorGetCode: function: Integer; stdcall;

    procedure PopulateDevices;
    procedure StartVisuals;
    procedure OnBtnStartClick(Sender: TObject);
    procedure OnComboEffectChange(Sender: TObject);
    procedure OnComboBackgroundChange(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure OnSensChange(Sender: TObject);
    procedure OnFPSChange(Sender: TObject);
    procedure OnColorButtonClick(Sender: TObject);
  public
  end;

var
  Form9: TForm9;

implementation
{$R *.fmx}

const
  BASS_DEVICE_ENABLED = 1;
  BASS_DATA_FFT2048 = $80000003;

procedure TForm9.FormCreate(Sender: TObject);
var
  FRightRowLayout: TLayout;
begin
  Self.Fill.Color := TAlphaColors.Black;

  FillChar(FPrevFFTData, SizeOf(FPrevFFTData), 0);

  FLayout := TLayout.Create(Self);
  FLayout.Parent := Self;
  FLayout.Align := TAlignLayout.Bottom;
  FLayout.Height := 350;
  FLayout.Margins.Bottom := 20;
  FLayout.SendToBack;

  FComboDevice := TComboBox.Create(Self);
  FComboDevice.Parent := FLayout;
  FComboDevice.Position.Y := 5;
  FComboDevice.Align := TAlignLayout.Top;
  FComboDevice.Margins.Left := 20;
  FComboDevice.Margins.Right := 20;

  FComboEffect := TComboBox.Create(Self);
  FComboEffect.Parent := FLayout;
  FComboEffect.Position.Y := 35;
  FComboEffect.Align := TAlignLayout.Top;
  FComboEffect.Margins.Left := 20;
  FComboDevice.Margins.Right := 20;
  FComboEffect.Items.Add('Spectrum (Neon)');
  FComboEffect.Items.Add('Circle Scope');
  FComboEffect.Items.Add('Waveform');
  FComboEffect.Items.Add('Color Drops');
  FComboEffect.ItemIndex := 0;
  FComboEffect.OnChange := OnComboEffectChange;

  FComboBackground := TComboBox.Create(Self);
  FComboBackground.Parent := FLayout;
  FComboBackground.Position.Y := 65;
  FComboBackground.Align := TAlignLayout.Top;
  FComboBackground.Margins.Left := 20;
  FComboBackground.Margins.Right := 20;
  FComboBackground.Items.Add('Gradient Blobs');
  FComboBackground.Items.Add('Solid Dark');
  FComboBackground.Items.Add('Solid Black');
  FComboBackground.ItemIndex := 0;
  FComboBackground.OnChange := OnComboBackgroundChange;

  FLabelSens := TLabel.Create(Self);
  FLabelSens.Parent := FLayout;
  FLabelSens.Position.Y := 95;
  FLabelSens.Text := 'Sensitivity:';
  FLabelSens.Align := TAlignLayout.Top;
  FLabelSens.Margins.Left := 20;
  FLabelSens.Width := 100;
  FLabelSens.TextAlign := TTextAlign.Leading;

  FTrackSensitivity := TTrackBar.Create(Self);
  FTrackSensitivity.Parent := FLayout;
  FTrackSensitivity.Position.Y := 115;
  FTrackSensitivity.Align := TAlignLayout.Top;
  FTrackSensitivity.Margins.Left := 120;
  FTrackSensitivity.Margins.Right := 20;
  FTrackSensitivity.Min := 10;
  FTrackSensitivity.Max := 100;
  FTrackSensitivity.Value := 15;
  FTrackSensitivity.Frequency := 1;
  FTrackSensitivity.OnChange := OnSensChange;

  FLabelFPS := TLabel.Create(Self);
  FLabelFPS.Parent := FLayout;
  FLabelFPS.Position.Y := 145;
  FLabelFPS.Text := 'FPS Limit: 60';
  FLabelFPS.Align := TAlignLayout.Top;
  FLabelFPS.Margins.Left := 20;
  FLabelFPS.Width := 150;
  FLabelFPS.Font.Style := [TFontStyle.fsBold];

  FTrackFPS := TTrackBar.Create(Self);
  FTrackFPS.Parent := FLayout;
  FTrackFPS.Position.Y := 170;
  FTrackFPS.Align := TAlignLayout.Top;
  FTrackFPS.Margins.Left := 120;
  FTrackFPS.Margins.Right := 20;
  FTrackFPS.Min := 15;
  FTrackFPS.Max := 120;
  FTrackFPS.Value := 60;
  FTrackFPS.Frequency := 5;
  FTrackFPS.OnChange := OnFPSChange;

  // Color Labels
  with TLabel.Create(Self) do
  begin
    Parent := FLayout;
    Position.Y := 200;
    Text := 'Peak Color (Accent):';
    Align := TAlignLayout.Top;
    Margins.Left := 20;
    Width := 130;
  end;

  with TLabel.Create(Self) do
  begin
    Parent := FLayout;
    Position.Y := 200;
    Text := 'Bar Color:';
    Align := TAlignLayout.Top;
    Margins.Left := 160; // Position next to first label
    Width := 100;
  end;

  // First Color Button (Accent/Peaks)
  FColorButton := TColorButton.Create(Self);
  FColorButton.Parent := FLayout;
  FColorButton.Position.Y := 220;
  FColorButton.Align := TAlignLayout.Top;
  FColorButton.Margins.Left := 20;
  FColorButton.Color := TAlphaColors.Cyan;
  FColorButton.Width := 50;
  FColorButton.Height := 25;
  FColorButton.Text := '';
  FColorButton.OnClick := OnColorButtonClick;

  // Second Color Button (Bars)
  FColorButton2 := TColorButton.Create(Self);
  FColorButton2.Parent := FLayout;
  FColorButton2.Position.Y := 220;
  FColorButton2.Align := TAlignLayout.Top;
  FColorButton2.Margins.Left := 80; // Position next to first button
  FColorButton2.Color := $FF008080; // Dark Teal default
  FColorButton2.Width := 50;
  FColorButton2.Height := 25;
  FColorButton2.Text := '';
  FColorButton2.OnClick := OnColorButtonClick;

  FLabelStatus := TLabel.Create(Self);
  FLabelStatus.Parent := FLayout;
  FLabelStatus.Position.Y := 220;
  FLabelStatus.Text := 'Ready';
  FLabelStatus.Align := TAlignLayout.Top;
  FLabelStatus.Margins.Left := 140;
  FLabelStatus.TextAlign := TTextAlign.Leading;

  FRightRowLayout := TLayout.Create(Self);
  FRightRowLayout.Parent := FLayout;
  FRightRowLayout.Position.Y := 255;
  FRightRowLayout.Align := TAlignLayout.Top;
  FRightRowLayout.Height := 50;
  FRightRowLayout.HitTest := True;

  FColorPicker := TColorPicker.Create(Self);
  FColorPicker.Parent := FRightRowLayout;
  FColorPicker.Align := TAlignLayout.Left;
  FColorPicker.Margins.Left := 20;
  FColorPicker.Color := TAlphaColors.Cyan;

  FBtnStart := TButton.Create(Self);
  FBtnStart.Parent := FRightRowLayout;
  FBtnStart.Text := 'Start Visuals';
  FBtnStart.Align := TAlignLayout.Right;
  FBtnStart.Margins.Right := 20;
  FBtnStart.OnClick := OnBtnStartClick;

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

  if Assigned(BASS_Init) then
    BASS_Init(-1, 44100, 0, 0, nil);

  PopulateDevices;
end;

procedure TForm9.FormDestroy(Sender: TObject);
begin
  if Assigned(FTimer) then
    FreeAndNil(FTimer);
  if FRecordChannel <> 0 then
    BASS_ChannelStop(FRecordChannel);
  if FBassHandle <> 0 then
  begin
    BASS_RecordFree;
    BASS_Free;
    FreeLibrary(FBassHandle);
  end;
  if Assigned(FVis) then
    FreeAndNil(FVis);
end;

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

procedure TForm9.OnColorButtonClick(Sender: TObject);
begin
  if Assigned(FColorPicker) and Assigned(FVis) then
  begin
    if Sender = FColorButton then
    begin
      // Button 1 was clicked -> Set Accent/Peaks
      FColorButton.Color := FColorPicker.Color;
      FVis.AccentColor := FColorPicker.Color;
    end
    else if Sender = FColorButton2 then
    begin
      // Button 2 was clicked -> Set Bars
      FColorButton2.Color := FColorPicker.Color;
      FVis.BarColor := FColorPicker.Color;
    end;
  end;
end;

procedure TForm9.PopulateDevices;
var
  DevInfo: BASS_DEVICEINFO;
  i: Integer;
begin
  FComboDevice.Clear;
  i := 0;
  while BASS_RecordGetDeviceInfo(i, DevInfo) do
  begin
    if (DevInfo.flags and BASS_DEVICE_ENABLED) = BASS_DEVICE_ENABLED then
      FComboDevice.Items.Add(string(AnsiString(DevInfo.name)));
    Inc(i);
  end;
  if FComboDevice.Items.Count > 0 then
    FComboDevice.ItemIndex := 0;
end;

procedure TForm9.StartVisuals;
var
  DevIndex: Integer;
begin
  if not Assigned(FVis) then
  begin
    FVis := TSkiaAVisualizer.Create(Self);
    FVis.Parent := Self;
    FVis.Align := TAlignLayout.Client;
    if Assigned(FComboBackground) then
      OnComboBackgroundChange(nil);
  end;

  FVis.UseExternalData := True;
  OnSensChange(nil);
  OnComboEffectChange(nil);

  // Initialize colors from the buttons
  if Assigned(FColorButton) then
    FVis.AccentColor := FColorButton.Color;
  if Assigned(FColorButton2) then
    FVis.BarColor := FColorButton2.Color;

  if FRecordChannel <> 0 then
    BASS_ChannelStop(FRecordChannel);
  BASS_RecordFree;
  DevIndex := FComboDevice.ItemIndex;
  if DevIndex = -1 then
    DevIndex := 0;
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

  FBtnStart.Enabled := False;
  FComboDevice.Enabled := False;
  FLabelStatus.Text := 'Running...';
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 30;
  FTimer.OnTimer := OnTimer;
end;

procedure TForm9.OnBtnStartClick(Sender: TObject);
begin
  StartVisuals;
end;

procedure TForm9.OnComboEffectChange(Sender: TObject);
begin
  if not Assigned(FVis) then
    Exit;
  case FComboEffect.ItemIndex of
    0:
      FVis.VisualType := vtSpectrum;
    1:
      FVis.VisualType := vtCircle;
    2:
      FVis.VisualType := vtWave;
    3:
      FVis.VisualType := vtColorDrops;
  end;
end;

procedure TForm9.OnComboBackgroundChange(Sender: TObject);
begin
  if not Assigned(FVis) then
    Exit;
  case FComboBackground.ItemIndex of
    0:
      FVis.BackgroundType := btGradientBlobs;
    1:
      FVis.BackgroundType := btSolidDark;
    2:
      FVis.BackgroundType := btSolidBlack;
  end;
end;

procedure TForm9.OnTimer(Sender: TObject);
var
  BASS_Raw: array[0..1023] of Single;
  Vis_Data: TFFTData;
  BASS_Ret: DWORD;
  I: Integer;
begin
  if (FRecordChannel = 0) or not Assigned(FVis) then
    Exit;

  BASS_Ret := BASS_ChannelGetData(FRecordChannel, @BASS_Raw, BASS_DATA_FFT2048);
  if BASS_Ret = DWORD(-1) then
    Exit;

  for I := 0 to High(Vis_Data) do
  begin
    if IsNan(BASS_Raw[I]) or IsInfinite(BASS_Raw[I]) then
      BASS_Raw[I] := 0;
    if IsNan(FPrevFFTData[I]) or IsInfinite(FPrevFFTData[I]) then
      FPrevFFTData[I] := 0;

    Vis_Data[I] := (BASS_Raw[I] + FPrevFFTData[I]) * 0.5;
    FPrevFFTData[I] := Vis_Data[I];
  end;

  FVis.UpdateAudioData(Vis_Data);
end;

end.

