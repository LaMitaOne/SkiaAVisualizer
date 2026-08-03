{*******************************************************************************
  SkiaAudioVisualizer Demo Form
  by Lara Miriam Tamy Reschke
*******************************************************************************}

unit Unit9;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  FMX.Controls.Presentation, FMX.ListBox, FMX.Colors, Math, System.SyncObjs,
  uSkiaAVisualizer, FMX.Objects;

type
  TForm9 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FVis: TSkiaAVisualizer;
    FPanel: TPanel;

    FComboDevice: TComboBox;
    FComboBackground: TComboBox;
    FComboVisual: TComboBox;

    FTrackSensitivity: TTrackBar;
    FTrackFPS: TTrackBar;

    FLabelStatus: TLabel;
    FLabelSens: TLabel;
    FLabelFPS: TLabel;
    FLabelAccent: TLabel;
    FLabelBar: TLabel;

    FBtnStart: TButton;
    FColorButton: TColorButton;
    FColorButton2: TColorButton;
    FColorPicker: TColorPicker;
    FCheckPeaks: TCheckBox;

    procedure PopulateDevices;
    procedure StartVisuals;
    procedure OnBtnStartClick(Sender: TObject);
    procedure OnComboBackgroundChange(Sender: TObject);
    procedure OnComboVisualChange(Sender: TObject);
    procedure OnSensChange(Sender: TObject);
    procedure OnFPSChange(Sender: TObject);
    procedure OnColorButtonClick(Sender: TObject);
    procedure OnPeaksChange(Sender: TObject);
  public
  end;

var
  Form9: TForm9;

implementation
{$R *.fmx}

procedure TForm9.FormCreate(Sender: TObject);
begin
  Self.Fill.Color := TAlphaColors.Black;

  // Initialize the main visualizer component
  FVis := TSkiaAVisualizer.Create(Self);
  FVis.Parent := Self;
  FVis.Align := TAlignLayout.Client;

  // Setup the bottom control panel
  FPanel := TPanel.Create(Self);
  FPanel.Parent := Self;
  FPanel.Align := TAlignLayout.Bottom;
  FPanel.Height := 420;
  FPanel.Margins.Bottom := 20;
  FPanel.Margins.Left := 20;
  FPanel.Margins.Right := 20;

  // Control Panel: Comboboxes for device, background, and visual mode
  FComboDevice := TComboBox.Create(Self);
  FComboDevice.Parent := FPanel;
  FComboDevice.SetBounds(20, 10, 200, 32);

  FComboBackground := TComboBox.Create(Self);
  FComboBackground.Parent := FPanel;
  FComboBackground.SetBounds(230, 10, 150, 32);
  FComboBackground.Items.Add('Solid Black');
  FComboBackground.Items.Add('Gradient Blobs');
  FComboBackground.ItemIndex := 1;
  FComboBackground.OnChange := OnComboBackgroundChange;

  FComboVisual := TComboBox.Create(Self);
  FComboVisual.Parent := FPanel;
  FComboVisual.SetBounds(390, 10, 150, 32);
  FComboVisual.Items.Add('Spectrum');
  FComboVisual.Items.Add('Circle');
  FComboVisual.Items.Add('Wave');
  FComboVisual.Items.Add('Color Drops');
  FComboVisual.ItemIndex := 0;
  FComboVisual.OnChange := OnComboVisualChange;

  // Control Panel: Color picker and start button
  FColorPicker := TColorPicker.Create(Self);
  FColorPicker.Parent := FPanel;
  FColorPicker.SetBounds(20, 60, 200, 120);
  FColorPicker.Color := TAlphaColors.Cyan;

  FBtnStart := TButton.Create(Self);
  FBtnStart.Parent := FPanel;
  FBtnStart.SetBounds(230, 100, 150, 50);
  FBtnStart.Text := 'Start Visuals';
  FBtnStart.OnClick := OnBtnStartClick;

  // Control Panel: Color selection labels
  FLabelAccent := TLabel.Create(Self);
  FLabelAccent.Parent := FPanel;
  FLabelAccent.SetBounds(20, 200, 130, 20);
  FLabelAccent.Text := 'Peak Color (Accent):';

  FLabelBar := TLabel.Create(Self);
  FLabelBar.Parent := FPanel;
  FLabelBar.SetBounds(160, 200, 100, 20);
  FLabelBar.Text := 'Bar Color:';

  // Control Panel: Color buttons and status label
  FColorButton := TColorButton.Create(Self);
  FColorButton.Parent := FPanel;
  FColorButton.SetBounds(20, 230, 50, 25);
  FColorButton.Color := TAlphaColors.Cyan;
  FColorButton.Text := '';
  FColorButton.OnClick := OnColorButtonClick;

  FColorButton2 := TColorButton.Create(Self);
  FColorButton2.Parent := FPanel;
  FColorButton2.SetBounds(160, 230, 50, 25);
  FColorButton2.Color := $FF008080;
  FColorButton2.Text := '';
  FColorButton2.OnClick := OnColorButtonClick;

  FLabelStatus := TLabel.Create(Self);
  FLabelStatus.Parent := FPanel;
  FLabelStatus.SetBounds(240, 230, 150, 25);
  FLabelStatus.Text := 'Ready';
  FLabelStatus.TextAlign := TTextAlign.Leading;

  // Control Panel: Sensitivity trackbar
  FLabelSens := TLabel.Create(Self);
  FLabelSens.Parent := FPanel;
  FLabelSens.SetBounds(20, 280, 100, 20);
  FLabelSens.Text := 'Sensitivity:';

  FTrackSensitivity := TTrackBar.Create(Self);
  FTrackSensitivity.Parent := FPanel;
  FTrackSensitivity.SetBounds(20, 310, 300, 25);
  FTrackSensitivity.Min := 10;
  FTrackSensitivity.Max := 100;
  FTrackSensitivity.Value := 25;
  FTrackSensitivity.Frequency := 1;
  FTrackSensitivity.OnChange := OnSensChange;

  // Control Panel: FPS limit and peak visibility settings
  FLabelFPS := TLabel.Create(Self);
  FLabelFPS.Parent := FPanel;
  FLabelFPS.SetBounds(340, 280, 100, 20);
  FLabelFPS.Text := 'FPS Limit: 30';
  FLabelFPS.Font.Style := [TFontStyle.fsBold];

  FTrackFPS := TTrackBar.Create(Self);
  FTrackFPS.Parent := FPanel;
  FTrackFPS.SetBounds(340, 310, 200, 25);
  FTrackFPS.Min := 15;
  FTrackFPS.Max := 120;
  FTrackFPS.Value := 30;
  FTrackFPS.Frequency := 5;
  FTrackFPS.OnChange := OnFPSChange;

  FCheckPeaks := TCheckBox.Create(Self);
  FCheckPeaks.Parent := FPanel;
  FCheckPeaks.SetBounds(20, 350, 200, 25);
  FCheckPeaks.Text := 'Show Falling Peaks';
  FCheckPeaks.IsChecked := True;
  FCheckPeaks.OnChange := OnPeaksChange;

  // Initialize UI states to match default visualizer settings
  OnSensChange(nil);
  OnFPSChange(nil);
  OnComboBackgroundChange(nil);
  OnComboVisualChange(nil);
  OnColorButtonClick(nil);
  OnPeaksChange(nil);

  PopulateDevices;
end;

procedure TForm9.PopulateDevices;
begin
  if Assigned(FVis) and Assigned(FVis.Audio) then
    FVis.Audio.PopulateDevices(FComboDevice.Items);

  if FComboDevice.Items.Count > 0 then
    FComboDevice.ItemIndex := 0;
end;

procedure TForm9.StartVisuals;
var
  DevIndex: Integer;
  ErrorMsg: string;
begin
  DevIndex := FComboDevice.ItemIndex;
  if DevIndex = -1 then DevIndex := 0;

  if FVis.Audio.StartRecording(DevIndex, ErrorMsg) then
  begin
    FBtnStart.Enabled := False;
    FComboDevice.Enabled := False;
    FLabelStatus.Text := 'Running...';
    FVis.ActivateRendering;
  end
  else
  begin
    FLabelStatus.Text := 'Error: ' + ErrorMsg;
  end;
end;

procedure TForm9.OnBtnStartClick(Sender: TObject);
begin
  StartVisuals;
end;

procedure TForm9.OnComboBackgroundChange(Sender: TObject);
begin
  if not Assigned(FVis) then Exit;
  case FComboBackground.ItemIndex of
    0: FVis.BackgroundType := btSolidBlack;
    1: FVis.BackgroundType := btGradientBlobs;
  end;
end;

procedure TForm9.OnComboVisualChange(Sender: TObject);
begin
  if not Assigned(FVis) then Exit;
  case FComboVisual.ItemIndex of
    0: FVis.VisualType := vtSpectrum;
    1: FVis.VisualType := vtCircle;
    2: FVis.VisualType := vtWave;
    3: FVis.VisualType := vtColorDrops;
  end;
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
      FColorButton.Color := FColorPicker.Color;
      FVis.AccentColor := FColorPicker.Color;
    end
    else if Sender = FColorButton2 then
    begin
      FColorButton2.Color := FColorPicker.Color;
      FVis.BarColor := FColorPicker.Color;
    end;
  end;
end;

procedure TForm9.OnPeaksChange(Sender: TObject);
begin
  if Assigned(FVis) and Assigned(FCheckPeaks) then
    FVis.ShowFallingPeaks := FCheckPeaks.IsChecked;
end;

end.
