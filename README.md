# SkiaAVisualizer
Delphi FMX Skia4delphi Audio Visualizer v0.2 alpha

A high-performance, hardware-accelerated audio visualizer component for Delphi (FMX). Built on Skia4Delphi, it offers smooth, GPU-rendered visuals.    
     
<img width="669" height="729" alt="Unbenannt" src="https://github.com/user-attachments/assets/7204f8e0-8d4f-4601-b0a7-c92f22806473" />
   
It is designed to be audio-library agnostic. It simply accepts TFFTData (array of Single), so you can feed it data from BASS, FMOD, OpenAL, or any other audio engine.    
Features    
    
    4 Visualization Modes:    
        Spectrum: Classic bar spectrum with neon glow effects.    
        Circle Scope: Radial frequency visualization.    
        Waveform: Oscilloscope-style wave view (centered).    
        Color Drops: "Bass Rain" effect where drops react to low frequencies.    
    Dynamic Backgrounds: Animated gradient blobs that shift based on the selected visualization mode.    
    Customizable: Adjustable sensitivity, FPS limits, and accent colors.    
        
Sample project uses bass.dll and BASSStreamRecorder.dll  -> https://www.un4seen.com/       
sample exe (zipped) included    
        
Requirements    
Delphi 10.4 Sydney or later.    
Skia4Delphi.    
         
have fun :D    
    
 ----Latest Changes   
  v 0.2:   
   - Added slower falling Peaks to Spectrum   
   - Added new TSkBackgroundType = btGradientBlobs, btSolidDark, btSolidBlack   
   - Added property FBarColor   
    
