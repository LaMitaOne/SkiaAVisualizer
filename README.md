# SkiaAVisualizer
Delphi FMX Skia4delphi Audio Visualizer v0.1 alpha

A high-performance, hardware-accelerated audio visualizer component for Delphi (FMX). Built on Skia4Delphi, it offers smooth, GPU-rendered visuals.    
     
<img width="646" height="512" alt="Unbenannt" src="https://github.com/user-attachments/assets/4f6a302a-aad3-4056-9dd0-01deba120c1a" />

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
        
Yea color selection btn missing, had to stop, fingers hurting 😝 make few days break better 
