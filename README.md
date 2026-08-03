# SkiaAVisualizer
Delphi FMX Skia4delphi Audio Visualizer v0.3 alpha   

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/LaMitaOne/SkiaAVisualizer)
    
A high-performance, hardware-accelerated audio visualizer component for Delphi (FMX). Built on Skia4Delphi, it offers smooth, GPU-rendered visuals.    
     
<img width="665" height="725" alt="Unbenannt" src="https://github.com/user-attachments/assets/affb2425-6a85-4efb-9b53-f79cddbaa828" />
   
    
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

   
If you want to tip me a coffee.. :)   
    
<p align="center">
  <a href="https://www.paypal.com/donate/?hosted_button_id=RX5KTTMXW497Q">
    <img src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif" alt="Donate with PayPal"/>
  </a>
</p>
        

     
 ----Latest Changes   
   v 0.3:    
   - Added ShowFallingPeaks property    
   - Smoothed bars and peaks to eliminate flickering    
   - Replaced heavy ImageFilter with hardware-accelerated MaskFilter    
   - Massive performance boost, runs now smooth even on M3 dualcore...at 120fps    
     (but not at fullscreen)   
   
  v 0.2:   
   - Added slower falling Peaks to Spectrum   
   - Added new TSkBackgroundType = btGradientBlobs, btSolidDark, btSolidBlack   
   - Added property FBarColor   
    
