# SkiaAVisualizer
Delphi FMX Skia4delphi Audio Visualizer v0.3 alpha   

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/LaMitaOne/SkiaAVisualizer)
    
A high-performance, hardware-accelerated audio visualizer component for Delphi (FMX). Built on Skia4Delphi, it offers smooth visuals.    
    
<img width="360" height="202" alt="ay195x" src="https://github.com/user-attachments/assets/9ff37d67-79cc-43ad-9dad-7ed82921e38e" />
     
Sample video: https://www.youtube.com/watch?v=auMeGsbSWUQ       
    
    4 Visualization Modes:    
        Spectrum: Classic bar spectrum with neon glow effects.    
        Circle Scope: Radial frequency visualization.    
        Waveform: Oscilloscope-style wave view (centered).    
        Color Drops: "Bass Rain" effect where drops react to low frequencies.    
    Dynamic Backgrounds: Animated gradient blobs that shift based on the selected visualization mode.    
    Customizable: Adjustable sensitivity, FPS limits, and accent colors.    
        
uses bass.dll and BASSStreamRecorder.dll  -> https://www.un4seen.com/       
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
   - Had annoying problems with it on laptops, so redesigned partly, put bass inside again,     
     lots optimizations and now its running smooth even at M3 dualcore cpu (except blobs)      
   
  v 0.2:   
   - Added slower falling Peaks to Spectrum   
   - Added new TSkBackgroundType = btGradientBlobs, btSolidBlack   
   - Added property FBarColor   
    

      
🎨 Skia4Delphi Components:    
   Alive Progress/Loading circle https://github.com/LaMitaOne/SkiaAliveProgress   
   CustomThreadedBase (this i use mostly) https://github.com/LaMitaOne/SkiaCustomThreadedBase    
   Skia Alive Grid https://github.com/LaMitaOne/Skia-AliveGrid    
   Skia-Flowmotion animated image grid https://github.com/LaMitaOne/skia-flowmotion   
   Skia-Slideshow https://github.com/LaMitaOne/Skia-Slideshow   
   Skia-Circlepopup https://github.com/LaMitaOne/skia-circlepopup  
   Skia-CubesPopup https://github.com/LaMitaOne/SkiaCubesPopup   
   Skia-Button https://github.com/LaMitaOne/SkiaButton    
   Skia Desktop Pet https://github.com/LaMitaOne/SkiaDesktopPetBase    
         
🧪 Skia4Delphi experimental Components:    
   CustomMultithreadedBase  https://github.com/LaMitaOne/SkiaCustomMultiThreadedBase    
   Page Control https://github.com/LaMitaOne/SkiaPageControl   
   Surface Widget/modules rendering engine https://github.com/LaMitaOne/MRX-Skia-Surface   
   LCARS fluid engine https://github.com/LaMitaOne/Skia-LCARS-Fluid-Engine   
   Fluid Magma effect https://github.com/LaMitaOne/Fluid-Magma-Effect    
      
