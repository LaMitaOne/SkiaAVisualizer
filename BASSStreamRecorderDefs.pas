//********************************************************************************************************************************
//*                                                                                                                              *
//*     BASS Stream Recorder Library 1.0.9.116 © 3delite 2008-2016                                                               *
//*     See BASS Stream Recorder Library Readme.txt for details                                                                  *
//*                                                                                                                              *
//* Two licenses are available if you like and use this library:                                                                 *
//* Shareware License: €50                                                                                                       *
//* Commercial License: €250                                                                                                     *
//*                                                                                                                              *
//*     http://www.shareit.com/product.html?productid=300294127                                                                  *
//*                                                                                                                              *
//*     http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/bassstreamrecorderlibrary.html                             *
//*                                                                                                                              *
//* If you have any questions or enquiries please mail: 3delite@3delite.hu                                                       *
//*                                                                                                                              *
//* Good coding! :)                                                                                                              *
//* 3delite                                                                                                                      *
//********************************************************************************************************************************

unit BASSStreamRecorderDefs;

interface

Uses
    {$IFDEF MSWINDOWS}
    Windows,
    {$ENDIF}
    Classes;

type
    HBASSStreamRecorder = Pointer;

const
    NAME_BASSStreamRecorderCreate                       = 'BASSStreamRecorder_Create';
    NAME_BASSStreamRecorderFree                         = 'BASSStreamRecorder_Free';
    NAME_BASSStreamRecorderGetParams                    = 'BASSStreamRecorder_GetParams';
    NAME_BASSStreamRecorderSetParams                    = 'BASSStreamRecorder_SetParams';
    NAME_BASSStreamRecorderRecord                       = 'BASSStreamRecorder_Record';
    NAME_BASSStreamRecorderStop                         = 'BASSStreamRecorder_Stop';
    NAME_BASSStreamRecorderStopDual                     = 'BASSStreamRecorder_StopDual';
    NAME_BASSStreamRecorderPause                        = 'BASSStreamRecorder_Pause';
    NAME_BASSStreamRecorderResume                       = 'BASSStreamRecorder_Resume';
    NAME_BASSStreamRecorderGetHandles                   = 'BASSStreamRecorder_GetHandles';
    NAME_BASSStreamRecorderSaveWAVRecordingToFile       = 'BASSStreamRecorder_SaveWAVRecordingToFile';
    NAME_BASSStreamRecorderGetMemoryStream              = 'BASSStreamRecorder_GetMemoryStream';
    NAME_BASSStreamRecorderProgress                     = 'BASSStreamRecorder_Progress';

const
    FORMAT_WAV      = 1;
    FORMAT_ENC      = 2;
    {$IFDEF MSWINDOWS}
    FORMAT_WMA      = 3;
    FORMAT_ACM      = 4;
    {$ENDIF}
    {$IFDEF MACOS}
    FORMAT_CA       = 5;
    {$ENDIF}

const
    {$IFDEF MSWINDOWS}
    BASSStreamRecorderDLL = 'BASSStreamRecorder.dll';
    {$ENDIF}
    {$IFDEF MACOS}
    BASSStreamRecorderDLL = 'libBASSStreamRecorder.dylib';
    {$ENDIF}

{$IFDEF MSWINDOWS}
type
    TACMSettings = record
        FormLen: DWord;
        Form: Pointer;
    end;

type
    TWMATags = record
        Encoder, Genre, Author, Title, Album,
        Composer, Url, Track, Year, CopyRight,
        Comments, Lyrics, Rating, PromotionURL, AlbumCoverURL: PWideChar;
    end;
{$ENDIF}

{$IFDEF MACOS}
type
    Bool = LongBool;
    DWord = Cardinal;
{$ENDIF}

const
    BSR_RECORIDNG_NEW       = 0;
    BSR_RECORDING_CONTINUE  = 1;
    BSR_RECORDING_INSERT    = 2;
    BSR_RECORDING_OVERWRITE = 3;
    BSR_RECORDING_APPEND    = 4;

type
    TBASSStreamRecorderParams = record
        Device: Integer;
        RecInput: Integer;
        SampleRate: Integer;
        NoOfChannels: Integer;
        FileName: PChar;
        Format: Cardinal;
        EncCmdLine: PChar;
        EncoderPriority: Cardinal;
        EncoderFlags: Cardinal;
        {$IFDEF MSWINDOWS}
        WMAVBR: Bool;
        WMAQuality: Integer;
        WMAPro: Bool;
        WMATags: TWMATags;
        ACMSettings: TACMSettings;
        {$ENDIF}
        TryFloatChannel: Bool;
        StartPaused: Bool;
        DualRecording: Bool;
        DualFileNameL: PChar;
        DualFileNameR: PChar;
        DualRecordingL: Bool;
        DualRecordingR: Bool;
        RecordInMemory: Bool;
        ContinueExistingRecordingType: Integer;
        ContinuePosition: Int64;
    end;

type
    TBASSStreamEncodingParams = record
        SampleRate: Integer;
        NoOfChannels: Integer;
        FileName: PChar;
        Format: Cardinal;
        EncCmdLine: PChar;
        EncoderPriority: Cardinal;
        EncoderFlags: Cardinal;
        {$IFDEF MSWINDOWS}
        WMAVBR: Bool;
        WMAQuality: Integer;
        WMAPro: Bool;
        WMATags: TWMATags;
        ACMSettings: TACMSettings;
        {$ENDIF}
        AppendMode: Bool;
    end;

type
    TRecordingHandles = record
        RecordingChannel: DWORD;
        ExtEncoderHandle: THandle;
        {$IFDEF MSWINDOWS}
        WMAEncoderHandle: DWORD;
        DualWMAEncL: DWORD;
        DualWMAEncR: DWORD;
        {$ENDIF}
        DualEncL: DWord;
        DualEncR: DWord;
    end;

type
    t_BASSStreamRecorder_Create             = function: HBASSStreamRecorder; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    t_BASSStreamRecorder_Free               = function (BASSStreamRecorder: HBASSStreamRecorder): Bool; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    t_BASSStreamRecorder_GetParams          = function (BASSStreamRecorder: HBASSStreamRecorder; var Params: TBASSStreamRecorderParams): Bool; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    t_BASSStreamRecorder_SetParams          = function (BASSStreamRecorder: HBASSStreamRecorder; Params: TBASSStreamRecorderParams): Bool; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    t_BASSStreamRecorder_Record             = function (BASSStreamRecorder: HBASSStreamRecorder): Bool; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    t_BASSStreamRecorder_StopDual           = function (BASSStreamRecorder: HBASSStreamRecorder; Channel: Integer; FreeChannel: Bool): Bool; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    t_BASSStreamRecorder_Stop               = function (BASSStreamRecorder: HBASSStreamRecorder; FreeChannel, FreeMemory: Bool): Bool; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    t_BASSStreamRecorder_Pause              = function (BASSStreamRecorder: HBASSStreamRecorder): Bool; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    t_BASSStreamRecorder_Resume             = function (BASSStreamRecorder: HBASSStreamRecorder): Bool; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    t_BASSStreamRecorder_GetHandles         = function (BASSStreamRecorder: HBASSStreamRecorder; var RecordingHandles: TRecordingHandles): Bool; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    t_BASSStreamRecorder_SaveWAVRecordingToFile = function (BASSStreamRecorder: HBASSStreamRecorder; InputFileName: PWideChar; Params: TBASSStreamEncodingParams): Bool; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    t_BASSStreamRecorder_GetMemoryStream    = function (BASSStreamRecorder: HBASSStreamRecorder): TMemoryStream; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    t_BASSStreamRecorder_Progress           = function (BASSStreamRecorder: HBASSStreamRecorder): Integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

var
    BASSStreamRecorder_Create: t_BASSStreamRecorder_Create;
    BASSStreamRecorder_Free: t_BASSStreamRecorder_Free;
    BASSStreamRecorder_GetParams: t_BASSStreamRecorder_GetParams;
    BASSStreamRecorder_SetParams: t_BASSStreamRecorder_SetParams;
    BASSStreamRecorder_Record: t_BASSStreamRecorder_Record;
    BASSStreamRecorder_Stop: t_BASSStreamRecorder_Stop;
    BASSStreamRecorder_StopDual: t_BASSStreamRecorder_StopDual;
    BASSStreamRecorder_Pause: t_BASSStreamRecorder_Pause;
    BASSStreamRecorder_Resume: t_BASSStreamRecorder_Resume;
    BASSStreamRecorder_GetHandles: t_BASSStreamRecorder_GetHandles;
    BASSStreamRecorder_SaveWAVRecordingToFile: t_BASSStreamRecorder_SaveWAVRecordingToFile;
    BASSStreamRecorder_GetMemoryStream: t_BASSStreamRecorder_GetMemoryStream;
    BASSStreamRecorder_Progress: t_BASSStreamRecorder_Progress;

var
    BASSStreamRecorderDLLHandle: THandle = 0;
    BASSStreamRecorderDLLLoaded: Boolean = False;

    function InitBASSStreamRecorderLibrary: Boolean;
    function FreeBASSStreamRecorderLibrary: Boolean;

implementation

Uses
    SysUtils;

function InitBASSStreamRecorderLibrary: Boolean;
begin
    BASSStreamRecorderDLLHandle := LoadLibrary(PChar(BASSStreamRecorderDLL));
    Result := BASSStreamRecorderDLLHandle <> 0;
    if Result then begin
        BASSStreamRecorder_Create               := GetProcAddress(BASSStreamRecorderDLLHandle, PChar(NAME_BASSStreamRecorderCreate));
        BASSStreamRecorder_Free                 := GetProcAddress(BASSStreamRecorderDLLHandle, PChar(NAME_BASSStreamRecorderFree));
        BASSStreamRecorder_GetParams            := GetProcAddress(BASSStreamRecorderDLLHandle, PChar(NAME_BASSStreamRecorderGetParams));
        BASSStreamRecorder_SetParams            := GetProcAddress(BASSStreamRecorderDLLHandle, PChar(NAME_BASSStreamRecorderSetParams));
        BASSStreamRecorder_Record               := GetProcAddress(BASSStreamRecorderDLLHandle, PChar(NAME_BASSStreamRecorderRecord));
        BASSStreamRecorder_Stop                 := GetProcAddress(BASSStreamRecorderDLLHandle, PChar(NAME_BASSStreamRecorderStop));
        BASSStreamRecorder_StopDual             := GetProcAddress(BASSStreamRecorderDLLHandle, PChar(NAME_BASSStreamRecorderStopDual));
        BASSStreamRecorder_Pause                := GetProcAddress(BASSStreamRecorderDLLHandle, PChar(NAME_BASSStreamRecorderPause));
        BASSStreamRecorder_Resume               := GetProcAddress(BASSStreamRecorderDLLHandle, PChar(NAME_BASSStreamRecorderResume));
        BASSStreamRecorder_GetHandles           := GetProcAddress(BASSStreamRecorderDLLHandle, PChar(NAME_BASSStreamRecorderGetHandles));
        BASSStreamRecorder_SaveWAVRecordingToFile := GetProcAddress(BASSStreamRecorderDLLHandle, PChar(NAME_BASSStreamRecorderSaveWAVRecordingToFile));
        BASSStreamRecorder_GetMemoryStream      := GetProcAddress(BASSStreamRecorderDLLHandle, PChar(NAME_BASSStreamRecorderGetMemoryStream));
        BASSStreamRecorder_Progress             := GetProcAddress(BASSStreamRecorderDLLHandle, PChar(NAME_BASSStreamRecorderProgress));
    end;
    if (@BASSStreamRecorder_Create = nil)
    OR (@BASSStreamRecorder_Free = nil)
    OR (@BASSStreamRecorder_GetParams = nil)
    OR (@BASSStreamRecorder_SetParams = nil)
    OR (@BASSStreamRecorder_Record = nil)
    OR (@BASSStreamRecorder_Stop = nil)
    OR (@BASSStreamRecorder_StopDual = nil)
    OR (@BASSStreamRecorder_Pause = nil)
    OR (@BASSStreamRecorder_Resume = nil)
    OR (@BASSStreamRecorder_GetHandles = nil)
    OR (@BASSStreamRecorder_SaveWAVRecordingToFile = nil)
    OR (@BASSStreamRecorder_GetMemoryStream = nil)
    OR (@BASSStreamRecorder_Progress = nil)
        then Result := False;

    if Result
        then BASSStreamRecorderDLLLoaded := True;
end;

function FreeBASSStreamRecorderLibrary: Boolean;
begin
    Result := False;
    if BASSStreamRecorderDLLHandle <> 0 then begin
        Result := FreeLibrary(BASSStreamRecorderDLLHandle);
        if Result then begin
            BASSStreamRecorderDLLLoaded := False;
            BASSStreamRecorderDLLHandle := 0;
        end;
    end;
end;

Initialization

    InitBASSStreamRecorderLibrary;

Finalization

    FreeBASSStreamRecorderLibrary;

end.

