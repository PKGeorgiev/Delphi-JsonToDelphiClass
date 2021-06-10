unit System.Console;

interface

uses
  SysUtils, Classes,
  {$IF CompilerVersion >= 22}
  UITypes, Winapi.Windows,
  {$ELSE}
    Windows,
  {$IFEND}
  Types;

{$M+}
{$IFDEF CONDITIONALEXPRESSIONS}
{$IF RTLVersion >= 14.0}
{$DEFINE HASERROUTPUT}
{$IFEND}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$SCOPEDENUMS ON}

const
  Int16_MaxValue = 32767;
{$IF CompilerVersion <= 2500} //Delphi XE4
const
  { Virtual Keys, Standard Set }
  vkLButton          = $01;  {   1 }
  vkRButton          = $02;  {   2 }
  vkCancel           = $03;  {   3 }
  vkMButton          = $04;  {   4 }
  vkXButton1         = $05;  {   5 }
  vkXButton2         = $06;  {   6 }
  vkBack             = $08;  {   8 }
  vkTab              = $09;  {   9 }
  vkLineFeed         = $0A;  {  10 }
  vkClear            = $0C;  {  12 }
  vkReturn           = $0D;  {  13 }
  vkShift            = $10;  {  16 }
  vkControl          = $11;  {  17 }
  vkMenu             = $12;  {  18 }
  vkPause            = $13;  {  19 }
  vkCapital          = $14;  {  20 }
  vkKana             = $15;  {  21 }
  vkHangul           = $15;  {  21 }
  vkJunja            = $17;  {  23 }
  vkFinal            = $18;  {  24 }
  vkHanja            = $19;  {  25 }
  vkKanji            = $19;  {  25 }
  vkConvert          = $1C;  {  28 }
  vkNonConvert       = $1D;  {  29 }
  vkAccept           = $1E;  {  30 }
  vkModeChange       = $1F;  {  31 }
  vkEscape           = $1B;  {  27 }
  vkSpace            = $20;  {  32 }
  vkPrior            = $21;  {  33 }
  vkNext             = $22;  {  34 }
  vkEnd              = $23;  {  35 }
  vkHome             = $24;  {  36 }
  vkLeft             = $25;  {  37 }
  vkUp               = $26;  {  38 }
  vkRight            = $27;  {  39 }
  vkDown             = $28;  {  40 }
  vkSelect           = $29;  {  41 }
  vkPrint            = $2A;  {  42 }
  vkExecute          = $2B;  {  43 }
  vkSnapshot         = $2C;  {  44 }
  vkInsert           = $2D;  {  45 }
  vkDelete           = $2E;  {  46 }
  vkHelp             = $2F;  {  47 }
  { vk0 thru vk9 are the same as ASCII '0' thru '9' ($30 - $39) }
  vk0                = $30;  {  48 }
  vk1                = $31;  {  49 }
  vk2                = $32;  {  50 }
  vk3                = $33;  {  51 }
  vk4                = $34;  {  52 }
  vk5                = $35;  {  53 }
  vk6                = $36;  {  54 }
  vk7                = $37;  {  55 }
  vk8                = $38;  {  56 }
  vk9                = $39;  {  57 }
  { vkA thru vkZ are the same as ASCII 'A' thru 'Z' ($41 - $5A) }
  vkA                = $41;  {  65 }
  vkB                = $42;  {  66 }
  vkC                = $43;  {  67 }
  vkD                = $44;  {  68 }
  vkE                = $45;  {  69 }
  vkF                = $46;  {  70 }
  vkG                = $47;  {  71 }
  vkH                = $48;  {  72 }
  vkI                = $49;  {  73 }
  vkJ                = $4A;  {  74 }
  vkK                = $4B;  {  75 }
  vkL                = $4C;  {  76 }
  vkM                = $4D;  {  77 }
  vkN                = $4E;  {  78 }
  vkO                = $4F;  {  79 }
  vkP                = $50;  {  80 }
  vkQ                = $51;  {  81 }
  vkR                = $52;  {  82 }
  vkS                = $53;  {  83 }
  vkT                = $54;  {  84 }
  vkU                = $55;  {  85 }
  vkV                = $56;  {  86 }
  vkW                = $57;  {  87 }
  vkX                = $58;  {  88 }
  vkY                = $59;  {  89 }
  vkZ                = $5A;  {  90 }
  vkLWin             = $5B;  {  91 }
  vkRWin             = $5C;  {  92 }
  vkApps             = $5D;  {  93 }
  vkSleep            = $5F;  {  95 }
  vkNumpad0          = $60;  {  96 }
  vkNumpad1          = $61;  {  97 }
  vkNumpad2          = $62;  {  98 }
  vkNumpad3          = $63;  {  99 }
  vkNumpad4          = $64;  { 100 }
  vkNumpad5          = $65;  { 101 }
  vkNumpad6          = $66;  { 102 }
  vkNumpad7          = $67;  { 103 }
  vkNumpad8          = $68;  { 104 }
  vkNumpad9          = $69;  { 105 }
  vkMultiply         = $6A;  { 106 }
  vkAdd              = $6B;  { 107 }
  vkSeparator        = $6C;  { 108 }
  vkSubtract         = $6D;  { 109 }
  vkDecimal          = $6E;  { 110 }
  vkDivide           = $6F;  { 111 }
  vkF1               = $70;  { 112 }
  vkF2               = $71;  { 113 }
  vkF3               = $72;  { 114 }
  vkF4               = $73;  { 115 }
  vkF5               = $74;  { 116 }
  vkF6               = $75;  { 117 }
  vkF7               = $76;  { 118 }
  vkF8               = $77;  { 119 }
  vkF9               = $78;  { 120 }
  vkF10              = $79;  { 121 }
  vkF11              = $7A;  { 122 }
  vkF12              = $7B;  { 123 }
  vkF13              = $7C;  { 124 }
  vkF14              = $7D;  { 125 }
  vkF15              = $7E;  { 126 }
  vkF16              = $7F;  { 127 }
  vkF17              = $80;  { 128 }
  vkF18              = $81;  { 129 }
  vkF19              = $82;  { 130 }
  vkF20              = $83;  { 131 }
  vkF21              = $84;  { 132 }
  vkF22              = $85;  { 133 }
  vkF23              = $86;  { 134 }
  vkF24              = $87;  { 135 }

  vkCamera           = $88;  { 136 }
  vkHardwareBack     = $89;  { 137 }

  vkNumLock          = $90;  { 144 }
  vkScroll           = $91;  { 145 }
  vkLShift           = $A0;  { 160 }
  vkRShift           = $A1;  { 161 }
  vkLControl         = $A2;  { 162 }
  vkRControl         = $A3;  { 163 }
  vkLMenu            = $A4;  { 164 }
  vkRMenu            = $A5;  { 165 }

  vkBrowserBack      = $A6;  { 166 }
  vkBrowserForward   = $A7;  { 167 }
  vkBrowserRefresh   = $A8;  { 168 }
  vkBrowserStop      = $A9;  { 169 }
  vkBrowserSearch    = $AA;  { 170 }
  vkBrowserFavorites = $AB;  { 171 }
  vkBrowserHome      = $AC;  { 172 }
  vkVolumeMute       = $AD;  { 173 }
  vkVolumeDown       = $AE;  { 174 }
  vkVolumeUp         = $AF;  { 175 }
  vkMediaNextTrack   = $B0;  { 176 }
  vkMediaPrevTrack   = $B1;  { 177 }
  vkMediaStop        = $B2;  { 178 }
  vkMediaPlayPause   = $B3;  { 179 }
  vkLaunchMail       = $B4;  { 180 }
  vkLaunchMediaSelect= $B5;  { 181 }
  vkLaunchApp1       = $B6;  { 182 }
  vkLaunchApp2       = $B7;  { 183 }

  vkSemicolon        = $BA;  { 186 }
  vkEqual            = $BB;  { 187 }
  vkComma            = $BC;  { 188 }
  vkMinus            = $BD;  { 189 }
  vkPeriod           = $BE;  { 190 }
  vkSlash            = $BF;  { 191 }
  vkTilde            = $C0;  { 192 }
  vkLeftBracket      = $DB;  { 219 }
  vkBackslash        = $DC;  { 220 }
  vkRightBracket     = $DD;  { 221 }
  vkQuote            = $DE;  { 222 }
  vkPara             = $DF;  { 223 }

  vkOem102           = $E2;  { 226 }
  vkIcoHelp          = $E3;  { 227 }
  vkIco00            = $E4;  { 228 }
  vkProcessKey       = $E5;  { 229 }
  vkIcoClear         = $E6;  { 230 }
  vkPacket           = $E7;  { 231 }
  vkAttn             = $F6;  { 246 }
  vkCrsel            = $F7;  { 247 }
  vkExsel            = $F8;  { 248 }
  vkErEof            = $F9;  { 249 }
  vkPlay             = $FA;  { 250 }
  vkZoom             = $FB;  { 251 }
  vkNoname           = $FC;  { 252 }
  vkPA1              = $FD;  { 253 }
  vkOemClear         = $FE;  { 254 }
  vkNone             = $FF;  { 255 }
  {$IFEND}


type
  ECOnsoleError = class(Exception);
  TConsoleColor = (
    Black,
    DarkBlue,
    DarkGreen,
    DarkCyan,
    DarkRed,
    DarkMagenta,
    DarkYellow,
    Gray,
    DarkGray,
    Blue,
    Green,
    Cyan,
    Red,
    Magenta,
    Yellow,
    White
    );

  TConsoleKey = (
      Backspace = vkBack,  Tab       = vkTab,    Clear = vkClear,  Enter   = vkReturn,  Pause       = vkPause,    Escape  = vkEscape,  Spacebar   = vkSpace,
      PageUp    = vkPrior, PageDown  = vkNext,   &End  = vkEnd,    Home    = vkHome,    LeftArrow   = vkLeft,     UpArrow = vkUp,      RightArrow = vkRight,
      DownArrow = vkDown,  Select    = vkSelect, Print = vkPrint,  Execute = vkExecute, PrintScreen = vkSnapshot, Insert  = vkInsert,  Delete     = vkDelete,
      Help      = vkHelp,

      D0 = vk0,     D1 = vk1,     D2 = vk2,     D3 = vk3,     D4 = vk4,     D5 = vk5,     D6 = vk6,     D7 = vk7,      D8 = vk8,     D9 = vk9,
      A  = vkA,      B = vkB,      C = vkC,      D = vkD,      E = vkE,      F = vkF,      G = vkG,      H = vkH,       I = vkI,      J = vkJ,
      K  = vkK,      L = vkL,      M = vkM,      N = vkN,      O = vkO,      P = vkP,      Q = vkQ,      R = vkR,       S = vkS,      T = vkT,
      U  = vkU,      V = vkV,      W = vkW,      X = vkX,      Y = vkY,      Z = vkZ,

      LeftWindows = vkLWin,     RightWindows = vkRWin,  Applications = vkApps,      Sleep     = vkSleep,
      Multiply    = vkMultiply, Add          = vkAdd,   Separator    = vkSeparator, Subtract  = vkSubtract, Decimal = vkDecimal, Divide = vkDivide,

      NumPad0 = vkNumpad0,      NumPad1 = vkNumpad1,      NumPad2 = vkNumpad2,      NumPad3 = vkNumpad3,      NumPad4 = vkNumpad4,
      NumPad5 = vkNumpad5,      NumPad6 = vkNumpad6,      NumPad7 = vkNumpad7,      NumPad8 = vkNumpad8,      NumPad9 = vkNumpad9,


      F1  =  vkF1,   F2 =  vkF2,  F3 =  vkF3,  F4 =  vkF4,  F5 =  vkF5,  F6 =  vkF6,  F7 =  vkF7,  F8  = vkF8,   F9 =  vkF9,
      F10 = vkF10,  F11 = vkF11, F12 = vkF12, F13 = vkF13, F14 = vkF14, F15 = vkF15, F16 = vkF16, F17 = vkF17,  F18 = vkF18,
      F19 = vkF19,  F20 = vkF20, F21 = vkF21, F22 = vkF22, F23 = vkF23, F24 = vkF24,

      BrowserBack   = vkBrowserBack,    BrowserForward    = vkBrowserForward,   BrowserRefresh = vkBrowserRefresh,  BrowserStop       = vkBrowserStop,
      BrowserSearch = vkBrowserSearch,  BrowserFavorites  = vkBrowserFavorites, BrowserHome    = vkBrowserHome,     VolumeMute        = vkVolumeMute,
      VolumeDown    = vkVolumeDown,     VolumeUp          = vkVolumeUp,         MediaNext      = vkMediaNextTrack,  MediaPrevious     = vkMediaPrevTrack,
      MediaStop     = vkMediaStop,      MediaPlay         = vkMediaPlayPause,   LaunchMail     = vkLaunchMail,      LaunchMediaSelect = vkLaunchMediaSelect,
      LaunchApp1    = vkLaunchApp1,     LaunchApp2        = vkLaunchApp2,       Oem1           = vkSemicolon,       OemPlus           = vkEqual,
      OemComma      = vkComma,          OemMinus          = vkMinus,            OemPeriod      = vkPeriod,          Oem2              = vkSlash,
      Oem3          = vkTilde,          Oem4              = vkLeftBracket,      Oem5           = vkBackslash,       Oem6              = vkRightBracket,
      Oem7          = vkQuote,          Oem8              = vkPara,             Oem102         = vkOem102,

      Process       = vkProcessKey,     Packet            = vkPacket,           Attention      = vkAttn,            CrSel             = vkCrsel,
      ExSel         = vkExsel,          EraseEndOfFile    = vkErEof,            Play           = vkPlay,            Zoom              = vkZoom,
      NoName        = vkNoname,         Pa1               = vkPA1,              OemClear       = vkOemClear
  );

  TConsoleModifiers = (Alt, Shift, Control);

  TConsoleModifiersSet = set of TConsoleModifiers;

  TConsoleKeyInfo = record
  strict private
    FKey: TConsoleKey;
    FKeyChar: Char;
    FMods: TConsoleModifiersSet;
  public
    property Key: TConsoleKey read FKey write FKey;
    property KeyChar: Char read FKeyChar write FKeyChar;
    property Modifiers: TConsoleModifiersSet read FMods;
  end;

  TCONSOLE_FONT_INFOEX = record
    cbSize: Cardinal;
    nFont: LongWord;
    dwFontSize: COORD;
    FontFamily: Cardinal;
    FontWeight: Cardinal;
    FaceName: array [0 .. LF_FACESIZE - 1] of WideChar;
  end;

  pCONSOLE_FONT_INFOEX = ^TCONSOLE_FONT_INFOEX;

  TFontFamily =
  (
    ffDontCare  = FF_DONTCARE,
    Roman       = FF_ROMAN,
    Swiss       = FF_SWISS,
    Modern      = FF_MODERN,
    Script      = FF_SCRIPT,
    Decorative  = FF_DECORATIVE
  );

  TFontWeight =
  (
    fwDontCare  = FW_DONTCARE,
    Thin        = FW_THIN,
    ExtraLight  = FW_EXTRALIGHT,
    Normal      = FW_NORMAL,
    Medium      = FW_MEDIUM,
    SemiBold    = FW_SEMIBOLD,
    Bold        = FW_BOLD,
    ExtraBold   = FW_EXTRABOLD,
    Heavy       = FW_HEAVY,
    UltraLight  = FW_ULTRALIGHT,
    Regular     = FW_NORMAL,
    DemiBold    = FW_SEMIBOLD,
    UltraBold   = FW_EXTRABOLD,
    Black       = FW_HEAVY
  );

   TWinColor =
  (
      colBackgroundBlue       = $10,
      colBackgroundGreen      = $20,
      colBackgroundRed        = $40,
      colBackgroundYellow     = $60,
      colBackgroundIntensity  = $80,
      colBackgroundMask       = $F0,
      colBlack                = $00,

      colColorMask            = $FF,
      colForegroundBlue       =   1,
      colForegroundGreen      =   2,
      colForegroundRed        =   4,
      colForegroundYellow     =   6,
      colForegroundIntensity  =   8,
      colForegroundMask       =  15
  );
  Console = class
  private
    class var DefaultTextAttributes: Word;
    class var FScreenSize: TCoord;
    class var FTextWindow: TRect;
    class var StdErr: THandle;
    class var StdIn: THandle;
    class var StdOut: THandle;
    class var TextAttr: Word;
    class var FAutoAllocateConsole : Boolean;

    class procedure RaiseConsoleError(aCaller : String);
    class function ConsoleColorToColorAttribute(ConsoleColor: TConsoleColor; IsBackground: Boolean): TWinColor; static;
    class function ConsoleCursorInfo: TConsoleCursorInfo;
    class function ConsoleRect: TRect;
    class function GetBufferInfo: TConsoleScreenBufferInfo; static;
    class function CanGetBufferInfo: Boolean; static;
    class function GetConsoleOutputHandle: THandle; static;
    class procedure GotoXY(X, Y: SmallInt);
    class procedure ScrollScreenBuffer(Left, Top, Right, Bottom: Integer; Distance: Integer = 0);
    class procedure SetConsoleOutputHandle(const Value: THandle); static;
    class procedure SetConsoleRect(Rect: TRect);
    class function WriteString(aValue: string): Cardinal; inline;
    class function GenericToString<T>(aValue: T): string;
    class function GetConsoleRedirected(const Index: Integer): Boolean; static;
    class function GetBackgroundColor: TConsoleColor; static;
    class procedure SetBackgroundColor(const Value: TConsoleColor); static;
    class function GetForegroundColor: TConsoleColor; static;
    class procedure SetForegroundColor(const Value: TConsoleColor); static;
    class function GetBufferHeight: Integer; static;
    class procedure SetBufferHeight(const Value: Integer); static;
    class function GetBufferWidth: Integer; static;
    class procedure SetBufferWidth(const Value: Integer); static;
    class function GetLargestWindowWidth: Integer; static;
    class function GetLargestWindowHeight: Integer; static;
    class function GetWindowLeft: Integer; static;
    class procedure SetWindowLeft(const Value: Integer); static;
    class function GetWindowTop: Integer; static;
    class procedure SetWindowTop(const Value: Integer); static;
    class function GetCursorLeft: Integer; static;
    class procedure SetCursorLeft(const Value: Integer); static;
    class function GetCursorTop: Integer; static;
    class procedure SetCursorTop(const Value: Integer); static;
    class function GetCursorSize: Integer; static;
    class procedure SetCursorSize(const Value: Integer); static;
    class function GetCursorVisible: Boolean; static;
    class procedure SetCursorVisible(const Value: Boolean); static;
    class function GetTitle: string; static;
    class procedure SetTitle(const Value: string); static;
    class function GetKeyAvailable: Boolean; static;
    class function GetKey(const Index: Integer): Boolean; static;
    class function GetTreatControlCAsInput: Boolean; static;
    class procedure SetTreatControlCAsInput(const Value: Boolean); static;
    class function GetConsoleInputHandle: Integer; static;
    class procedure SetConsoleInputHandle(const Value: Integer); static;
    class function GetBufferSize: TCoord; static;
    class procedure SetBufferSize(const Value: TCoord); overload; static;
    class function GetWindowWidth: Integer; static;
    class procedure SetWindowWidth(const Value: Integer); static;
    class function GetWindowHeight: Integer; static;
    class procedure SetWindowHeight(const Value: Integer); static;
    class function GetOutputEncoding: DWORD; static;
    class procedure SetOutputEncoding(const Value: DWORD); static;
    class function GetConsoleFont: TCONSOLE_FONT_INFOEX; static;
    class procedure SetConsoleFont(const Value: TCONSOLE_FONT_INFOEX); static;
  public
    // Not implemented
    // class function OpenStandardError(BufferSize: Integer): TStream; overload; static;
    // class function OpenStandardError: TStream; overload; static;
    // class function OpenStandardInput(BufferSize: Integer): TStream; overload; static;
    // class function OpenStandardInput: TStream; overload; static;
    // class function OpenStandardOutput(BufferSize: Integer): TStream; overload; static;
    // class function OpenStandardOutput: TStream; overload; static;
    // class procedure SetError(NewError: TTextWriter); static;
    // class procedure SetIn(NewIn: TTextReader); static;
    // Class procedure SetOut(NewOut: TTextWriter); static;

    // Initialize
    class constructor Create;
    class destructor Destroy;
    class function AttatchConsole : Boolean;
    class procedure AllocateConsole;
    class procedure FreeConsole;
    // Methods
    class procedure Beep(Frequency, Duration: Cardinal); overload; static;
    class procedure Beep; overload; static;
    class procedure Clear; static;
    class procedure ClearEOL; static;
    class procedure DeleteLine;
    class procedure InsertLine;
    class procedure MoveBufferArea(SourceLeft, SourceTop, SourceWidth, SourceHeight, TargetLeft, TargetTop: Integer); overload;
    class procedure MoveBufferArea(SourceLeft, SourceTop, SourceWidth, SourceHeight, TargetLeft, TargetTop: Integer; SourceChar: Char; SourceForeColor, SourceBackColor: TConsoleColor); overload;
    class function Read: Integer; static;
    class function ReadKey(Intercept: Boolean): TConsoleKeyInfo; overload; static;
    class function ReadKey: TConsoleKeyInfo; overload; static;
    class function ReadLine: string; static;
    class procedure ResetColor;
    class procedure SetCursorPosition(Left, Top: SmallInt);
    class procedure SetBufferSize(Width, Height: Integer); overload; static;
    class procedure SetWindowPosition(Left, Top: Integer);
    class procedure SetWindowSize(Width, Height: Integer); static;
    class procedure UpdateConsoleFont(aFontName: string = ''; aFontSize: Cardinal = 0; aFontFamily: TFontFamily = TFontFamily.ffDontCare; aFontWeight: TFontWeight = TFontWeight.fwDontCare);
    class procedure Write<T>(aValue: T); overload; static;
    class procedure Write(Value: Variant; Args: array of const); overload; static;
    class procedure WriteLine<T>(aValue: T); overload; static;
    class procedure WriteLine(FormatString: String; Args: array of Variant); overload; static;
    class procedure WriteLine; overload; static;

    // properties
    class property AutoAllocateConsole : Boolean read FAutoAllocateConsole write FAutoAllocateConsole;
    class property BackgroundColor: TConsoleColor read GetBackgroundColor write SetBackgroundColor;
    class property BufferHeight: Integer read GetBufferHeight write SetBufferHeight;
    class property BufferSize: TCoord read GetBufferSize write SetBufferSize;
    class property BufferWidth: Integer read GetBufferWidth write SetBufferWidth;
    class property CapsLock: Boolean index VK_CAPITAL read GetKey;
    class property ConsoleInputHandle: Integer read GetConsoleInputHandle write SetConsoleInputHandle;
    class property ConsoleOutputHandle: THandle read GetConsoleOutputHandle write SetConsoleOutputHandle;
    class property ConsoleFont: TCONSOLE_FONT_INFOEX read GetConsoleFont write SetConsoleFont;
    class property CursorLeft: Integer read GetCursorLeft write SetCursorLeft;
    class property CursorSize: Integer read GetCursorSize write SetCursorSize;
    class property CursorTop: Integer read GetCursorTop write SetCursorTop;
    class property CursorVisible: Boolean read GetCursorVisible write SetCursorVisible;
    class property ForegroundColor: TConsoleColor read GetForegroundColor write SetForegroundColor;
    class property IsErrorRedirected: Boolean index STD_ERROR_HANDLE read GetConsoleRedirected;
    class property IsInputRedirected: Boolean index STD_INPUT_HANDLE read GetConsoleRedirected;
    class property IsOutputRedirected: Boolean index STD_OUTPUT_HANDLE read GetConsoleRedirected;
    class property KeyAvailable: Boolean read GetKeyAvailable;
    class property LargestWindowWidth: Integer read GetLargestWindowWidth;
    class property LargestWindowHeight: Integer read GetLargestWindowHeight;
    class property OutputEncoding: DWORD read GetOutputEncoding write SetOutputEncoding;

    class property WindowHeight: Integer read GetWindowHeight write SetWindowHeight;
    class property WindowWidth: Integer read GetWindowWidth write SetWindowWidth;
    class property NumberLock: Boolean index VK_NUMLOCK read GetKey;
    class property Title: string read GetTitle write SetTitle;
    class property TreatControlCAsInput: Boolean read GetTreatControlCAsInput write SetTreatControlCAsInput;
    class property WindowLeft: Integer read GetWindowLeft write SetWindowLeft;
    class property WindowTop: Integer read GetWindowTop write SetWindowTop;
  end;

function AttachConsole(dwProcessId: DWORD): Bool; stdcall; external KERNEL32 name 'AttachConsole';
function GetConsoleWindow: HWND; stdcall; external kernel32 name 'GetConsoleWindow';
function GetCurrentConsoleFontEx(ConsoleOutput: THandle; MaximumWindow: BOOL; ConsoleInfo: pCONSOLE_FONT_INFOEX): BOOL; stdcall; external kernel32 name 'GetCurrentConsoleFontEx';
function SetCurrentConsoleFontEx(ConsoleOutput: THandle; MaximumWindow: BOOL; ConsoleInfo: pCONSOLE_FONT_INFOEX): BOOL; stdcall; external kernel32 name 'SetCurrentConsoleFontEx';

implementation

uses
  StrUtils, RTTI, TypInfo;

var
  LockObject: TObject;

function Lock(ALockObject: TObject; ATimeout: Cardinal = INFINITE): Boolean;
begin
  Result := System.TMonitor.Enter(LockObject, ATimeout)
end;

procedure Release(ALockObject: TObject; ATimeout: Cardinal = INFINITE);
begin
  System.TMonitor.Exit(LockObject);
end;
{ Console }

class function Console.AttatchConsole: Boolean;
const
  ATTACH_PARENT_PROCESS : UINT = $0ffffffff;
begin
  {$IFNDEF Win32}
    if IsDebuggerPresent then
      Exit(false);
  (*
    In the case of a 64-bit debug, it appears that the IDE is creating a console for the debugger,
    which hosts the debugged application as a child, and so electing to attach to the parent console succeeds.

    This console is not created for a 32-bit debug session.
  *)
  {$ENDIF}

    Result := AttachConsole(ATTACH_PARENT_PROCESS);
  if Result then
    AllocateConsole;
end;

class procedure Console.Beep;
begin
  Beep;
end;

class procedure Console.Beep(Frequency, Duration: Cardinal);
begin
  {$IF CompilerVersion >= 23}
  Winapi.
  {$IFEND}
  Windows.Beep(Frequency, Duration);
end;

class procedure Console.Clear;
var
  StartPos: TCoord;
  Buffer : TConsoleScreenBufferInfo;
  ConSize : Integer;
  NumWritten: DWORD;
begin
  if StdOut = INVALID_HANDLE_VALUE then
    exit;

  Buffer := GetBufferInfo;
  ConSize := Buffer.dwSize.X * Buffer.dwSize.Y;
  StartPos.X := 0;
  StartPos.y := 0;

  if not FillConsoleOutputCharacter(StdOut, ' ', ConSize, StartPos, NumWritten) then
     RaiseConsoleError('FillConsoleOutputCharacter in Console.Clear');

  NumWritten := 0;

  if not FillConsoleOutputAttribute(StdOut, Buffer.wAttributes, ConSize, StartPos, NumWritten) then
     RaiseConsoleError('FillConsoleOutputAttribute in Console.Clear');

  GotoXY(1, 1);
end;

class procedure Console.ClearEOL;
var
  Len: Integer;
  Pos: TCoord;
  NumWritten: DWORD;
begin
  if StdOut = INVALID_HANDLE_VALUE then
    exit;

  Pos := GetBufferInfo.dwCursorPosition;
  if Pos.X > FTextWindow.Right then
    Exit;

  Len := FTextWindow.Right - Pos.X + 1;
  FillConsoleOutputCharacter(StdOut, ' ', Len, Pos, NumWritten);
  FillConsoleOutputAttribute(StdOut, TextAttr, Len, Pos, NumWritten);
end;

class function Console.ConsoleColorToColorAttribute(ConsoleColor: TConsoleColor; IsBackground: Boolean): TWinColor;
begin
  if ((Integer(ConsoleColor) and not Integer(TConsoleColor.White)) <> Integer(TConsoleColor.Black)) then
    raise EArgumentException.Create('InvalidConsoleColor');

  Result := TWinColor(ConsoleColor);

  if (IsBackground) then
    Result := TWinColor(SmallInt(Integer(Result) shl 4));
end;

class function Console.ConsoleCursorInfo: TConsoleCursorInfo;
begin
  GetConsoleCursorInfo(ConsoleOutputHandle, Result);
end;

class function Console.ConsoleRect: TRect;
begin
  ZeroMemory(@Result, SizeOf(TRect));
  GetWindowRect(GetConsoleWindow, Result);
end;

class function Console.GetConsoleRedirected(const Index: Integer): Boolean;
var
  FileType: DWORD;
begin
  FileType := GetFileType(GetStdHandle(index));
  Result := (FileType = FILE_TYPE_PIPE) or (FileType = FILE_TYPE_DISK);
end;

class procedure Console.AllocateConsole;
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  if not System.Console.AttachConsole(DWORD(-1)) then
  begin
     if FAutoAllocateConsole then
       AllocConsole
     else
       Exit;
  end;

  StdIn := GetStdHandle(STD_INPUT_HANDLE);
  StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  StdErr := GetStdHandle(STD_ERROR_HANDLE);

  Reset(Input);
  Rewrite(Output);
  Rewrite(ErrOutput);

  TextAttr := GetBufferInfo.wAttributes and $FF;
  DefaultTextAttributes := TextAttr;

  if not GetConsoleScreenBufferInfo(StdOut, BufferInfo) then
  begin
    SetInOutRes(GetLastError);
    Exit;
  end;

  FTextWindow.Left := 0;
  FTextWindow.Top := 0;
  FTextWindow.Right := BufferInfo.dwSize.X - 1;
  FTextWindow.Bottom := BufferInfo.dwSize.Y - 1;
  FScreenSize.X := BufferInfo.srWindow.Right - BufferInfo.srWindow.Left + 1;
  FScreenSize.Y := BufferInfo.srWindow.Bottom - BufferInfo.srWindow.Top + 1;
end;

class constructor Console.Create;
begin
  StdIn := GetStdHandle(STD_INPUT_HANDLE);
  StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  StdErr := GetStdHandle(STD_ERROR_HANDLE);
  FAutoAllocateConsole := True;

  if Console.CanGetBufferInfo then
    Console.AllocateConsole;
end;

class procedure Console.DeleteLine;
begin
  ScrollScreenBuffer(FTextWindow.Left, GetBufferInfo.dwCursorPosition.Y, FTextWindow.Right, FTextWindow.Bottom, -1);
end;

class destructor Console.Destroy;
begin
  FreeConsole;
end;

class procedure Console.FreeConsole;
begin
  {$IF CompilerVersion >= 23}
  Winapi.
  {$IFEND}
  Windows.FreeConsole;
end;

class function Console.GenericToString<T>(aValue: T): string;
var
  ElementValue, Value: TValue;
  Data: PTypeData;
  I: Integer;
  AContext: TRttiContext;
  ARecord: TRttiRecordType;
begin
  TValue.Make(@aValue, System.TypeInfo(T), Value);

  if Value.IsArray then
  begin
    if Value.GetArrayLength = 0 then
      Exit('[ø]');

    Result := '[';

    for I := 0 to Value.GetArrayLength - 1 do
    begin
      ElementValue := Value.GetArrayElement(I);
      Result := Result + ElementValue.ToString + ',';
    end;

    Result[Length(Result)] := ']';
    Exit;
  end;

  Data := GetTypeData(Value.TypeInfo);

  if (Value.IsObject) and (Value.TypeInfo^.Kind <> tkInterface) then
     Exit(Format('0x%p %s', [pointer(Value.AsObject), Data.ClassType.ClassName]));

  if Value.TypeInfo^.Kind = tkRecord then
  begin
    AContext := TRttiContext.Create;
    ARecord := AContext.GetType(Value.TypeInfo).AsRecord;
    Exit(Format('0x%p (Record ''%s'' @ %p)', [Value.GetReferenceToRawData, ARecord.Name, Data]));
  end;

  Result := Value.ToString;
end;

class function Console.GetBackgroundColor: TConsoleColor;
begin
  Result := TConsoleColor((TextAttr and $0F) shr 4);
end;

class function Console.GetBufferHeight: Integer;
begin
  Result := GetBufferSize.Y;
end;

class function Console.CanGetBufferInfo: Boolean;
var
  Dummy: TConsoleScreenBufferInfo;
begin
  Result := GetConsoleScreenBufferInfo(ConsoleOutputHandle, Dummy);
end;

class function Console.GetBufferInfo: TConsoleScreenBufferInfo;
begin
  if StdOut = INVALID_HANDLE_VALUE then
    exit;

  if not GetConsoleScreenBufferInfo(ConsoleOutputHandle, Result) then
    RaiseConsoleError('GetBufferInfo');
end;

class function Console.GetBufferSize: TCoord;
begin
  Result := GetBufferInfo.dwSize;
end;

class function Console.GetBufferWidth: Integer;
begin
  Result := GetBufferSize.X;
end;

class function Console.GetConsoleFont: TCONSOLE_FONT_INFOEX;
begin
  FillChar(Result, SizeOf(TCONSOLE_FONT_INFOEX), 0);
  Result.cbSize := SizeOf(TCONSOLE_FONT_INFOEX);
  GetCurrentConsoleFontEx(ConsoleOutputHandle, FALSE, @Result);
end;

class function Console.GetConsoleInputHandle: Integer;
begin
  Result := StdIn;
end;

class function Console.GetConsoleOutputHandle: THandle;
begin
  Result := StdOut;
end;

class function Console.GetCursorLeft: Integer;
begin
  Result := GetBufferInfo.dwCursorPosition.X
end;

class function Console.GetCursorSize: Integer;
begin
  Result := ConsoleCursorInfo.dwSize;
end;

class function Console.GetCursorTop: Integer;
begin
  Result := GetBufferInfo.dwCursorPosition.Y
end;

class function Console.GetCursorVisible: Boolean;
begin
  Result := ConsoleCursorInfo.bVisible;
end;

class function Console.GetForegroundColor: TConsoleColor;
begin
  Result := TConsoleColor(TextAttr and $0F);
end;

class function Console.GetKey(const Index: Integer): Boolean;
begin
  Result := (GetKeyState(index) and 1) = 1;
end;

class function Console.GetKeyAvailable: Boolean;
var
  NumberOfEvents: DWORD;
  Buffer: TInputRecord;
  NumberOfEventsRead: DWORD;
begin
  Result := FALSE;

  if StdIn = INVALID_HANDLE_VALUE then
    exit;

  NumberOfEvents := 0;
  GetNumberOfConsoleInputEvents(StdIn, NumberOfEvents);

  if NumberOfEvents = 0 then
    Exit;

  PeekConsoleInput(StdIn, Buffer, 1, NumberOfEventsRead);
  if NumberOfEventsRead = 0 then
    Exit;

  if Buffer.EventType = KEY_EVENT then // is a Keyboard event?
  begin
    if Buffer.Event.KeyEvent.bKeyDown then // the key was pressed?
      Result := True
    else
      FlushConsoleInputBuffer(StdIn); // flush the buffer
  end
  else
    FlushConsoleInputBuffer(StdIn); // flush the buffer
end;

class function Console.GetLargestWindowHeight: Integer;
begin
  Result := GetLargestConsoleWindowSize(ConsoleOutputHandle).Y;
end;

class function Console.GetLargestWindowWidth: Integer;
begin
  Result := GetLargestConsoleWindowSize(ConsoleOutputHandle).X
end;

class function Console.GetOutputEncoding: DWORD;
begin
  Result := GetConsoleOutputCP;
end;

class function Console.GetTitle: string;
var
  TitleLength: Integer;
begin
  if GetConsoleWindow = 0 then
    exit('');

  TitleLength := GetWindowTextLength(GetConsoleWindow);
  SetLength(Result, TitleLength);
  GetWindowText(GetConsoleWindow, PChar(Result), TitleLength + 1);
end;

class function Console.GetTreatControlCAsInput: Boolean;
var
  Mode: Cardinal;
begin
  Result := FALSE;
  Mode := 0;
  if not GetConsoleMode(ConsoleInputHandle, Mode) then
    RaiseLastOSError
  else
    Result := (Mode and 1) = 0;
end;

class function Console.GetWindowHeight: Integer;
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  BufferInfo := GetBufferInfo;
  Result := BufferInfo.srWindow.Bottom - BufferInfo.srWindow.Top + 1;
end;

class function Console.GetWindowLeft: Integer;
begin
  Result := ConsoleRect.Left;
end;

class function Console.GetWindowTop: Integer;
begin
  Result := ConsoleRect.Top;
end;

class function Console.GetWindowWidth: Integer;
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  BufferInfo := GetBufferInfo;
  Result := BufferInfo.srWindow.Right - BufferInfo.srWindow.Left + 1;
end;

class procedure Console.GotoXY(X, Y: SmallInt);
begin
  Inc(X, FTextWindow.Left - 1);
  Inc(Y, FTextWindow.Top - 1);
  if PtInRect(FTextWindow, POINT(X, Y)) then
    SetCursorPosition(X, Y);
end;

class procedure Console.InsertLine;
begin
  ScrollScreenBuffer(FTextWindow.Left, GetBufferInfo.dwCursorPosition.Y, FTextWindow.Right, FTextWindow.Bottom, 1);
end;

class procedure Console.MoveBufferArea(SourceLeft, SourceTop, SourceWidth, SourceHeight, TargetLeft, TargetTop: Integer);
begin
  MoveBufferArea(SourceLeft, SourceTop, SourceWidth, SourceHeight, TargetLeft, TargetTop, ' ', TConsoleColor.Black, BackgroundColor)
end;

class procedure Console.MoveBufferArea(SourceLeft, SourceTop, SourceWidth, SourceHeight, TargetLeft, TargetTop: Integer; SourceChar: Char; SourceForeColor, SourceBackColor: TConsoleColor);
var
  I: Integer;
  CharInfoArray: array of CHAR_INFO;
  NumberOfCharsWritten: DWORD;
  dwSize: TCoord;
  ReadRegion, WriteRegion: SMALL_RECT;
  dwWriteCoord, bufferCoord: TCoord;
  wColorAttribute, color: TWinColor;
begin
  if StdOut = INVALID_HANDLE_VALUE then
    exit;

  if ((SourceForeColor < TConsoleColor.Black) or (SourceForeColor > TConsoleColor.White)) then
    raise EArgumentException.Create('ParamName: SourceForeColor');
  if ((SourceBackColor < TConsoleColor.Black) or (SourceBackColor > TConsoleColor.White)) then
    raise EArgumentException.Create('ParamName: SourceBackColor');

  dwSize := GetBufferInfo.dwSize;

  if ((SourceLeft < 0) or (SourceLeft > dwSize.X)) then
    raise EArgumentOutOfRangeException.Create('SourceLeft ' + IntToStr(SourceLeft) + 'ArgumentOutOfRange: ConsoleBufferBoundaries');
  if ((SourceTop < 0) or (SourceTop > dwSize.Y)) then
    raise EArgumentOutOfRangeException.Create('SourceTop' + IntToStr(SourceTop) + 'ArgumentOutOfRange: ConsoleBufferBoundaries');
  if ((SourceWidth < 0) or (SourceWidth > (dwSize.X - SourceLeft))) then
    raise EArgumentOutOfRangeException.Create('SourceWidth' + IntToStr(SourceWidth) + 'ArgumentOutOfRange: ConsoleBufferBoundaries');
  if ((SourceHeight < 0) or (SourceTop > (dwSize.Y - SourceHeight))) then
    raise EArgumentOutOfRangeException.Create('SourceHeight' + IntToStr(SourceHeight) + 'ArgumentOutOfRange: ConsoleBufferBoundaries');
  if ((TargetLeft < 0) or (TargetLeft > dwSize.X)) then
    raise EArgumentOutOfRangeException.Create('TargetLeft' + IntToStr(TargetLeft) + 'ArgumentOutOfRange: ConsoleBufferBoundaries');
  if ((TargetTop < 0) or (TargetTop > dwSize.Y)) then
    raise EArgumentOutOfRangeException.Create('TargetTop' + IntToStr(TargetTop) + 'ArgumentOutOfRange: ConsoleBufferBoundaries');

  if ((SourceWidth <> 0) and (SourceHeight <> 0)) then
  begin
    SetLength(CharInfoArray, (SourceWidth * SourceHeight));
    dwSize.X := SourceWidth;
    dwSize.Y := SourceHeight;
    ReadRegion.Left := (SourceLeft);
    ReadRegion.Right := (((SourceLeft + SourceWidth) - 1));
    ReadRegion.Top := (SourceTop);
    ReadRegion.Bottom := (((SourceTop + SourceHeight) - 1));

    bufferCoord.X := 0;
    bufferCoord.Y := 0;

    if (not ReadConsoleOutput(ConsoleOutputHandle, CharInfoArray, dwSize, bufferCoord, ReadRegion)) then
      RaiseLastOSError;

    dwWriteCoord.X := SourceLeft;
    color := TWinColor(Integer(ConsoleColorToColorAttribute(SourceBackColor, True)) or Integer(ConsoleColorToColorAttribute(SourceForeColor, FALSE)));
    wColorAttribute := color;
    I := SourceTop;

    while ((I < (SourceTop + SourceHeight))) do
    begin
      dwWriteCoord.Y := (I);
      if (not FillConsoleOutputCharacter(ConsoleOutputHandle, SourceChar, SourceWidth, dwWriteCoord, NumberOfCharsWritten)) then
        RaiseLastOSError;
      if (not FillConsoleOutputAttribute(ConsoleOutputHandle, Word(wColorAttribute), SourceWidth, dwWriteCoord, NumberOfCharsWritten)) then
        RaiseLastOSError;
      Inc(I)
    end;

    WriteRegion.Left := TargetLeft;
    WriteRegion.Right := TargetLeft + SourceWidth;
    WriteRegion.Top := TargetTop;
    WriteRegion.Bottom := TargetTop + SourceHeight;

    Win32Check(WriteConsoleOutput(ConsoleOutputHandle, CharInfoArray, dwSize, bufferCoord, WriteRegion));
  end
end;

class function Console.ReadKey: TConsoleKeyInfo;
var
  InputRec: TInputRecord;
  NumRead: Cardinal;
  KeyMode: DWORD;
  StdIn: THandle;
begin
  StdIn := TTextRec(Input).Handle;
  GetConsoleMode(StdIn, KeyMode);
  SetConsoleMode(StdIn, 0);

  repeat
    ReadConsoleInput(StdIn, InputRec, 1, NumRead);
    if (InputRec.EventType and KEY_EVENT <> 0) and InputRec.Event.KeyEvent.bKeyDown then
    begin
      if InputRec.Event.KeyEvent.AsciiChar <> #0 then
      begin
        Result.Key := TConsoleKey(InputRec.Event.KeyEvent.wVirtualKeyCode);
        Result.KeyChar := InputRec.Event.KeyEvent.UnicodeChar;
        Break;
      end;
    end;
  until FALSE;

  SetConsoleMode(StdIn, KeyMode);
end;

class procedure Console.RaiseConsoleError(aCaller: String);
begin
  raise ECOnsoleError.Create('Error calling ' + aCaller +':' + sLineBreak + SysErrorMessage(GetLastError));
end;

class function Console.Read: Integer;
begin
  System.Read(Result);
end;

function IsKeyDownEvent(ir: TInputRecord): Boolean;
begin
  Result := (ir.EventType = KEY_EVENT) and (ir.Event.KeyEvent.bKeyDown);
end;

function IsModKey(ir: TInputRecord): Boolean;
begin
  // We should also skip over Shift, Control, and Alt, as well as caps lock.
  // Apparently we don't need to check for 0xA0 through 0xA5, which are keys like
  // Left Control & Right Control. See the ConsoleKey enum for these values.
  Result := ir.Event.KeyEvent.wVirtualScanCode in [$10 .. $12, $14, $90, $91];
end;

class function Console.ReadKey(Intercept: Boolean): TConsoleKeyInfo;
var
  InputRec: TInputRecord;
  NumRead: Cardinal;
  OldKeyMode: DWORD;
begin
  if StdIn = INVALID_HANDLE_VALUE then
    exit;

  GetConsoleMode(StdIn, OldKeyMode);
  SetConsoleMode(StdIn, 0);

  repeat
    ReadConsoleInput(StdIn, InputRec, 1, NumRead);
  until (InputRec.EventType and KEY_EVENT <> 0) and InputRec.Event.KeyEvent.bKeyDown;

  Result.Key := TConsoleKey(InputRec.Event.KeyEvent.wVirtualKeyCode);
  Result.KeyChar := InputRec.Event.KeyEvent.UnicodeChar;

  SetConsoleMode(StdIn, OldKeyMode);
end;

class function Console.ReadLine: string;
var
  Buffer : Array[0..1024] of Char;
  NumberOfCharsRead : DWORD;
begin
  if StdIn = INVALID_HANDLE_VALUE then
    exit('');

  ZeroMemory(@Buffer, Length(Buffer));
  ReadConsole(StdIn, @Buffer, Length(Buffer),  NumberOfCharsRead , nil);
  Result := PChar(@Buffer);
end;

class procedure Console.ResetColor;
begin
  SetConsoleTextAttribute(StdOut, DefaultTextAttributes);
end;

class procedure Console.ScrollScreenBuffer(Left, Top, Right, Bottom, Distance: Integer);
var
  Rect: TSmallRect;
  Fill: TCharInfo;
  NewPos: TCoord;
begin
  Fill.UnicodeChar := ' ';
  Fill.Attributes := TextAttr;
  if Distance = 0 then
    Distance := Bottom - Top + 1;
  Rect.Left := Left;
  Rect.Right := Right;
  Rect.Top := Top;
  Rect.Bottom := Bottom;
  NewPos.X := Left;
  NewPos.Y := Top + Distance;
  ScrollConsoleScreenBuffer(StdOut, Rect, @Rect, NewPos, Fill);
end;

class procedure Console.SetBackgroundColor(const Value: TConsoleColor);
begin
  TextAttr := (TextAttr and $0F) or ((Word(Value) shl 4) and $F0);
  SetConsoleTextAttribute(StdOut, TextAttr);
end;

class procedure Console.SetBufferHeight(const Value: Integer);
var
  Size: TCoord;
begin
  Size.X := GetBufferWidth;
  Size.Y := Value;
  SetConsoleScreenBufferSize(ConsoleOutputHandle, Size);
end;

class procedure Console.SetBufferSize(Width, Height: Integer);
var
  Size: TCoord;
begin
  if StdOut = INVALID_HANDLE_VALUE then
    exit;

  Size.X := Width;
  Size.Y := Height;
  SetConsoleScreenBufferSize(ConsoleOutputHandle, Size)
end;

class procedure Console.SetBufferSize(const Value: TCoord);
begin
  SetBufferSize(Value.X, Value.Y);
end;

class procedure Console.SetBufferWidth(const Value: Integer);
var
  Size: TCoord;
begin
  Size.X := Value;
  Size.Y := GetBufferHeight;
  SetConsoleScreenBufferSize(ConsoleOutputHandle, Size);
end;

class procedure Console.SetConsoleRect(Rect: TRect);
begin
  SetWindowPos(GetConsoleWindow, 0, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, SWP_SHOWWINDOW);
end;

class procedure Console.SetCursorLeft(const Value: Integer);
begin
  SetCursorPosition(Value, CursorTop)
end;

class procedure Console.SetCursorPosition(Left, Top: SmallInt);
var
  COORD: TCoord;
begin
  COORD.X := Left;
  COORD.Y := Top;
  if not SetConsoleCursorPosition(ConsoleOutputHandle, COORD) then
    RaiseLastOSError;
end;

class procedure Console.SetCursorSize(const Value: Integer);
var
  ConsoleCursorInfo: TConsoleCursorInfo;
begin
  if ((Value < 1) or (Value > 100)) then
    raise ERangeError.Create('value' + IntToStr(Value) + ' out of range for CursorSize');

  if (not GetConsoleCursorInfo(ConsoleOutputHandle, ConsoleCursorInfo)) then
    RaiseLastOSError;

  ConsoleCursorInfo.dwSize := Value;

  if (not SetConsoleCursorInfo(ConsoleOutputHandle, ConsoleCursorInfo)) then
    RaiseLastOSError;
end;

class procedure Console.SetCursorTop(const Value: Integer);
begin
  SetCursorPosition(CursorLeft, Value);
end;

class procedure Console.SetCursorVisible(const Value: Boolean);
var
  CursorInfo: TConsoleCursorInfo;
begin
  if not GetConsoleCursorInfo(ConsoleOutputHandle, CursorInfo) then
    RaiseLastOSError;

  CursorInfo.bVisible := Value;

  if not SetConsoleCursorInfo(ConsoleOutputHandle, CursorInfo) then
    RaiseLastOSError;
end;

class procedure Console.SetConsoleFont(const Value: TCONSOLE_FONT_INFOEX);
begin
  SetCurrentConsoleFontEx(StdOut, FALSE, @Value);
end;

class procedure Console.SetConsoleInputHandle(const Value: Integer);
begin
  TTextRec(Input).Handle := Value;
end;

class procedure Console.SetConsoleOutputHandle(const Value: THandle);
begin
  TTextRec(Output).Handle := Value;
end;

class procedure Console.SetForegroundColor(const Value: TConsoleColor);
begin
  TextAttr := (TextAttr and $F0) or (Word(Value) and $0F);
  SetConsoleTextAttribute(StdOut, TextAttr);
end;

class procedure Console.SetOutputEncoding(const Value: DWORD);
begin
  SetConsoleOutputCP(Value);
end;

class procedure Console.SetTitle(const Value: string);
begin
  SetConsoleTitle(PChar(Value));
end;

class procedure Console.SetTreatControlCAsInput(const Value: Boolean);
begin
  if not SetConsoleMode(StdOut, Cardinal(Value)) then
    RaiseLastOSError;
end;

class procedure Console.SetWindowHeight(const Value: Integer);
begin
  SetWindowSize(WindowWidth, Value);
end;

class procedure Console.SetWindowLeft(const Value: Integer);
begin
  SetWindowPosition(Value, ConsoleRect.Top);
end;

class procedure Console.SetWindowPosition(Left, Top: Integer);
var
  Rect: TRect;
begin
  Rect := ConsoleRect;
  Rect.Top := Top;
  Rect.Left := Left;
  SetConsoleRect(Rect);
end;

class procedure Console.SetWindowSize(Width, Height: Integer);
var
  srWindow: TSmallRect;
  Size: TCoord;
  BufferInfo: TConsoleScreenBufferInfo;
  ResizeBuffer: Boolean;
begin
  if Width <= 0 then
    raise EArgumentOutOfRangeException.Create('Width must be a positive number required');
  if Height <= 0 then
    raise EArgumentOutOfRangeException.Create('Height must be a positive number required');

  BufferInfo := GetBufferInfo;
  ResizeBuffer := FALSE;

  Size.X := BufferInfo.dwSize.X;
  Size.Y := BufferInfo.dwSize.Y;

  if BufferInfo.dwSize.X < BufferInfo.srWindow.Left + Width then
  begin
    if BufferInfo.srWindow.Left >= Int16_MaxValue - Width then
      raise EArgumentOutOfRangeException.Create('Width must be a positive number required and les than ' + IntToStr(Int16_MaxValue));

    Size.X := Short(BufferInfo.srWindow.Left + Width);
    ResizeBuffer := True;
  end;

  if BufferInfo.dwSize.Y < BufferInfo.srWindow.Top + Height then
  begin
    if BufferInfo.srWindow.Top >= Int16_MaxValue - Height then
      raise EArgumentOutOfRangeException.Create('Height must be a positive number required and lesthan ' + IntToStr(Int16_MaxValue));
    Size.Y := Short(BufferInfo.srWindow.Top + Height);
    ResizeBuffer := True;
  end;

  if ResizeBuffer then
    if not SetConsoleScreenBufferSize(ConsoleOutputHandle, Size) then
      RaiseLastOSError;

  srWindow := BufferInfo.srWindow;
  // Preserve the position, but change the size.
  srWindow.Bottom := Short(srWindow.Top + Height - 1);
  srWindow.Right := Short(srWindow.Left + Width - 1);

  if not SetConsoleWindowInfo(ConsoleOutputHandle, True, srWindow) then
  begin
    // Try to give a better error message here
    Size := GetLargestConsoleWindowSize(ConsoleOutputHandle);
    if Width > Size.X then
      raise EArgumentOutOfRangeException.Create(IntToStr(Width) + 'Is out of range for Console width');
    if Height > Size.Y then
      raise EArgumentOutOfRangeException.Create(IntToStr(Height) + 'Is out of range for Console height');

    RaiseLastOSError;
  end;
end;

class procedure Console.SetWindowTop(const Value: Integer);
begin
  SetWindowPosition(ConsoleRect.Left, Value);
end;

class procedure Console.SetWindowWidth(const Value: Integer);
begin
  SetWindowSize(Value, WindowHeight);
end;

class procedure Console.UpdateConsoleFont(aFontName: string; aFontSize: Cardinal; aFontFamily: TFontFamily; aFontWeight: TFontWeight);
var
  CONSOLE_FONT_INFOEX: TCONSOLE_FONT_INFOEX;
  Destination: PChar;
begin
  CONSOLE_FONT_INFOEX := ConsoleFont;
  CONSOLE_FONT_INFOEX.FontFamily := Cardinal(aFontFamily);
  if aFontName <> '' then
  begin
    Destination := @CONSOLE_FONT_INFOEX.FaceName[0];
    StrLCopy(Destination, PChar(aFontName), LF_FACESIZE - 1);
  end;
  CONSOLE_FONT_INFOEX.dwFontSize.X := 0;

  if aFontSize <> 0 then
    CONSOLE_FONT_INFOEX.dwFontSize.Y := aFontSize;

  CONSOLE_FONT_INFOEX.FontWeight := Cardinal(aFontWeight);

  if not SetCurrentConsoleFontEx(StdOut, FALSE, @CONSOLE_FONT_INFOEX) then
    RaiseLastOSError;
end;

class procedure Console.WriteLine<T>(aValue: T);
begin
  WriteString(GenericToString(aValue) + sLineBreak);
end;

class function Console.WriteString(aValue: string): Cardinal;
begin
  if StdOut = INVALID_HANDLE_VALUE then
    AllocateConsole;

  if StdOut <> INVALID_HANDLE_VALUE then
    WriteConsole(StdOut, PWideChar(aValue), Length(aValue), Result, nil);
end;

class procedure Console.Write<T>(aValue: T);
begin
  WriteString(GenericToString(aValue));
end;

class procedure Console.Write(Value: Variant; Args: array of const);
var
  S: string;
  I: Integer;
begin
  S := Value;
  for I := 0 to high(Args) do
    S := ReplaceStr(S, '{' + IntToStr(I) + '}', '%' + IntToStr(I) + ':s');

  WriteString(Format(S, Args));
end;

class procedure Console.WriteLine(FormatString: String; Args: array of Variant);
var
  I: Integer;
  VarRecArray: array of TVarRec;
begin
  for I := 0 to high(Args) do
    FormatString := ReplaceStr(FormatString, '{' + IntToStr(I) + '}', '%' + IntToStr(I) + ':s');

  SetLength(VarRecArray, Length(Args));

  for I := 0 to high(Args) do
  begin
    VarRecArray[I].VType := vtUnicodeString;
    // nil out first, so no attempt to decrement reference count.
    VarRecArray[I].VUnicodeString := nil;
    UnicodeString(VarRecArray[I].VUnicodeString) := UnicodeString(Args[I]);
  end;

  WriteString(Format(FormatString, VarRecArray) + sLineBreak);
end;

class procedure Console.WriteLine;
begin
  WriteString(sLineBreak);
end;

initialization

LockObject := TObject.Create;

finalization

LockObject.Free;

end.



