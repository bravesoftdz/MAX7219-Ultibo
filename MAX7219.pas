unit MAX7219;

{$mode objfpc}{$H+}

interface

uses
  GlobalConfig,
  GlobalConst,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo;

// The Max7219 Registers:
const
  DECODE_MODE = $09;
  INTENSITY = $0a;
  SCAN_LIMIT = $0b;
  SHUTDOWN = $0c;
  DISPLAY_TEST = $0f;

  DATA_PIN = GPIO_PIN_17;
  LOAD_PIN = GPIO_PIN_23;
  CLOCK_PIN = GPIO_PIN_22;

type
  TArrayDisplay = array of byte;

  { TMAX7219 }

  TMAX7219 = class
  private
    FDisplays, FDataPin, FLoadPin, FClockPin: integer;
    FCharSet: array[0..255] of word;
    FSpacing: byte;
    procedure Init;
    procedure StartSend();
    procedure Send16bits(output: word);
    procedure DoSend(reg_number, dataout: word);
    procedure EndSend();
    procedure BuildChars;
  public
    constructor Create(); overload;
    constructor Create(Displays: integer); overload;
    constructor Create(Displays, DataPin, LoadPin, ClockPin: integer); overload;
    procedure Send(reg_number, dataout: word); overload;
    procedure Send(num_display, reg_number, dataout: word); overload;
    procedure PrintChar(num_display: word; Chr, Size: byte);
    function GetCharPos(Chr: char): integer; overload;
    function GetCharPos(Chr: byte): integer; overload;
    function GenString(Text: string): TArrayDisplay;
    property Spacing: byte read FSpacing write FSpacing default 1;
  end;

const
  sysfont: array[0..1455] of byte = (
    0,                                     // 0 - 'Empty Cell'
    5, $3e, $5b, $4f, $5b, $3e,            // 1 - 'Sad Smiley'
    5, $3e, $6b, $4f, $6b, $3e,            // 2 - 'Happy Smiley'
    5, $1c, $3e, $7c, $3e, $1c,            // 3 - 'Heart'
    5, $18, $3c, $7e, $3c, $18,            // 4 - 'Diamond'
    5, $1c, $57, $7d, $57, $1c,            // 5 - 'Clubs'
    5, $1c, $5e, $7f, $5e, $1c,            // 6 - 'Spades'
    4, $00, $18, $3c, $18,                 // 7 - 'Bullet Point'
    5, $ff, $e7, $c3, $e7, $ff,            // 8 - 'Rev Bullet Point'
    4, $00, $18, $24, $18,                 // 9 - 'Hollow Bullet Point'
    5, $ff, $e7, $db, $e7, $ff,            // 10 - 'Rev Hollow BP'
    5, $30, $48, $3a, $06, $0e,            // 11 - 'Male'
    5, $26, $29, $79, $29, $26,            // 12 - 'Female'
    5, $40, $7f, $05, $05, $07,            // 13 - 'Music Note 1'
    5, $40, $7f, $05, $25, $3f,            // 14 - 'Music Note 2'
    5, $5a, $3c, $e7, $3c, $5a,            // 15 - 'Snowflake'
    5, $7f, $3e, $1c, $1c, $08,            // 16 - 'Right Pointer'
    5, $08, $1c, $1c, $3e, $7f,            // 17 - 'Left Pointer'
    5, $14, $22, $7f, $22, $14,            // 18 - 'UpDown Arrows'
    5, $5f, $5f, $00, $5f, $5f,            // 19 - 'Double Exclamation'
    5, $06, $09, $7f, $01, $7f,            // 20 - 'Paragraph Mark'
    4, $66, $89, $95, $6a,                 // 21 - 'Section Mark'
    5, $60, $60, $60, $60, $60,            // 22 - 'Double Underline'
    5, $94, $a2, $ff, $a2, $94,            // 23 - 'UpDown Underlined'
    5, $08, $04, $7e, $04, $08,            // 24 - 'Up Arrow'
    5, $10, $20, $7e, $20, $10,            // 25 - 'Down Arrow'
    5, $08, $08, $2a, $1c, $08,            // 26 - 'Right Arrow'
    5, $08, $1c, $2a, $08, $08,            // 27 - 'Left Arrow'
    5, $1e, $10, $10, $10, $10,            // 28 - 'Angled'
    5, $0c, $1e, $0c, $1e, $0c,            // 29 - 'Squashed #'
    5, $30, $38, $3e, $38, $30,            // 30 - 'Up Pointer'
    5, $06, $0e, $3e, $0e, $06,            // 31 - 'Down Pointer'
    2, $00, $00,                           // 32 - 'Space'
    1, $5f,                                // 33 - '!'
    3, $07, $00, $07,                      // 34 - '"'
    5, $14, $7f, $14, $7f, $14,            // 35 - '#'
    5, $24, $2a, $7f, $2a, $12,            // 36 - '$'
    5, $23, $13, $08, $64, $62,            // 37 - '%'
    5, $36, $49, $56, $20, $50,            // 38 - '&'
    3, $08, $07, $03,                      // 39 - '''
    3, $1c, $22, $41,                      // 40 - '('
    3, $41, $22, $1c,                      // 41 - ')'
    5, $2a, $1c, $7f, $1c, $2a,            // 42 - '*'
    5, $08, $08, $3e, $08, $08,            // 43 - '+'
    3, $80, $70, $30,                      // 44 - ','
    5, $08, $08, $08, $08, $08,            // 45 - '-'
    2, $60, $60,                           // 46 - '.'
    5, $20, $10, $08, $04, $02,            // 47 - '/'
    5, $3e, $51, $49, $45, $3e,            // 48 - '0'
    3, $42, $7f, $40,                      // 49 - '1'
    5, $72, $49, $49, $49, $46,            // 50 - '2'
    5, $21, $41, $49, $4d, $33,            // 51 - '3'
    5, $18, $14, $12, $7f, $10,            // 52 - '4'
    5, $27, $45, $45, $45, $39,            // 53 - '5'
    5, $3c, $4a, $49, $49, $31,            // 54 - '6'
    5, $41, $21, $11, $09, $07,            // 55 - '7'
    5, $36, $49, $49, $49, $36,            // 56 - '8'
    5, $46, $49, $49, $29, $1e,            // 57 - '9'
    1, $14,                                // 58 - ':'
    2, $80, $68,                           // 59 - ';'
    4, $08, $14, $22, $41,                 // 60 - '<'
    5, $14, $14, $14, $14, $14,            // 61 - '='
    4, $41, $22, $14, $08,                 // 62 - '>'
    5, $02, $01, $59, $09, $06,            // 63 - '?'
    5, $3e, $41, $5d, $59, $4e,            // 64 - '@'
    5, $7c, $12, $11, $12, $7c,            // 65 - 'A'
    5, $7f, $49, $49, $49, $36,            // 66 - 'B'
    5, $3e, $41, $41, $41, $22,            // 67 - 'C'
    5, $7f, $41, $41, $41, $3e,            // 68 - 'D'
    5, $7f, $49, $49, $49, $41,            // 69 - 'E'
    5, $7f, $09, $09, $09, $01,            // 70 - 'F'
    5, $3e, $41, $41, $51, $73,            // 71 - 'G'
    5, $7f, $08, $08, $08, $7f,            // 72 - 'H'
    3, $41, $7f, $41,                      // 73 - 'I'
    5, $20, $40, $41, $3f, $01,            // 74 - 'J'
    5, $7f, $08, $14, $22, $41,            // 75 - 'K'
    5, $7f, $40, $40, $40, $40,            // 76 - 'L'
    5, $7f, $02, $1c, $02, $7f,            // 77 - 'M'
    5, $7f, $04, $08, $10, $7f,            // 78 - 'N'
    5, $3e, $41, $41, $41, $3e,            // 79 - 'O'
    5, $7f, $09, $09, $09, $06,            // 80 - 'P'
    5, $3e, $41, $51, $21, $5e,            // 81 - 'Q'
    5, $7f, $09, $19, $29, $46,            // 82 - 'R'
    5, $26, $49, $49, $49, $32,            // 83 - 'S'
    5, $03, $01, $7f, $01, $03,            // 84 - 'T'
    5, $3f, $40, $40, $40, $3f,            // 85 - 'U'
    5, $1f, $20, $40, $20, $1f,            // 86 - 'V'
    5, $3f, $40, $38, $40, $3f,            // 87 - 'W'
    5, $63, $14, $08, $14, $63,            // 88 - 'X'
    5, $03, $04, $78, $04, $03,            // 89 - 'Y'
    5, $61, $59, $49, $4d, $43,            // 90 - 'Z'
    3, $7f, $41, $41,                      // 91 - '['
    5, $02, $04, $08, $10, $20,            // 92 - '\'
    3, $41, $41, $7f,                      // 93 - ']'
    5, $04, $02, $01, $02, $04,            // 94 - '^'
    5, $40, $40, $40, $40, $40,            // 95 - '_'
    3, $03, $07, $08,                      // 96 - '`'
    5, $20, $54, $54, $78, $40,            // 97 - 'a'
    5, $7f, $28, $44, $44, $38,            // 98 - 'b'
    5, $38, $44, $44, $44, $28,            // 99 - 'c'
    5, $38, $44, $44, $28, $7f,            // 100 - 'd'
    5, $38, $54, $54, $54, $18,            // 101 - 'e'
    4, $08, $7e, $09, $02,                 // 102 - 'f'
    5, $18, $a4, $a4, $9c, $78,            // 103 - 'g'
    5, $7f, $08, $04, $04, $78,            // 104 - 'h'
    3, $44, $7d, $40,                      // 105 - 'i'
    4, $40, $80, $80, $7a,                 // 106 - 'j'
    4, $7f, $10, $28, $44,                 // 107 - 'k'
    3, $41, $7f, $40,                      // 108 - 'l'
    5, $7c, $04, $78, $04, $78,            // 109 - 'm'
    5, $7c, $08, $04, $04, $78,            // 110 - 'n'
    5, $38, $44, $44, $44, $38,            // 111 - 'o'
    5, $fc, $18, $24, $24, $18,            // 112 - 'p'
    5, $18, $24, $24, $18, $fc,            // 113 - 'q'
    5, $7c, $08, $04, $04, $08,            // 114 - 'r'
    5, $48, $54, $54, $54, $24,            // 115 - 's'
    4, $04, $3f, $44, $24,                 // 116 - 't'
    5, $3c, $40, $40, $20, $7c,            // 117 - 'u'
    5, $1c, $20, $40, $20, $1c,            // 118 - 'v'
    5, $3c, $40, $30, $40, $3c,            // 119 - 'w'
    5, $44, $28, $10, $28, $44,            // 120 - 'x'
    5, $4c, $90, $90, $90, $7c,            // 121 - 'y'
    5, $44, $64, $54, $4c, $44,            // 122 - 'z'
    3, $08, $36, $41,                      // 123 - '{'
    1, $77,                                // 124 - '|'
    3, $41, $36, $08,                      // 125 - '}'
    5, $02, $01, $02, $04, $02,            // 126 - '~'
    5, $3c, $26, $23, $26, $3c,            // 127 - 'Hollow Up Arrow'
    5, $1e, $a1, $a1, $61, $12,            // 128 - 'C sedilla'
    5, $38, $42, $40, $22, $78,            // 129 - 'u umlaut'
    5, $38, $54, $54, $55, $59,            // 130 - 'e acute'
    5, $21, $55, $55, $79, $41,            // 131 - 'a accent'
    5, $21, $54, $54, $78, $41,            // 132 - 'a umlaut'
    5, $21, $55, $54, $78, $40,            // 133 - 'a grave'
    5, $20, $54, $55, $79, $40,            // 134 - 'a acute'
    5, $18, $3c, $a4, $e4, $24,            // 135 - 'c sedilla'
    5, $39, $55, $55, $55, $59,            // 136 - 'e accent'
    5, $38, $55, $54, $55, $58,            // 137 - 'e umlaut'
    5, $39, $55, $54, $54, $58,            // 138 - 'e grave'
    3, $45, $7c, $41,                      // 139 - 'i umlaut'
    4, $02, $45, $7d, $42,                 // 140 - 'i hat'
    4, $01, $45, $7c, $40,                 // 141 - 'i grave'
    5, $f0, $29, $24, $29, $f0,            // 142 - 'A umlaut'
    5, $f0, $28, $25, $28, $f0,            // 143 - 'A dot'
    4, $7c, $54, $55, $45,                 // 144 - 'E grave'
    7, $20, $54, $54, $7c, $54, $54, $08,  // 145 - 'ae'
    6, $7c, $0a, $09, $7f, $49, $49,       // 146 - 'AE'
    5, $32, $49, $49, $49, $32,            // 147 - 'o hat'
    5, $30, $4a, $48, $4a, $30,            // 148 - 'o umlaut'
    5, $32, $4a, $48, $48, $30,            // 149 - 'o grave'
    5, $3a, $41, $41, $21, $7a,            // 150 - 'u hat'
    5, $3a, $42, $40, $20, $78,            // 151 - 'u grave'
    4, $9d, $a0, $a0, $7d,                 // 152 - 'y umlaut'
    5, $38, $45, $44, $45, $38,            // 153 - 'O umlaut'
    5, $3c, $41, $40, $41, $3c,            // 154 - 'U umlaut'
    5, $3c, $24, $ff, $24, $24,            // 155 - 'Cents'
    5, $48, $7e, $49, $43, $66,            // 156 - 'Pounds'
    5, $2b, $2f, $fc, $2f, $2b,            // 157 - 'Yen'
    5, $ff, $09, $29, $f6, $20,            // 158 - 'R +'
    5, $c0, $88, $7e, $09, $03,            // 159 - 'f notation'
    5, $20, $54, $54, $79, $41,            // 160 - 'a acute'
    3, $44, $7d, $41,                      // 161 - 'i acute'
    5, $30, $48, $48, $4a, $32,            // 162 - 'o acute'
    5, $38, $40, $40, $22, $7a,            // 163 - 'u acute'
    4, $7a, $0a, $0a, $72,                 // 164 - 'n accent'
    5, $7d, $0d, $19, $31, $7d,            // 165 - 'N accent'
    5, $26, $29, $29, $2f, $28,            // 166
    5, $26, $29, $29, $29, $26,            // 167
    5, $30, $48, $4d, $40, $20,            // 168 - 'Inverted ?'
    5, $38, $08, $08, $08, $08,            // 169 - 'LH top corner'
    5, $08, $08, $08, $08, $38,            // 170 - 'RH top corner'
    5, $2f, $10, $c8, $ac, $ba,            // 171 - '1/2'
    5, $2f, $10, $28, $34, $fa,            // 172 - '1/4'
    1, $7b,                                // 173 - '| split'
    5, $08, $14, $2a, $14, $22,            // 174 - '<<'
    5, $22, $14, $2a, $14, $08,            // 175 - '>>'
    5, $aa, $00, $55, $00, $aa,            // 176 - '30% shading'
    5, $aa, $55, $aa, $55, $aa,            // 177 - '50% shading'
    5, $00, $00, $00, $00, $ff,            // 178 - 'Right side'
    5, $10, $10, $10, $10, $ff,            // 179 - 'Right T'
    5, $14, $14, $14, $14, $ff,            // 180 - 'Right T double H'
    5, $10, $10, $ff, $00, $ff,            // 181 - 'Right T double V'
    5, $10, $10, $f0, $10, $f0,            // 182 - 'Top Right double V'
    5, $14, $14, $14, $14, $fc,            // 183 - 'Top Right double H'
    5, $14, $14, $f7, $00, $ff,            // 184 - 'Right T double all'
    5, $00, $00, $ff, $00, $ff,            // 185 - 'Right side double'
    5, $14, $14, $f4, $04, $fc,            // 186 - 'Top Right double'
    5, $14, $14, $17, $10, $1f,            // 187 - 'Bot Right double'
    5, $10, $10, $1f, $10, $1f,            // 188 - 'Bot Right double V'
    5, $14, $14, $14, $14, $1f,            // 189 - 'Bot Right double H'
    5, $10, $10, $10, $10, $f0,            // 190 - 'Top Right'
    5, $00, $00, $00, $1f, $10,            // 191 - 'Bot Left'
    5, $10, $10, $10, $1f, $10,            // 192 - 'Bot T'
    5, $10, $10, $10, $f0, $10,            // 193 - 'Top T'
    5, $00, $00, $00, $ff, $10,            // 194 - 'Left T'
    5, $10, $10, $10, $10, $10,            // 195 - 'Top side'
    5, $10, $10, $10, $ff, $10,            // 196 - 'Center +'
    5, $00, $00, $00, $ff, $14,            // 197 - 'Left side double H'
    5, $00, $00, $ff, $00, $ff,            // 198 - 'Left side double'
    5, $00, $00, $1f, $10, $17,            // 199 - 'Bot Left double V'
    5, $00, $00, $fc, $04, $f4,            // 200 - 'Top Left double V'
    5, $14, $14, $17, $10, $17,            // 201 - 'Bot T double'
    5, $14, $14, $f4, $04, $f4,            // 202 - 'Top T double'
    5, $00, $00, $ff, $00, $f7,            // 203 - 'Left Side double spl'
    5, $14, $14, $14, $14, $14,            // 204 - 'Center double'
    5, $14, $14, $f7, $00, $f7,            // 205 - 'Center + double'
    5, $14, $14, $14, $17, $14,            // 206 - 'Bot T double H'
    5, $10, $10, $1f, $10, $1f,            // 207 - 'Bot Right double V'
    5, $14, $14, $14, $f4, $14,            // 208 - 'Top T double H'
    5, $10, $10, $f0, $10, $f0,            // 209 - 'Top Right double V'
    5, $00, $00, $1f, $10, $1f,            // 210 - 'Bot Left double V'
    5, $00, $00, $00, $1f, $14,            // 211 - 'Bot Right double H'
    5, $00, $00, $00, $fc, $14,            // 212 - 'Top Right double H'
    5, $00, $00, $f0, $10, $f0,            // 213 - 'Top Right double V'
    5, $10, $10, $ff, $10, $ff,            // 214 - 'Center + double V'
    5, $14, $14, $14, $ff, $14,            // 215 - 'Center + double H'
    5, $10, $10, $10, $10, $1f,            // 216 - 'Bot Right'
    5, $00, $00, $00, $f0, $10,            // 217 - 'Top Left'
    5, $ff, $ff, $ff, $ff, $ff,            // 218 - 'Full Block'
    5, $f0, $f0, $f0, $f0, $f0,            // 219 - 'Half Block Bottom'
    3, $ff, $ff, $ff,                      // 220 - 'Half Block LHS'
    5, $00, $00, $00, $ff, $ff,            // 221 - 'Half Block RHS'
    5, $0f, $0f, $0f, $0f, $0f,            // 222 - 'Half Block Top'
    5, $38, $44, $44, $38, $44,            // 223 - 'Alpha'
    5, $7c, $2a, $2a, $3e, $14,            // 224 - 'Beta'
    5, $7e, $02, $02, $06, $06,            // 225 - 'Gamma'
    5, $02, $7e, $02, $7e, $02,            // 226 - 'Pi'
    5, $63, $55, $49, $41, $63,            // 227 - 'Sigma'
    5, $38, $44, $44, $3c, $04,            // 228 - 'Theta'
    5, $40, $7e, $20, $1e, $20,            // 229 - 'mu'
    5, $06, $02, $7e, $02, $02,            // 230 - 'Tau'
    5, $99, $a5, $e7, $a5, $99,            // 231
    5, $1c, $2a, $49, $2a, $1c,            // 232
    5, $4c, $72, $01, $72, $4c,            // 233
    5, $30, $4a, $4d, $4d, $30,            // 234
    5, $30, $48, $78, $48, $30,            // 235
    5, $bc, $62, $5a, $46, $3d,            // 236 - 'Zero Slashed'
    4, $3e, $49, $49, $49,                 // 237
    5, $7e, $01, $01, $01, $7e,            // 238
    5, $2a, $2a, $2a, $2a, $2a,            // 239 - '3 Bar Equals'
    5, $44, $44, $5f, $44, $44,            // 240 - '+/-'
    5, $40, $51, $4a, $44, $40,            // 241 - '>='
    5, $40, $44, $4a, $51, $40,            // 242 - '<='
    5, $00, $00, $ff, $01, $03,            // 243 - 'Top of Integral'
    3, $e0, $80, $ff,                      // 244 - 'Bot of Integral'
    5, $08, $08, $6b, $6b, $08,            // 245 - 'Divide'
    5, $36, $12, $36, $24, $36,            // 246 - 'Wavy ='
    5, $06, $0f, $09, $0f, $06,            // 247 - 'Degree'
    4, $00, $00, $18, $18,                 // 248 - 'Math Product'
    4, $00, $00, $10, $10,                 // 249 - 'Short Dash'
    5, $30, $40, $ff, $01, $01,            // 250 - 'Square Root'
    5, $00, $1f, $01, $01, $1e,            // 251 - 'Superscript n'
    5, $00, $19, $1d, $17, $12,            // 252 - 'Superscript 2'
    5, $00, $3c, $3c, $3c, $3c,            // 253 - 'Centered Square'
    5, $ff, $81, $81, $81, $ff,            // 254 - 'Full Frame'
    5, $ff, $ff, $ff, $ff, $ff             // 255 - 'Full Block'
    );

implementation

constructor TMAX7219.Create(Displays, DataPin, LoadPin, ClockPin: integer);
begin
  inherited Create;

  FDisplays := Displays;
  FDataPin := DataPin;
  FLoadPin := LoadPin;
  FClockPin := ClockPin;

  Init;
end;

procedure TMAX7219.Send16bits(output: word);
var
  i, mask: word;
begin
  for i := 15 downto 0 do
  begin
    mask := 1 shl i; // calculate bitmask
    GPIOOutputSet(FClockPin, GPIO_LEVEL_LOW);  // set clock to 0

    // Send one bit on the data pin
    if (output and mask) <> 0 then
      GPIOOutputSet(FDataPin, GPIO_LEVEL_HIGH)
    else
      GPIOOutputSet(FDataPin, GPIO_LEVEL_LOW);

    GPIOOutputSet(FClockPin, GPIO_LEVEL_HIGH);  // set clock to 1
  end;
end;

procedure TMAX7219.BuildChars;
var
  i, n: integer;
begin
  FCharSet[0] := 0;
  i := 1;

  for n := 1 to 255 do
  begin
    FCharSet[n] := i;
    Inc(i, sysfont[i] + 1);
  end;
end;

constructor TMAX7219.Create;
begin
  inherited Create;

  FDisplays := 1;
  FDataPin := DATA_PIN;
  FLoadPin := LOAD_PIN;
  FClockPin := CLOCK_PIN;

  Init;
end;

constructor TMAX7219.Create(Displays: integer);
begin
  inherited Create;

  FDisplays := Displays;
  FDataPin := DATA_PIN;
  FLoadPin := LOAD_PIN;
  FClockPin := CLOCK_PIN;

  Init;
end;

procedure TMAX7219.Init;
begin
  BuildChars;

  {Now set GPIO pin 17 to Pull None}
  GPIOPullSelect(FDataPin, GPIO_PULL_NONE);
  {And make GPIO pin 17 an Output}
  GPIOFunctionSelect(FDataPin, GPIO_FUNCTION_OUT);
  {Finally set the value of GPIO pin 17 to Low}
  GPIOOutputSet(FDataPin, GPIO_LEVEL_LOW);

  {Now set GPIO pin 23 to Pull None}
  GPIOPullSelect(FClockPin, GPIO_PULL_NONE);
  {And make GPIO pin 23 an Output}
  GPIOFunctionSelect(FClockPin, GPIO_FUNCTION_OUT);
  {Finally set the value of GPIO pin 23 to Low}
  GPIOOutputSet(FClockPin, GPIO_LEVEL_HIGH);

  {Now set GPIO pin 23 to Pull None}
  GPIOPullSelect(FLoadPin, GPIO_PULL_NONE);
  {And make GPIO pin 23 an Output}
  GPIOFunctionSelect(FLoadPin, GPIO_FUNCTION_OUT);
  {Finally set the value of GPIO pin 23 to Low}
  GPIOOutputSet(FLoadPin, GPIO_LEVEL_HIGH);
end;

procedure TMAX7219.StartSend();
begin
  GPIOOutputSet(FLoadPin, GPIO_LEVEL_HIGH); // set LOAD 1 to start
end;

// Take a reg numer and data and send to the max7219
procedure TMAX7219.DoSend(reg_number, dataout: word);
begin
  Send16bits((reg_number shl 8) + dataout); // send 16 bits ( reg number + dataout )
end;

procedure TMAX7219.EndSend();
begin
  GPIOOutputSet(FLoadPin, GPIO_LEVEL_LOW);  // LOAD 0 to latch
end;

procedure TMAX7219.Send(reg_number, dataout: word);
var
  i: integer;
begin
  StartSend();

  for i := 1 to FDisplays do
    Send(i, reg_number, dataout); // send 16 bits ( reg number + dataout )

  EndSend();
end;

// Take a reg numer and data and send to the max7219
procedure TMAX7219.Send(num_display, reg_number, dataout: word); overload;
var
  i: integer;
begin
  StartSend();

  for i := FDisplays downto 1 do
    if i = num_display then
      DoSend(reg_number, dataout) // send 16 bits ( reg number + dataout )
    else
      DoSend(0, 0);

  EndSend();
end;

procedure TMAX7219.PrintChar(num_display: word; Chr, Size: byte);
var
  i, n, c: integer;
begin
  i := FCharSet[Chr]; // get char pos
  n := sysfont[i];  // get char length

  if n > 8 then
    n := 8;

  for c := 1 to n do
  begin
    Inc(i);
    Send(num_display, c, sysfont[i]);
  end;

  if n < Size then
  begin
    Send(num_display, n + 1, 0);

    for c := n + 2 to Size do
      Send(num_display, c, sysfont[0]);
  end;
end;

function TMAX7219.GetCharPos(Chr: char): integer;
begin
  Result := GetCharPos(Ord(Chr));
end;

function TMAX7219.GetCharPos(Chr: byte): integer;
begin
  Result := FCharSet[Chr];
end;

function TMAX7219.GenString(Text: string): TArrayDisplay;
var
  i, n, c, x: integer;
begin
  SetLength(Result, 0);

  if Text = '' then
    Exit;

  n := FDisplays * 8 - 1;
  x := n;

  for i := 1 to Length(Text) do
  begin
    c := FCharSet[Ord(Text[i])]; // get char pos
    Inc(n, sysfont[c]);  // get char length
  end;

  SetLength(Result, n);

  for i := 1 to Length(Text) do
  begin
    c := FCharSet[Ord(Text[i])]; // get char pos
    n := sysfont[c];  // get char length

    for c := 1 to n do
    begin
      Result[x] := sysfont[n];
      Inc(x);
      Inc(n);
    end;
  end;
end;

end.

