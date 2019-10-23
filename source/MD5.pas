{*******************************************************}
{       MD5 加密单元 (基于IdHttp中的MD5修改)            }
{       author: YangYxd  2014.11.10                     }
{                                                       }
{*******************************************************}
{
 ver 1.0.0.1  by YangYxd 2015.07.23
 ----------------------------------------------------------------------------
  - 基于IdHttp中的MD5算法修改
}
unit MD5;

interface

uses
  Classes, SysUtils;

type
  T16x4LongWordRecord = array[0..15] of Cardinal;
  T4x4LongWordRecord = array [0..3] of Cardinal;
  T4x4x4LongWordRecord = array[0..3] of T4x4LongWordRecord;
  P4x4LongWordRecord = ^T4x4LongWordRecord;

  T512BitRecord = array [0..63] of byte;
  T384BitRecord = array [0..47] of byte;
  T128BitRecord = array [0..15] of byte;

function MD5Bin(const S: string): TBytes; overload;
function MD5Bin(Stream: TStream): TBytes; overload;
function MD5Bin(const P: Pointer; Len: Integer): TBytes; overload;
function MD5BinHex(const P: Pointer; Len: Integer): string;
function MD5Str(const S: string): RawByteString; overload;
function MD5Str(Stream: TStream): RawByteString; overload;
function MD5Hex(const S: string): string; overload;
function MD5Hex(Stream: TStream): string; overload;

function ToHex(const AValue: T4x4LongWordRecord; ALowerCase: Boolean = False): string;
function ToHexBytes(const AValue: TBytes; ALowerCase: Boolean = False): string;

implementation

type
{$IFDEF UNICODE}
  StringW = UnicodeString;
{$ELSE}
  StringW = WideString;
{$ENDIF}
  CharW = WideChar;
  PCharW = PWideChar;

const
  MD2_PI_SUBST : array [0..255] of byte = (
     41,  46,  67, 201, 162, 216, 124,   1,  61,  54,  84, 161, 236, 240,
      6,  19,  98, 167,   5, 243, 192, 199, 115, 140, 152, 147,  43, 217,
    188,  76, 130, 202,  30, 155,  87,  60, 253, 212, 224,  22, 103,  66,
    111,  24, 138,  23, 229,  18, 190,  78, 196, 214, 218, 158, 222,  73,
    160, 251, 245, 142, 187,  47, 238, 122, 169, 104, 121, 145,  21, 178,
      7,  63, 148, 194,  16, 137,  11,  34,  95,  33, 128, 127,  93, 154,
     90, 144,  50,  39,  53,  62, 204, 231, 191, 247, 151,   3, 255,  25,
     48, 179, 72, 165,  181, 209, 215,  94, 146,  42, 172,  86, 170, 198,
     79, 184,  56, 210, 150, 164, 125, 182, 118, 252, 107, 226, 156, 116,
      4, 241,  69, 157, 112,  89, 100, 113, 135,  32, 134,  91, 207, 101,
    230,  45, 168,   2,  27,  96,  37, 173, 174, 176, 185, 246,  28,  70,
     97, 105,  52,  64, 126, 15,   85,  71, 163,  35, 221,  81, 175,  58,
    195,  92, 249, 206, 186, 197, 234,  38,  44,  83,  13, 110, 133,  40,
    132,   9, 211, 223, 205, 244, 65,  129,  77,  82, 106, 220,  55, 200,
    108, 193, 171, 250,  36, 225, 123,   8,  12, 189, 177,  74, 120, 136,
    149, 139, 227,  99, 232, 109, 233, 203, 213, 254,  59,   0,  29,  57,
    242, 239, 183,  14, 102,  88, 208, 228, 166, 119, 114, 248, 235, 117,
     75,  10,  49,  68,  80, 180, 143, 237,  31,  26, 219, 153, 141,  51,
     159,  17, 131, 20);

type
  TPointerStream = class(TCustomMemoryStream)
  public
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

function TPointerStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

var
  SCheckSum: T128BitRecord;
  SX: T384BitRecord;
  SCBuffer: T128BitRecord;

function BinToHex(p: Pointer; l: Integer; ALowerCase: Boolean): StringW;
const
  B2HConvert: array [0 .. 15] of CharW = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  B2HConvertL: array [0 .. 15] of CharW = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  pd: PCharW;
  pb: PByte;
begin
  SetLength(Result, l shl 1);
  pd := PCharW(Result);
  pb := p;
  if ALowerCase then
  begin
    while l > 0 do begin
      pd^ := B2HConvertL[pb^ shr 4];
      Inc(pd);
      pd^ := B2HConvertL[pb^ and $0F];
      Inc(pd);
      Inc(pb);
      Dec(l);
    end;
  end else begin
    while l > 0 do begin
      pd^ := B2HConvert[pb^ shr 4];
      Inc(pd);
      pd^ := B2HConvert[pb^ and $0F];
      Inc(pd);
      Inc(pb);
      Dec(l);
    end;
  end;
end;

function ToHexBytes(const AValue: TBytes; ALowerCase: Boolean): string;
begin
  Result := BinToHex(Pointer(AValue), Length(AValue), ALowerCase);
end;

function ToHex(const AValue: T4x4LongWordRecord; ALowerCase: Boolean): string;
begin
  Result := BinToHex(@AValue, SizeOf(AValue), ALowerCase);
end;

// Clear Buffer and Checksum arrays
procedure Reset(var CheckSum: T128BitRecord; var X: T384BitRecord;
  var CBuffer: T128BitRecord);
var
  I: Integer;
begin
  for I := 0 to 15 do begin
    CheckSum[I] := 0;
    CBuffer[I] := 0;
    X[I] := 0;
    X[I+16] := 0;
    X[I+32] := 0;
  end;
end;

const
  MD4_INIT_VALUES: T4x4LongWordRecord = (
    $67452301, $EFCDAB89, $98BADCFE, $10325476);
const
  MD5_SINE : array [1..64] of Cardinal = (
   { Round 1. }
   $d76aa478, $e8c7b756, $242070db, $c1bdceee, $f57c0faf, $4787c62a,
   $a8304613, $fd469501, $698098d8, $8b44f7af, $ffff5bb1, $895cd7be,
   $6b901122, $fd987193, $a679438e, $49b40821,
   { Round 2. }
   $f61e2562, $c040b340, $265e5a51, $e9b6c7aa, $d62f105d, $02441453,
   $d8a1e681, $e7d3fbc8, $21e1cde6, $c33707d6, $f4d50d87, $455a14ed,
   $a9e3e905, $fcefa3f8, $676f02d9, $8d2a4c8a,
   { Round 3. }
   $fffa3942, $8771f681, $6d9d6122, $fde5380c, $a4beea44, $4bdecfa9,
   $f6bb4b60, $bebfbc70, $289b7ec6, $eaa127fa, $d4ef3085, $04881d05,
   $d9d4d039, $e6db99e5, $1fa27cf8, $c4ac5665,
   { Round 4. }
   $f4292244, $432aff97, $ab9423a7, $fc93a039, $655b59c3, $8f0ccc92,
   $ffeff47d, $85845dd1, $6fa87e4f, $fe2ce6e0, $a3014314, $4e0811a1,
   $f7537e82, $bd3af235, $2ad7d2bb, $eb86d391
  );

{$Q-} // Arithmetic operations performed modulo $100000000
{ TODO : Remove ROL and add IdGlobal to the uses clause, when it is ready for dotNET. }
function ROL(AVal: Cardinal; AShift: Byte): Cardinal;
begin
   Result := (AVal shl AShift) or (AVal shr (32 - AShift));
end;

procedure MD2_MDCoder(var FState: T4x4LongWordRecord; var FX: T384BitRecord;
  var FCheckSum: T128BitRecord; var FCBuffer: T128BitRecord);
const
  NumRounds = 18;
var
  x: Byte;
  i, j: Integer;
  T: Word;
  LCheckSumScore: Byte;
begin
  // Move the next 16 bytes into the second 16 bytes of X.
  for i := 0 to 15 do
  begin
    x := FCBuffer[i];
    FX[i + 16] := x;
    FX[i + 32] := x xor FX[i];
  end;

  { Do 18 rounds. }
  T := 0;
  for i := 0 to NumRounds - 1 do
  begin
    for j := 0 to 47 do
    begin
      T := FX[j] xor MD2_PI_SUBST[T];
      FX[j] := T and $FF;
    end;
    T := (T + i) and $FF;
  end;

  LCheckSumScore := FChecksum[15];
  for i := 0 to 15 do
  begin
    x := FCBuffer[i] xor LCheckSumScore;
    LCheckSumScore := FChecksum[i] xor MD2_PI_SUBST[x];
    FChecksum[i] := LCheckSumScore;
  end;
end;

procedure MD4_MDCoder(var FState: T4x4LongWordRecord; var FCBuffer: T512BitRecord);
var
  A, B, C, D, i : Cardinal;
  buff : T16x4LongWordRecord; // 64-byte buffer
begin
  A := FState[0];
  B := FState[1];
  C := FState[2];
  D := FState[3];

  for i := 0 to 15 do
    buff[i] := FCBuffer[i*4+0] +
               (FCBuffer[i*4+1] shl 8) +
               (FCBuffer[i*4+2] shl 16) +
               (FCBuffer[i*4+3] shl 24);

  // Round 1
  { Note:
      (x and y) or ( (not x) and z)
    is equivalent to
      (((z xor y) and x) xor z)
    -HHellstr鰉 }
  for i := 0 to 3 do
  begin
    A := ROL((((D xor C) and B) xor D) + A + buff[i*4+0],  3);
    D := ROL((((C xor B) and A) xor C) + D + buff[i*4+1],  7);
    C := ROL((((B xor A) and D) xor B) + C + buff[i*4+2], 11);
    B := ROL((((A xor D) and C) xor A) + B + buff[i*4+3], 19);
  end;

  // Round 2
  { Note:
      (x and y) or (x and z) or (y and z)
    is equivalent to
      ((x and y) or (z and (x or y)))
    -HHellstr鰉 }
  for i := 0 to 3 do
  begin
    A := ROL(((B and C) or (D and (B or C))) + A + buff[0*4+i] + $5A827999,  3);
    D := ROL(((A and B) or (C and (A or B))) + D + buff[1*4+i] + $5A827999,  5);
    C := ROL(((D and A) or (B and (D or A))) + C + buff[2*4+i] + $5A827999,  9);
    B := ROL(((C and D) or (A and (C or D))) + B + buff[3*4+i] + $5A827999, 13);
  end;

  // Round 3
  A := ROL((B xor C xor D) + A + buff[ 0] + $6ED9EBA1,  3);
  D := ROL((A xor B xor C) + D + buff[ 8] + $6ED9EBA1,  9);
  C := ROL((D xor A xor B) + C + buff[ 4] + $6ED9EBA1, 11);
  B := ROL((C xor D xor A) + B + buff[12] + $6ED9EBA1, 15);
  A := ROL((B xor C xor D) + A + buff[ 2] + $6ED9EBA1,  3);
  D := ROL((A xor B xor C) + D + buff[10] + $6ED9EBA1,  9);
  C := ROL((D xor A xor B) + C + buff[ 6] + $6ED9EBA1, 11);
  B := ROL((C xor D xor A) + B + buff[14] + $6ED9EBA1, 15);
  A := ROL((B xor C xor D) + A + buff[ 1] + $6ED9EBA1,  3);
  D := ROL((A xor B xor C) + D + buff[ 9] + $6ED9EBA1,  9);
  C := ROL((D xor A xor B) + C + buff[ 5] + $6ED9EBA1, 11);
  B := ROL((C xor D xor A) + B + buff[13] + $6ED9EBA1, 15);
  A := ROL((B xor C xor D) + A + buff[ 3] + $6ED9EBA1,  3);
  D := ROL((A xor B xor C) + D + buff[11] + $6ED9EBA1,  9);
  C := ROL((D xor A xor B) + C + buff[ 7] + $6ED9EBA1, 11);
  B := ROL((C xor D xor A) + B + buff[15] + $6ED9EBA1, 15);

  Inc(FState[0], A);
  Inc(FState[1], B);
  Inc(FState[2], C);
  Inc(FState[3], D);
end;
{$Q+}

{$Q-} // Arithmetic operations performed modulo $100000000
procedure MD5_MDCoder(var FState: T4x4LongWordRecord; var FCBuffer: T512BitRecord);
var
  A, B, C, D : Cardinal;
  i: Integer;
  x : T16x4LongWordRecord; // 64-byte buffer
begin
  A := FState[0];
  B := FState[1];
  C := FState[2];
  D := FState[3];

  for i := 0 to 15 do
  begin
    x[i] := FCBuffer[i*4+0] +
            (FCBuffer[i*4+1] shl 8) +
            (FCBuffer[i*4+2] shl 16) +
            (FCBuffer[i*4+3] shl 24);
  end;
  { Round 1 }
  { Note:
      (x and y) or ( (not x) and z)
    is equivalent to
      (((z xor y) and x) xor z)
    -HHellstr鰉 }
  A := ROL(A + (((D xor C) and B) xor D) + x[ 0] + MD5_SINE[ 1],  7) + B;
  D := ROL(D + (((C xor B) and A) xor C) + x[ 1] + MD5_SINE[ 2], 12) + A;
  C := ROL(C + (((B xor A) and D) xor B) + x[ 2] + MD5_SINE[ 3], 17) + D;
  B := ROL(B + (((A xor D) and C) xor A) + x[ 3] + MD5_SINE[ 4], 22) + C;
  A := ROL(A + (((D xor C) and B) xor D) + x[ 4] + MD5_SINE[ 5],  7) + B;
  D := ROL(D + (((C xor B) and A) xor C) + x[ 5] + MD5_SINE[ 6], 12) + A;
  C := ROL(C + (((B xor A) and D) xor B) + x[ 6] + MD5_SINE[ 7], 17) + D;
  B := ROL(B + (((A xor D) and C) xor A) + x[ 7] + MD5_SINE[ 8], 22) + C;
  A := ROL(A + (((D xor C) and B) xor D) + x[ 8] + MD5_SINE[ 9],  7) + B;
  D := ROL(D + (((C xor B) and A) xor C) + x[ 9] + MD5_SINE[10], 12) + A;
  C := ROL(C + (((B xor A) and D) xor B) + x[10] + MD5_SINE[11], 17) + D;
  B := ROL(B + (((A xor D) and C) xor A) + x[11] + MD5_SINE[12], 22) + C;
  A := ROL(A + (((D xor C) and B) xor D) + x[12] + MD5_SINE[13],  7) + B;
  D := ROL(D + (((C xor B) and A) xor C) + x[13] + MD5_SINE[14], 12) + A;
  C := ROL(C + (((B xor A) and D) xor B) + x[14] + MD5_SINE[15], 17) + D;
  B := ROL(B + (((A xor D) and C) xor A) + x[15] + MD5_SINE[16], 22) + C;

  { Round 2 }
  { Note:
      (x and z) or (y and (not z) )
    is equivalent to
      (((y xor x) and z) xor y)
    -HHellstr鰉 }
  A := ROL(A + (C xor (D and (B xor C))) + x[ 1] + MD5_SINE[17],  5) + B;
  D := ROL(D + (B xor (C and (A xor B))) + x[ 6] + MD5_SINE[18],  9) + A;
  C := ROL(C + (A xor (B and (D xor A))) + x[11] + MD5_SINE[19], 14) + D;
  B := ROL(B + (D xor (A and (C xor D))) + x[ 0] + MD5_SINE[20], 20) + C;
  A := ROL(A + (C xor (D and (B xor C))) + x[ 5] + MD5_SINE[21],  5) + B;
  D := ROL(D + (B xor (C and (A xor B))) + x[10] + MD5_SINE[22],  9) + A;
  C := ROL(C + (A xor (B and (D xor A))) + x[15] + MD5_SINE[23], 14) + D;
  B := ROL(B + (D xor (A and (C xor D))) + x[ 4] + MD5_SINE[24], 20) + C;
  A := ROL(A + (C xor (D and (B xor C))) + x[ 9] + MD5_SINE[25],  5) + B;
  D := ROL(D + (B xor (C and (A xor B))) + x[14] + MD5_SINE[26],  9) + A;
  C := ROL(C + (A xor (B and (D xor A))) + x[ 3] + MD5_SINE[27], 14) + D;
  B := ROL(B + (D xor (A and (C xor D))) + x[ 8] + MD5_SINE[28], 20) + C;
  A := ROL(A + (C xor (D and (B xor C))) + x[13] + MD5_SINE[29],  5) + B;
  D := ROL(D + (B xor (C and (A xor B))) + x[ 2] + MD5_SINE[30],  9) + A;
  C := ROL(C + (A xor (B and (D xor A))) + x[ 7] + MD5_SINE[31], 14) + D;
  B := ROL(B + (D xor (A and (C xor D))) + x[12] + MD5_SINE[32], 20) + C;

  { Round 3. }
  A := ROL(A + (B xor C xor D) + x[ 5] + MD5_SINE[33],  4) + B;
  D := ROL(D + (A xor B xor C) + x[ 8] + MD5_SINE[34], 11) + A;
  C := ROL(C + (D xor A xor B) + x[11] + MD5_SINE[35], 16) + D;
  B := ROL(B + (C xor D xor A) + x[14] + MD5_SINE[36], 23) + C;
  A := ROL(A + (B xor C xor D) + x[ 1] + MD5_SINE[37],  4) + B;
  D := ROL(D + (A xor B xor C) + x[ 4] + MD5_SINE[38], 11) + A;
  C := ROL(C + (D xor A xor B) + x[ 7] + MD5_SINE[39], 16) + D;
  B := ROL(B + (C xor D xor A) + x[10] + MD5_SINE[40], 23) + C;
  A := ROL(A + (B xor C xor D) + x[13] + MD5_SINE[41],  4) + B;
  D := ROL(D + (A xor B xor C) + x[ 0] + MD5_SINE[42], 11) + A;
  C := ROL(C + (D xor A xor B) + x[ 3] + MD5_SINE[43], 16) + D;
  B := ROL(B + (C xor D xor A) + x[ 6] + MD5_SINE[44], 23) + C;
  A := ROL(A + (B xor C xor D) + x[ 9] + MD5_SINE[45],  4) + B;
  D := ROL(D + (A xor B xor C) + x[12] + MD5_SINE[46], 11) + A;
  C := ROL(C + (D xor A xor B) + x[15] + MD5_SINE[47], 16) + D;
  B := ROL(B + (C xor D xor A) + x[ 2] + MD5_SINE[48], 23) + C;

  { Round 4. }
  A := ROL(A + ((B or not D) xor C) + x[ 0] + MD5_SINE[49],  6) + B;
  D := ROL(D + ((A or not C) xor B) + x[ 7] + MD5_SINE[50], 10) + A;
  C := ROL(C + ((D or not B) xor A) + x[14] + MD5_SINE[51], 15) + D;
  B := ROL(B + ((C or not A) xor D) + x[ 5] + MD5_SINE[52], 21) + C;
  A := ROL(A + ((B or not D) xor C) + x[12] + MD5_SINE[53],  6) + B;
  D := ROL(D + ((A or not C) xor B) + x[ 3] + MD5_SINE[54], 10) + A;
  C := ROL(C + ((D or not B) xor A) + x[10] + MD5_SINE[55], 15) + D;
  B := ROL(B + ((C or not A) xor D) + x[ 1] + MD5_SINE[56], 21) + C;
  A := ROL(A + ((B or not D) xor C) + x[ 8] + MD5_SINE[57],  6) + B;
  D := ROL(D + ((A or not C) xor B) + x[15] + MD5_SINE[58], 10) + A;
  C := ROL(C + ((D or not B) xor A) + x[ 6] + MD5_SINE[59], 15) + D;
  B := ROL(B + ((C or not A) xor D) + x[13] + MD5_SINE[60], 21) + C;
  A := ROL(A + ((B or not D) xor C) + x[ 4] + MD5_SINE[61],  6) + B;
  D := ROL(D + ((A or not C) xor B) + x[11] + MD5_SINE[62], 10) + A;
  C := ROL(C + ((D or not B) xor A) + x[ 2] + MD5_SINE[63], 15) + D;
  B := ROL(B + ((C or not A) xor D) + x[ 9] + MD5_SINE[64], 21) + C;

  Inc(FState[0], A);
  Inc(FState[1], B);
  Inc(FState[2], C);
  Inc(FState[3], D);
end;
{$Q+}

type
  TMDCoder = procedure (var FState: T4x4LongWordRecord; var FCBuffer: T512BitRecord);

function MD2_HashValue(AStream: TStream): T4x4LongWordRecord; overload;
Var
  LStartPos: Integer;
  LSize: Int64;
  Pad: Byte;
  I: Integer;
  FCheckSum: T128BitRecord;
  FX: T384BitRecord;
  FCBuffer: T128BitRecord;
  FState: T4x4LongWordRecord;
begin
  FX := SX;
  FCheckSum := SCheckSum;
  FCBuffer := SCBuffer;

  LStartPos := AStream.Position;
  LSize := AStream.Size - LStartPos;

  // Code the entire file in complete 16-byte chunks.
  while LSize - AStream.Position >= 16 do begin
    AStream.Read(FCBuffer, 16);
    MD2_MDCoder(FState, FX, FCheckSum, FCBuffer);
  end;

  LStartPos := AStream.Read(FCBuffer, 16);
  Pad := 16 - LStartPos;
  // Step 1
  for I := LStartPos to 15 do
    FCBuffer[I] := Pad;
  MD2_MDCoder(FState, FX, FCheckSum, FCBuffer);
  // Step 2
  for I := 0 to 15 do
    FCBuffer[I] := FCheckSum[I];
  MD2_MDCoder(FState, FX, FCheckSum, FCBuffer);

  for I := 0 to 3 do
    Result[I] := FX[I*4] +
                 (FX[I*4+1] shl 8) +
                 (FX[I*4+2] shl 16) +
                 (FX[I*4+3] shl 24);
end;

function MD4_HashValue(AStream: TStream; MDCoder: TMDCoder): T4x4LongWordRecord; overload;
var
  LStartPos: Integer;
  LBitSize,
  LSize: Int64;
  I: Integer;
  FCBuffer: T512BitRecord;
  FState: T4x4LongWordRecord;
begin
  FillChar(FCBuffer, SizeOf(FCBuffer), 0);
  FillChar(FState, SizeOf(FState), 0);

  LStartPos := AStream.Position;
  LSize := AStream.Size - LStartPos;

  // A straight assignment would be by ref on dotNET.
  for I := 0 to 3 do
    FState[I] := MD4_INIT_VALUES[I];

  while LSize - AStream.Position >= 64 do begin
    AStream.Read(FCBuffer, 64);
    MDCoder(FState, FCBuffer);
  end;

  // Read the last set of bytes.
  LStartPos := AStream.Read(FCBuffer, 64);
  // Append one bit with value 1
  FCBuffer[LStartPos] := $80;
  LStartPos := LStartPos + 1;

  // Must have sufficient space to insert the 64-bit size value
  if LStartPos > 56 then
  begin
    for I := LStartPos to 63 do
      FCBuffer[I] := 0;
    MDCoder(FState, FCBuffer);
    LStartPos := 0;
  end;
  // Pad with zeroes. Leave room for the 64 bit size value.
  for I := LStartPos to 55 do
    FCBuffer[I] := 0;

  // Append the Number of bits processed.
  LBitSize := LSize * 8;
  for I := 56 to 63 do
  begin
    FCBuffer[I] := LBitSize and $FF;
    LBitSize := LBitSize shr 8;
  end;
  MDCoder(FState, FCBuffer);

  Result := FState;
end;

function HashValue(const ASrc: string; MDCoder: TMDCoder): T4x4LongWordRecord;
var
  LStream: TPointerStream;
begin
  LStream := TPointerStream.Create();
  LStream.SetPointer(Pointer(ASrc), Length(ASrc){$IFDEF UNICODE} shl 1{$ENDIF});
  Result := MD4_HashValue(LStream, MDCoder);
  LStream.Free;
end;

function MD2_HashValue(const ASrc: string): T4x4LongWordRecord; overload;
var
  LStream: TPointerStream;
begin
  LStream := TPointerStream.Create();
  try
    LStream.SetPointer(Pointer(ASrc), Length(ASrc){$IFDEF UNICODE} shl 1{$ENDIF});
    Result := MD2_HashValue(LStream);
  finally LStream.Free; end;
end;

function MD4_HashValue(const ASrc: string): T4x4LongWordRecord; overload;
begin
  Result := HashValue(ASrc, @MD4_MDCoder);
end;

function MD5_HashValue(const ASrc: string): T4x4LongWordRecord; overload;
begin
  Result := HashValue(ASrc, @MD5_MDCoder);
end;

function MD5Bin(const S: string): TBytes;
var
  V: T4x4LongWordRecord;
begin
  V := MD5_HashValue(S);
  SetLength(Result, SizeOf(V));
  Move(V[0], Result[0], SizeOf(V));
end;

function MD5Bin(Stream: TStream): TBytes;
var
  V: T4x4LongWordRecord;
begin
  V := MD4_HashValue(Stream, @MD5_MDCoder);
  SetLength(Result, SizeOf(V));
  Move(V[0], Result[0], SizeOf(V));
end;

function MD5Bin(const P: Pointer; Len: Integer): TBytes;
var
  LStream: TPointerStream;
begin
  LStream := TPointerStream.Create();
  try
    LStream.SetPointer(P, Len);
    Result := MD5Bin(LStream);
  finally
    LStream.Free;
  end;
end;

function MD5BinHex(const P: Pointer; Len: Integer): string;
var
  LStream: TPointerStream;
  V: T4x4LongWordRecord;
begin
  LStream := TPointerStream.Create();
  try
    LStream.SetPointer(P, Len);
    V := MD4_HashValue(LStream, @MD5_MDCoder);
    Result := ToHex(V);
  finally
    LStream.Free;
  end;
end;

function MD5Str(const S: string): RawByteString;
var
  V: T4x4LongWordRecord;
begin
  V := MD5_HashValue(S);
  SetLength(Result, SizeOf(V));
  Move(V[0], Result[1], SizeOf(V));
end;

function MD5Str(Stream: TStream): RawByteString;
var
  V: T4x4LongWordRecord;
begin
  V := MD4_HashValue(Stream, @MD5_MDCoder);
  SetLength(Result, SizeOf(V));
  Move(V[0], Result[1], SizeOf(V));
end;

function MD5Hex(const S: string): string;
begin
  Result := string(ToHex(MD5_HashValue(S)));
end;

function MD5Hex(Stream: TStream): string;
begin
  Result := string(ToHex(MD4_HashValue(Stream, @MD5_MDCoder)));
end;

initialization
  Reset(SCheckSum, SX, SCBuffer);

end.
