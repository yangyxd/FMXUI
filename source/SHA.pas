{*******************************************************}
{       SHA 加密单元 (基于IdHttp中的SHA修改)            }
{       author: YangYxd  2014.11.10                     }
{                                                       }
{*******************************************************}
{
 ver 1.0.0.2  by YangYxd 2018.01.24
 ----------------------------------------------------------------------------
  - 将QDAC中的digest加入，略作修改，感谢QDAC作者swish (www.qdac.cc)

 ver 1.0.0.1  by YangYxd 2016.07.01
 ----------------------------------------------------------------------------
  - 基于IdHttp中的SHA算法修改
}
unit SHA;

interface

{$IF RTLVersion>=24}
{$LEGACYIFEND ON}
{$IFEND}

{$IF defined(FPC)}
  {$DEFINE USEINLINE}
{$IFEND}
{$IF RTLVersion>=18}
  {$DEFINE USEINLINE}
{$IFEND}

{$ifdef VER150}
  {$define Version7}
{$endif}

{$IF (RTLVersion>=26) and (not Defined(NEXTGEN))}
{$DEFINE ANSISTRINGS}
{$IFEND}

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF ANSISTRINGS}AnsiStrings, {$ENDIF}
  {$IFDEF POSIX}Posix.String_, {$ENDIF}
  {$IFDEF NEXTGEN}System.NetEncoding, {$ENDIF}
  Classes, SysUtils;

type
  {$IFDEF NEXTGEN}
  WideString = UnicodeString;
  {$ENDIF}
  {$IFDEF UNICODE}
  StringW = UnicodeString;
  {$ELSE}
  StringW = WideString;
  {$ENDIF}
  PCharW = PWideChar;
  {$IFNDEF USEINLINE}
  TBytes = array of Byte;
  {$ENDIF}

type
  T5x4LongWordRecord = array [0..4] of Cardinal;

  TSHADigestType = (sdt160, sdt256, sdt384, sdt512);

  TSHADigest = record
  {$IFDEF USEINLINE}
  public
    function ToString(): string;
  public
  {$ENDIF}
    HashType: TSHADigestType;
    case Integer of
      0:
        (SHA160: array [0 .. 19] of Byte);
      1:
        (SHA256: array [0 .. 31] of Byte);
      2:
        (SHA384: array [0 .. 47] of Byte);
      3:
        (SHA512: array [0 .. 63] of Byte);
      4:
        (I32: array [0 .. 7] of Cardinal);
      5:
        (I64: array [0 .. 7] of UInt64);
  end;

  TSHAContext = record
    CurrentHash: TSHADigest;
    Index: Cardinal;
    HashBuffer: array [0 .. 127] of Byte;
    case Integer of
      0:
        (LenHi, LenLo: LongWord);
      1:
        (LenHi64, LenLo64: Int64);
  end;

  THashProgressNotify = procedure(AHashed, ATotal: Int64) of object;
  {$IFDEF UNICODE}
  THashProgressNotifyA = reference to procedure(AHashed, ATotal: Int64);
  PHashProgressNotifyA = ^THashProgressNotifyA;
  {$ENDIF}

function AnsiEncode(const P: StringW): TBytes; overload;
function AnsiEncode(P: PCharW; l: Integer): TBytes; overload;

function StringToStream(const V: string): TStream;

function ToHexBytes(const AValue: TBytes): string; overload;
function ToHexBytes(Value: PByte; Len: Integer): string; overload;

function SHA160Hash(const p: Pointer; len: Integer): TSHADigest; overload;
function SHA160Hash(const S: string): TSHADigest; overload;
function SHA160Hash(AStream: TStream; AOnProgress: THashProgressNotify = nil): TSHADigest; overload;
function SHA160File(AFileName: string; AOnProgress: THashProgressNotify = nil): TSHADigest; overload;
{$IFDEF UNICODE}
function SHA160Hash(AStream: TStream; AOnProgress: THashProgressNotifyA): TSHADigest; overload;
function SHA160File(const AFileName: string; AOnProgress: THashProgressNotifyA): TSHADigest; overload;
{$ENDIF}
function SHA256Hash(const p: Pointer; len: Integer): TSHADigest; overload;
function SHA256Hash(const S: string): TSHADigest; overload;
function SHA256Hash(AStream: TStream; AOnProgress: THashProgressNotify = nil): TSHADigest; overload;
function SHA256File(const AFileName: string; AOnProgress: THashProgressNotify = nil): TSHADigest; overload;
{$IFDEF UNICODE}
function SHA256Hash(AStream: TStream; AOnProgress: THashProgressNotifyA): TSHADigest; overload;
function SHA256File(const AFileName: string; AOnProgress: THashProgressNotifyA): TSHADigest; overload;
{$ENDIF}
function SHA384Hash(const p: Pointer; len: Integer): TSHADigest; overload;
function SHA384Hash(const S: string): TSHADigest; overload;
function SHA384Hash(AStream: TStream; AOnProgress: THashProgressNotify = nil): TSHADigest; overload;
function SHA384File(const AFileName: string; AOnProgress: THashProgressNotify = nil): TSHADigest; overload;
{$IFDEF UNICODE}
function SHA384Hash(AStream: TStream; AOnProgress: THashProgressNotifyA): TSHADigest; overload;
function SHA384File(const AFileName: string; AOnProgress: THashProgressNotifyA): TSHADigest; overload;
{$ENDIF}
function SHA512Hash(const p: Pointer; len: Integer): TSHADigest; overload;
function SHA512Hash(const S: string): TSHADigest; overload;
function SHA512Hash(AStream: TStream; AOnProgress: THashProgressNotify = nil): TSHADigest; overload;
function SHA512File(const AFileName: string; AOnProgress: THashProgressNotify = nil): TSHADigest; overload;
{$IFDEF UNICODE}
function SHA512Hash(AStream: TStream; AOnProgress: THashProgressNotifyA): TSHADigest; overload;
function SHA512File(const AFileName: string; AOnProgress: THashProgressNotifyA): TSHADigest; overload;
{$ENDIF}

function DigestToString(const ADigest: TSHADigest): string;


implementation

const
  HexDigits: array [0..15] of Char =
    ('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'); {do not localize}
type
  TPointerStream = class(TCustomMemoryStream)
  public
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

function TPointerStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

{$IFDEF USEINLINE}
function TSHADigest.ToString(): string;
begin
  Result := DigestToString(Self);
end;
{$ENDIF}

function StringToStream(const V: string): TStream;
{$IFDEF UNICODE}
var
  LValue: TBytes;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  LValue := AnsiEncode(V);
  Result := TMemoryStream.Create;
  Result.Write(LValue[0], Length(LValue));
  {$ELSE}
  Result := TPointerStream.Create;
  TPointerStream(Result).SetPointer(Pointer(V), Length(V));
  {$ENDIF}
  Result.Position := 0;
end;

const
  BufferSize = 64;

type
  T512BitRecord = array [0..BufferSize-1] of Byte;

function AnsiEncode(P: PCharW; l: Integer): TBytes;
var
  ps: PWideChar;
  {$IFDEF MSWINDOWS}
  len: Integer;
  {$ELSE}
  buf: TBytes;
  {$ENDIF}
begin
  if l<=0 then begin
    ps:=p;
    while ps^<>#0 do Inc(ps);
    l:=ps-p;
  end;
  if l > 0 then  begin
    {$IFDEF MSWINDOWS}
    len := WideCharToMultiByte(CP_ACP,0,p,l,nil,0,nil,nil);
    SetLength(Result, len);
    WideCharToMultiByte(CP_ACP,0,p,l,PAnsiChar(Result), len, nil, nil);
    {$ELSE}
    SetLength(buf, l shl 1);
    Move(p^, buf[0], l shl 1);
    Result := TEncoding.Convert(TEncoding.Unicode, TEncoding.ANSI, buf);
    {$ENDIF}
  end else
    SetLength(Result, 0);
end;

function AnsiEncode(const P: StringW): TBytes;
begin
  Result := AnsiEncode(PCharW(p), Length(p));
end;

function ToHexBytes(const AValue: TBytes): string;
var
  i: Integer;
  PD: PChar;
begin
  SetLength(Result, Length(AValue)*2);
  PD := PChar(Result);
  for i:=0 to Length(AValue)-1 do begin
    PD^ := HexDigits[AValue[i] shr 4];
    Inc(PD);
    PD^ := HexDigits[AValue[i] and $F];
    Inc(PD);
  end;
end;

function ToHexBytes(Value: PByte; Len: Integer): string;
var
  i: Integer;
  PD: PChar;
begin
  SetLength(Result, Len * 2);
  PD := PChar(Result);
  for i:=0 to Len - 1 do begin
    PD^ := HexDigits[Value^ shr 4];
    Inc(PD);
    PD^ := HexDigits[Value^ and $F];
    Inc(PD);
    Inc(Value);
  end;
end;

procedure SHAInit(var AContext: TSHAContext; AType: TSHADigestType);
begin
  FillChar(AContext, SizeOf(AContext), 0);
  AContext.CurrentHash.HashType := AType;
  case AType of
    sdt160:
      begin
        AContext.CurrentHash.I32[0] := $67452301;
        AContext.CurrentHash.I32[1] := $EFCDAB89;
        AContext.CurrentHash.I32[2] := $98BADCFE;
        AContext.CurrentHash.I32[3] := $10325476;
        AContext.CurrentHash.I32[4] := $C3D2E1F0;
      end;
    sdt256:
      begin
        AContext.CurrentHash.I32[0] := $6A09E667;
        AContext.CurrentHash.I32[1] := $BB67AE85;
        AContext.CurrentHash.I32[2] := $3C6EF372;
        AContext.CurrentHash.I32[3] := $A54FF53A;
        AContext.CurrentHash.I32[4] := $510E527F;
        AContext.CurrentHash.I32[5] := $9B05688C;
        AContext.CurrentHash.I32[6] := $1F83D9AB;
        AContext.CurrentHash.I32[7] := $5BE0CD19;
      end;
    sdt384:
      begin
        AContext.CurrentHash.I64[0] := UInt64($CBBB9D5DC1059ED8);
        AContext.CurrentHash.I64[1] := UInt64($629A292A367CD507);
        AContext.CurrentHash.I64[2] := UInt64($9159015A3070DD17);
        AContext.CurrentHash.I64[3] := UInt64($152FECD8F70E5939);
        AContext.CurrentHash.I64[4] := UInt64($67332667FFC00B31);
        AContext.CurrentHash.I64[5] := UInt64($8EB44A8768581511);
        AContext.CurrentHash.I64[6] := UInt64($DB0C2E0D64F98FA7);
        AContext.CurrentHash.I64[7] := UInt64($47B5481DBEFA4FA4);
      end;
    sdt512:
      begin
        AContext.CurrentHash.I64[0] := UInt64($6A09E667F3BCC908);
        AContext.CurrentHash.I64[1] := UInt64($BB67AE8584CAA73B);
        AContext.CurrentHash.I64[2] := UInt64($3C6EF372FE94F82B);
        AContext.CurrentHash.I64[3] := UInt64($A54FF53A5F1D36F1);
        AContext.CurrentHash.I64[4] := UInt64($510E527FADE682D1);
        AContext.CurrentHash.I64[5] := UInt64($9B05688C2B3E6C1F);
        AContext.CurrentHash.I64[6] := UInt64($1F83D9ABFB41BD6B);
        AContext.CurrentHash.I64[7] := UInt64($5BE0CD19137E2179);
      end;
  end;
end;

function SwapDWord(a: LongWord): LongWord; {$IFDEF USEINLINE}inline;{$ENDIF}
begin
  Result := ((a and $FF) shl 24) or ((a and $FF00) shl 8) or
    ((a and $FF0000) shr 8) or ((a and $FF000000) shr 24);
end;

function SwapQWord(a: Int64): Int64; {$IFDEF USEINLINE}inline;{$ENDIF}
begin
  Result := ((a and $FF) shl 56) or ((a and $FF00) shl 40) or
    ((a and $FF0000) shl 24) or ((a and $FF000000) shl 8) or
    ((a and $FF00000000) shr 8) or ((a and $FF0000000000) shr 24) or
    ((a and $FF000000000000) shr 40) or ((a and $FF00000000000000) shr 56);
end;

{$WARNINGS OFF}
procedure SHACompress(var AContext: TSHAContext);
  procedure SHA160Compress;
  var
    a, b, c, d, E: LongWord;
    W: array [0 .. 79] of LongWord;
    I: LongWord;
  begin
    AContext.Index := 0;
    FillChar(W, SizeOf(W), 0);
    Move(AContext.HashBuffer, W, 64);
    for I := 0 to 15 do
      W[I] := SwapDWord(W[I]);
    for I := 16 to 79 do
      W[I] := ((W[I - 3] xor W[I - 8] xor W[I - 14] xor W[I - 16]) shl 1) or
        ((W[I - 3] xor W[I - 8] xor W[I - 14] xor W[I - 16]) shr 31);
    a := AContext.CurrentHash.I32[0];
    b := AContext.CurrentHash.I32[1];
    c := AContext.CurrentHash.I32[2];
    d := AContext.CurrentHash.I32[3];
    E := AContext.CurrentHash.I32[4];
    Inc(E, ((a shl 5) or (a shr 27)) + (d xor (b and (c xor d))) +
      $5A827999 + W[0]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + (c xor (a and (b xor c))) +
      $5A827999 + W[1]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + (b xor (E and (a xor b))) +
      $5A827999 + W[2]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + (a xor (d and (E xor a))) +
      $5A827999 + W[3]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + (E xor (c and (d xor E))) +
      $5A827999 + W[4]);
    c := (c shl 30) or (c shr 2);
    Inc(E, ((a shl 5) or (a shr 27)) + (d xor (b and (c xor d))) +
      $5A827999 + W[5]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + (c xor (a and (b xor c))) +
      $5A827999 + W[6]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + (b xor (E and (a xor b))) +
      $5A827999 + W[7]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + (a xor (d and (E xor a))) +
      $5A827999 + W[8]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + (E xor (c and (d xor E))) +
      $5A827999 + W[9]);
    c := (c shl 30) or (c shr 2);
    Inc(E, ((a shl 5) or (a shr 27)) + (d xor (b and (c xor d))) +
      $5A827999 + W[10]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + (c xor (a and (b xor c))) +
      $5A827999 + W[11]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + (b xor (E and (a xor b))) +
      $5A827999 + W[12]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + (a xor (d and (E xor a))) +
      $5A827999 + W[13]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + (E xor (c and (d xor E))) +
      $5A827999 + W[14]);
    c := (c shl 30) or (c shr 2);
    Inc(E, ((a shl 5) or (a shr 27)) + (d xor (b and (c xor d))) +
      $5A827999 + W[15]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + (c xor (a and (b xor c))) +
      $5A827999 + W[16]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + (b xor (E and (a xor b))) +
      $5A827999 + W[17]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + (a xor (d and (E xor a))) +
      $5A827999 + W[18]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + (E xor (c and (d xor E))) +
      $5A827999 + W[19]);
    c := (c shl 30) or (c shr 2);

    Inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $6ED9EBA1 + W[20]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $6ED9EBA1 + W[21]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $6ED9EBA1 + W[22]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $6ED9EBA1 + W[23]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $6ED9EBA1 + W[24]);
    c := (c shl 30) or (c shr 2);
    Inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $6ED9EBA1 + W[25]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $6ED9EBA1 + W[26]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $6ED9EBA1 + W[27]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $6ED9EBA1 + W[28]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $6ED9EBA1 + W[29]);
    c := (c shl 30) or (c shr 2);
    Inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $6ED9EBA1 + W[30]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $6ED9EBA1 + W[31]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $6ED9EBA1 + W[32]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $6ED9EBA1 + W[33]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $6ED9EBA1 + W[34]);
    c := (c shl 30) or (c shr 2);
    Inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $6ED9EBA1 + W[35]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $6ED9EBA1 + W[36]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $6ED9EBA1 + W[37]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $6ED9EBA1 + W[38]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $6ED9EBA1 + W[39]);
    c := (c shl 30) or (c shr 2);

    Inc(E, ((a shl 5) or (a shr 27)) + ((b and c) or (d and (b or c))) +
      $8F1BBCDC + W[40]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + ((a and b) or (c and (a or b))) +
      $8F1BBCDC + W[41]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + ((E and a) or (b and (E or a))) +
      $8F1BBCDC + W[42]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + ((d and E) or (a and (d or E))) +
      $8F1BBCDC + W[43]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + ((c and d) or (E and (c or d))) +
      $8F1BBCDC + W[44]);
    c := (c shl 30) or (c shr 2);
    Inc(E, ((a shl 5) or (a shr 27)) + ((b and c) or (d and (b or c))) +
      $8F1BBCDC + W[45]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + ((a and b) or (c and (a or b))) +
      $8F1BBCDC + W[46]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + ((E and a) or (b and (E or a))) +
      $8F1BBCDC + W[47]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + ((d and E) or (a and (d or E))) +
      $8F1BBCDC + W[48]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + ((c and d) or (E and (c or d))) +
      $8F1BBCDC + W[49]);
    c := (c shl 30) or (c shr 2);
    Inc(E, ((a shl 5) or (a shr 27)) + ((b and c) or (d and (b or c))) +
      $8F1BBCDC + W[50]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + ((a and b) or (c and (a or b))) +
      $8F1BBCDC + W[51]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + ((E and a) or (b and (E or a))) +
      $8F1BBCDC + W[52]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + ((d and E) or (a and (d or E))) +
      $8F1BBCDC + W[53]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + ((c and d) or (E and (c or d))) +
      $8F1BBCDC + W[54]);
    c := (c shl 30) or (c shr 2);
    Inc(E, ((a shl 5) or (a shr 27)) + ((b and c) or (d and (b or c))) +
      $8F1BBCDC + W[55]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + ((a and b) or (c and (a or b))) +
      $8F1BBCDC + W[56]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + ((E and a) or (b and (E or a))) +
      $8F1BBCDC + W[57]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + ((d and E) or (a and (d or E))) +
      $8F1BBCDC + W[58]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + ((c and d) or (E and (c or d))) +
      $8F1BBCDC + W[59]);
    c := (c shl 30) or (c shr 2);

    Inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $CA62C1D6 + W[60]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $CA62C1D6 + W[61]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $CA62C1D6 + W[62]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $CA62C1D6 + W[63]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $CA62C1D6 + W[64]);
    c := (c shl 30) or (c shr 2);
    Inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $CA62C1D6 + W[65]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $CA62C1D6 + W[66]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $CA62C1D6 + W[67]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $CA62C1D6 + W[68]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $CA62C1D6 + W[69]);
    c := (c shl 30) or (c shr 2);
    Inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $CA62C1D6 + W[70]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $CA62C1D6 + W[71]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $CA62C1D6 + W[72]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $CA62C1D6 + W[73]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $CA62C1D6 + W[74]);
    c := (c shl 30) or (c shr 2);
    Inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $CA62C1D6 + W[75]);
    b := (b shl 30) or (b shr 2);
    Inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $CA62C1D6 + W[76]);
    a := (a shl 30) or (a shr 2);
    Inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $CA62C1D6 + W[77]);
    E := (E shl 30) or (E shr 2);
    Inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $CA62C1D6 + W[78]);
    d := (d shl 30) or (d shr 2);
    Inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $CA62C1D6 + W[79]);
    c := (c shl 30) or (c shr 2);
    AContext.CurrentHash.I32[0] := AContext.CurrentHash.I32[0] + a;
    AContext.CurrentHash.I32[1] := AContext.CurrentHash.I32[1] + b;
    AContext.CurrentHash.I32[2] := AContext.CurrentHash.I32[2] + c;
    AContext.CurrentHash.I32[3] := AContext.CurrentHash.I32[3] + d;
    AContext.CurrentHash.I32[4] := AContext.CurrentHash.I32[4] + E;
    FillChar(AContext.HashBuffer, 64, 0);
  end;
  procedure SHA256Compress;
  var
    a, b, c, d, E, F, G, H, t1, t2: Cardinal;
    W: array [0 .. 63] of Cardinal;
    I: LongWord;
  begin
    AContext.Index := 0;
    FillChar(W, SizeOf(W), 0);
    a := AContext.CurrentHash.I32[0];
    b := AContext.CurrentHash.I32[1];
    c := AContext.CurrentHash.I32[2];
    d := AContext.CurrentHash.I32[3];
    E := AContext.CurrentHash.I32[4];
    F := AContext.CurrentHash.I32[5];
    G := AContext.CurrentHash.I32[6];
    H := AContext.CurrentHash.I32[7];
    Move(AContext.HashBuffer, W, 64);
    for I := 0 to 15 do
      W[I] := SwapDWord(W[I]);
    for I := 16 to 63 do
      W[I] := (((W[I - 2] shr 17) or (W[I - 2] shl 15))
        xor ((W[I - 2] shr 19) or (W[I - 2] shl 13)) xor (W[I - 2] shr 10)) +
        W[I - 7] + (((W[I - 15] shr 7) or (W[I - 15] shl 25))
        xor ((W[I - 15] shr 18) or (W[I - 15] shl 14)) xor (W[I - 15] shr 3)) +
        W[I - 16];
    t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
      xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
      $428A2F98 + W[0];
    t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
      xor ((a shr 22) xor (a shl 10))) +
      ((a and b) xor (a and c) xor (b and c));
    H := t1 + t2;
    d := d + t1;
    t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
      xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
      $71374491 + W[1];
    t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
      xor ((H shr 22) xor (H shl 10))) +
      ((H and a) xor (H and b) xor (a and b));
    G := t1 + t2;
    c := c + t1;
    t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
      xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
      $B5C0FBCF + W[2];
    t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
      xor ((G shr 22) xor (G shl 10))) +
      ((G and H) xor (G and a) xor (H and a));
    F := t1 + t2;
    b := b + t1;
    t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
      xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
      $E9B5DBA5 + W[3];
    t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
      xor ((F shr 22) xor (F shl 10))) +
      ((F and G) xor (F and H) xor (G and H));
    E := t1 + t2;
    a := a + t1;
    t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
      xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
      $3956C25B + W[4];
    t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
      xor ((E shr 22) xor (E shl 10))) +
      ((E and F) xor (E and G) xor (F and G));
    d := t1 + t2;
    H := H + t1;
    t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
      xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
      $59F111F1 + W[5];
    t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
      xor ((d shr 22) xor (d shl 10))) +
      ((d and E) xor (d and F) xor (E and F));
    c := t1 + t2;
    G := G + t1;
    t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
      xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
      $923F82A4 + W[6];
    t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
      xor ((c shr 22) xor (c shl 10))) +
      ((c and d) xor (c and E) xor (d and E));
    b := t1 + t2;
    F := F + t1;
    t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
      xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
      $AB1C5ED5 + W[7];
    t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
      xor ((b shr 22) xor (b shl 10))) +
      ((b and c) xor (b and d) xor (c and d));
    a := t1 + t2;
    E := E + t1;
    t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
      xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
      $D807AA98 + W[8];
    t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
      xor ((a shr 22) xor (a shl 10))) +
      ((a and b) xor (a and c) xor (b and c));
    H := t1 + t2;
    d := d + t1;
    t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
      xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
      $12835B01 + W[9];
    t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
      xor ((H shr 22) xor (H shl 10))) +
      ((H and a) xor (H and b) xor (a and b));
    G := t1 + t2;
    c := c + t1;
    t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
      xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
      $243185BE + W[10];
    t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
      xor ((G shr 22) xor (G shl 10))) +
      ((G and H) xor (G and a) xor (H and a));
    F := t1 + t2;
    b := b + t1;
    t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
      xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
      $550C7DC3 + W[11];
    t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
      xor ((F shr 22) xor (F shl 10))) +
      ((F and G) xor (F and H) xor (G and H));
    E := t1 + t2;
    a := a + t1;
    t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
      xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
      $72BE5D74 + W[12];
    t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
      xor ((E shr 22) xor (E shl 10))) +
      ((E and F) xor (E and G) xor (F and G));
    d := t1 + t2;
    H := H + t1;
    t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
      xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
      $80DEB1FE + W[13];
    t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
      xor ((d shr 22) xor (d shl 10))) +
      ((d and E) xor (d and F) xor (E and F));
    c := t1 + t2;
    G := G + t1;
    t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
      xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
      $9BDC06A7 + W[14];
    t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
      xor ((c shr 22) xor (c shl 10))) +
      ((c and d) xor (c and E) xor (d and E));
    b := t1 + t2;
    F := F + t1;
    t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
      xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
      $C19BF174 + W[15];
    t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
      xor ((b shr 22) xor (b shl 10))) +
      ((b and c) xor (b and d) xor (c and d));
    a := t1 + t2;
    E := E + t1;
    t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
      xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
      $E49B69C1 + W[16];
    t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
      xor ((a shr 22) xor (a shl 10))) +
      ((a and b) xor (a and c) xor (b and c));
    H := t1 + t2;
    d := d + t1;
    t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
      xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
      $EFBE4786 + W[17];
    t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
      xor ((H shr 22) xor (H shl 10))) +
      ((H and a) xor (H and b) xor (a and b));
    G := t1 + t2;
    c := c + t1;
    t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
      xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
      $0FC19DC6 + W[18];
    t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
      xor ((G shr 22) xor (G shl 10))) +
      ((G and H) xor (G and a) xor (H and a));
    F := t1 + t2;
    b := b + t1;
    t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
      xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
      $240CA1CC + W[19];
    t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
      xor ((F shr 22) xor (F shl 10))) +
      ((F and G) xor (F and H) xor (G and H));
    E := t1 + t2;
    a := a + t1;
    t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
      xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
      $2DE92C6F + W[20];
    t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
      xor ((E shr 22) xor (E shl 10))) +
      ((E and F) xor (E and G) xor (F and G));
    d := t1 + t2;
    H := H + t1;
    t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
      xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
      $4A7484AA + W[21];
    t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
      xor ((d shr 22) xor (d shl 10))) +
      ((d and E) xor (d and F) xor (E and F));
    c := t1 + t2;
    G := G + t1;
    t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
      xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
      $5CB0A9DC + W[22];
    t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
      xor ((c shr 22) xor (c shl 10))) +
      ((c and d) xor (c and E) xor (d and E));
    b := t1 + t2;
    F := F + t1;
    t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
      xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
      $76F988DA + W[23];
    t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
      xor ((b shr 22) xor (b shl 10))) +
      ((b and c) xor (b and d) xor (c and d));
    a := t1 + t2;
    E := E + t1;
    t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
      xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
      $983E5152 + W[24];
    t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
      xor ((a shr 22) xor (a shl 10))) +
      ((a and b) xor (a and c) xor (b and c));
    H := t1 + t2;
    d := d + t1;
    t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
      xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
      $A831C66D + W[25];
    t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
      xor ((H shr 22) xor (H shl 10))) +
      ((H and a) xor (H and b) xor (a and b));
    G := t1 + t2;
    c := c + t1;
    t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
      xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
      $B00327C8 + W[26];
    t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
      xor ((G shr 22) xor (G shl 10))) +
      ((G and H) xor (G and a) xor (H and a));
    F := t1 + t2;
    b := b + t1;
    t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
      xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
      $BF597FC7 + W[27];
    t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
      xor ((F shr 22) xor (F shl 10))) +
      ((F and G) xor (F and H) xor (G and H));
    E := t1 + t2;
    a := a + t1;
    t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
      xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
      $C6E00BF3 + W[28];
    t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
      xor ((E shr 22) xor (E shl 10))) +
      ((E and F) xor (E and G) xor (F and G));
    d := t1 + t2;
    H := H + t1;
    t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
      xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
      $D5A79147 + W[29];
    t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
      xor ((d shr 22) xor (d shl 10))) +
      ((d and E) xor (d and F) xor (E and F));
    c := t1 + t2;
    G := G + t1;
    t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
      xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
      $06CA6351 + W[30];
    t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
      xor ((c shr 22) xor (c shl 10))) +
      ((c and d) xor (c and E) xor (d and E));
    b := t1 + t2;
    F := F + t1;
    t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
      xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
      $14292967 + W[31];
    t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
      xor ((b shr 22) xor (b shl 10))) +
      ((b and c) xor (b and d) xor (c and d));
    a := t1 + t2;
    E := E + t1;
    t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
      xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
      $27B70A85 + W[32];
    t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
      xor ((a shr 22) xor (a shl 10))) +
      ((a and b) xor (a and c) xor (b and c));
    H := t1 + t2;
    d := d + t1;
    t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
      xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
      $2E1B2138 + W[33];
    t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
      xor ((H shr 22) xor (H shl 10))) +
      ((H and a) xor (H and b) xor (a and b));
    G := t1 + t2;
    c := c + t1;
    t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
      xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
      $4D2C6DFC + W[34];
    t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
      xor ((G shr 22) xor (G shl 10))) +
      ((G and H) xor (G and a) xor (H and a));
    F := t1 + t2;
    b := b + t1;
    t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
      xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
      $53380D13 + W[35];
    t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
      xor ((F shr 22) xor (F shl 10))) +
      ((F and G) xor (F and H) xor (G and H));
    E := t1 + t2;
    a := a + t1;
    t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
      xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
      $650A7354 + W[36];
    t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
      xor ((E shr 22) xor (E shl 10))) +
      ((E and F) xor (E and G) xor (F and G));
    d := t1 + t2;
    H := H + t1;
    t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
      xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
      $766A0ABB + W[37];
    t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
      xor ((d shr 22) xor (d shl 10))) +
      ((d and E) xor (d and F) xor (E and F));
    c := t1 + t2;
    G := G + t1;
    t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
      xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
      $81C2C92E + W[38];
    t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
      xor ((c shr 22) xor (c shl 10))) +
      ((c and d) xor (c and E) xor (d and E));
    b := t1 + t2;
    F := F + t1;
    t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
      xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
      $92722C85 + W[39];
    t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
      xor ((b shr 22) xor (b shl 10))) +
      ((b and c) xor (b and d) xor (c and d));
    a := t1 + t2;
    E := E + t1;
    t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
      xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
      $A2BFE8A1 + W[40];
    t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
      xor ((a shr 22) xor (a shl 10))) +
      ((a and b) xor (a and c) xor (b and c));
    H := t1 + t2;
    d := d + t1;
    t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
      xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
      $A81A664B + W[41];
    t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
      xor ((H shr 22) xor (H shl 10))) +
      ((H and a) xor (H and b) xor (a and b));
    G := t1 + t2;
    c := c + t1;
    t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
      xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
      $C24B8B70 + W[42];
    t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
      xor ((G shr 22) xor (G shl 10))) +
      ((G and H) xor (G and a) xor (H and a));
    F := t1 + t2;
    b := b + t1;
    t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
      xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
      $C76C51A3 + W[43];
    t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
      xor ((F shr 22) xor (F shl 10))) +
      ((F and G) xor (F and H) xor (G and H));
    E := t1 + t2;
    a := a + t1;
    t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
      xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
      $D192E819 + W[44];
    t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
      xor ((E shr 22) xor (E shl 10))) +
      ((E and F) xor (E and G) xor (F and G));
    d := t1 + t2;
    H := H + t1;
    t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
      xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
      $D6990624 + W[45];
    t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
      xor ((d shr 22) xor (d shl 10))) +
      ((d and E) xor (d and F) xor (E and F));
    c := t1 + t2;
    G := G + t1;
    t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
      xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
      $F40E3585 + W[46];
    t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
      xor ((c shr 22) xor (c shl 10))) +
      ((c and d) xor (c and E) xor (d and E));
    b := t1 + t2;
    F := F + t1;
    t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
      xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
      $106AA070 + W[47];
    t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
      xor ((b shr 22) xor (b shl 10))) +
      ((b and c) xor (b and d) xor (c and d));
    a := t1 + t2;
    E := E + t1;
    t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
      xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
      $19A4C116 + W[48];
    t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
      xor ((a shr 22) xor (a shl 10))) +
      ((a and b) xor (a and c) xor (b and c));
    H := t1 + t2;
    d := d + t1;
    t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
      xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
      $1E376C08 + W[49];
    t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
      xor ((H shr 22) xor (H shl 10))) +
      ((H and a) xor (H and b) xor (a and b));
    G := t1 + t2;
    c := c + t1;
    t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
      xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
      $2748774C + W[50];
    t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
      xor ((G shr 22) xor (G shl 10))) +
      ((G and H) xor (G and a) xor (H and a));
    F := t1 + t2;
    b := b + t1;
    t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
      xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
      $34B0BCB5 + W[51];
    t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
      xor ((F shr 22) xor (F shl 10))) +
      ((F and G) xor (F and H) xor (G and H));
    E := t1 + t2;
    a := a + t1;
    t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
      xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
      $391C0CB3 + W[52];
    t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
      xor ((E shr 22) xor (E shl 10))) +
      ((E and F) xor (E and G) xor (F and G));
    d := t1 + t2;
    H := H + t1;
    t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
      xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
      $4ED8AA4A + W[53];
    t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
      xor ((d shr 22) xor (d shl 10))) +
      ((d and E) xor (d and F) xor (E and F));
    c := t1 + t2;
    G := G + t1;
    t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
      xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
      $5B9CCA4F + W[54];
    t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
      xor ((c shr 22) xor (c shl 10))) +
      ((c and d) xor (c and E) xor (d and E));
    b := t1 + t2;
    F := F + t1;
    t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
      xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
      $682E6FF3 + W[55];
    t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
      xor ((b shr 22) xor (b shl 10))) +
      ((b and c) xor (b and d) xor (c and d));
    a := t1 + t2;
    E := E + t1;
    t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
      xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
      $748F82EE + W[56];
    t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
      xor ((a shr 22) xor (a shl 10))) +
      ((a and b) xor (a and c) xor (b and c));
    H := t1 + t2;
    d := d + t1;
    t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
      xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
      $78A5636F + W[57];
    t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
      xor ((H shr 22) xor (H shl 10))) +
      ((H and a) xor (H and b) xor (a and b));
    G := t1 + t2;
    c := c + t1;
    t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
      xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
      $84C87814 + W[58];
    t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
      xor ((G shr 22) xor (G shl 10))) +
      ((G and H) xor (G and a) xor (H and a));
    F := t1 + t2;
    b := b + t1;
    t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
      xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
      $8CC70208 + W[59];
    t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
      xor ((F shr 22) xor (F shl 10))) +
      ((F and G) xor (F and H) xor (G and H));
    E := t1 + t2;
    a := a + t1;
    t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
      xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
      $90BEFFFA + W[60];
    t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
      xor ((E shr 22) xor (E shl 10))) +
      ((E and F) xor (E and G) xor (F and G));
    d := t1 + t2;
    H := H + t1;
    t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
      xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
      $A4506CEB + W[61];
    t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
      xor ((d shr 22) xor (d shl 10))) +
      ((d and E) xor (d and F) xor (E and F));
    c := t1 + t2;
    G := G + t1;
    t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
      xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
      $BEF9A3F7 + W[62];
    t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
      xor ((c shr 22) xor (c shl 10))) +
      ((c and d) xor (c and E) xor (d and E));
    b := t1 + t2;
    F := F + t1;
    t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
      xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
      $C67178F2 + W[63];
    t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
      xor ((b shr 22) xor (b shl 10))) +
      ((b and c) xor (b and d) xor (c and d));
    a := t1 + t2;
    E := E + t1;
    AContext.CurrentHash.I32[0] := AContext.CurrentHash.I32[0] + a;
    AContext.CurrentHash.I32[1] := AContext.CurrentHash.I32[1] + b;
    AContext.CurrentHash.I32[2] := AContext.CurrentHash.I32[2] + c;
    AContext.CurrentHash.I32[3] := AContext.CurrentHash.I32[3] + d;
    AContext.CurrentHash.I32[4] := AContext.CurrentHash.I32[4] + E;
    AContext.CurrentHash.I32[5] := AContext.CurrentHash.I32[5] + F;
    AContext.CurrentHash.I32[6] := AContext.CurrentHash.I32[6] + G;
    AContext.CurrentHash.I32[7] := AContext.CurrentHash.I32[7] + H;
    FillChar(AContext.HashBuffer, 64, 0);
  end;
  procedure SHA384Compress;
  var
    a, b, c, d, E, F, G, H, t1, t2: Int64;
    W: array [0 .. 79] of Int64;
    I: LongWord;
  begin
    AContext.Index := 0;
    FillChar(W, SizeOf(W), 0);
    a := AContext.CurrentHash.I64[0];
    b := AContext.CurrentHash.I64[1];
    c := AContext.CurrentHash.I64[2];
    d := AContext.CurrentHash.I64[3];
    E := AContext.CurrentHash.I64[4];
    F := AContext.CurrentHash.I64[5];
    G := AContext.CurrentHash.I64[6];
    H := AContext.CurrentHash.I64[7];
    Move(AContext.HashBuffer, W, 128);
    for I := 0 to 15 do
      W[I] := SwapQWord(W[I]);
    for I := 16 to 79 do
      W[I] := (((W[I - 2] shr 19) or (W[I - 2] shl 45))
        xor ((W[I - 2] shr 61) or (W[I - 2] shl 3)) xor (W[I - 2] shr 6)) +
        W[I - 7] + (((W[I - 15] shr 1) or (W[I - 15] shl 63))
        xor ((W[I - 15] shr 8) or (W[I - 15] shl 56)) xor (W[I - 15] shr 7)) +
        W[I - 16];
    t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
      xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
      $428A2F98D728AE22 + W[0];
    t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
      xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
    d := d + t1;
    H := t1 + t2;
    t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
      xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
      $7137449123EF65CD + W[1];
    t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
      xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
    c := c + t1;
    G := t1 + t2;
    t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
      xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
      $B5C0FBCFEC4D3B2F + W[2];
    t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
      xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
    b := b + t1;
    F := t1 + t2;
    t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
      xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
      $E9B5DBA58189DBBC + W[3];
    t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
      xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
    a := a + t1;
    E := t1 + t2;
    t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
      xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
      $3956C25BF348B538 + W[4];
    t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
      xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
    H := H + t1;
    d := t1 + t2;
    t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
      xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
      $59F111F1B605D019 + W[5];
    t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
      xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
    G := G + t1;
    c := t1 + t2;
    t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
      xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
      $923F82A4AF194F9B + W[6];
    t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
      xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
    F := F + t1;
    b := t1 + t2;
    t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
      xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
      $AB1C5ED5DA6D8118 + W[7];
    t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
      xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
    E := E + t1;
    a := t1 + t2;
    t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
      xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
      $D807AA98A3030242 + W[8];
    t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
      xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
    d := d + t1;
    H := t1 + t2;
    t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
      xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
      $12835B0145706FBE + W[9];
    t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
      xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
    c := c + t1;
    G := t1 + t2;
    t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
      xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
      $243185BE4EE4B28C + W[10];
    t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
      xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
    b := b + t1;
    F := t1 + t2;
    t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
      xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
      $550C7DC3D5FFB4E2 + W[11];
    t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
      xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
    a := a + t1;
    E := t1 + t2;
    t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
      xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
      $72BE5D74F27B896F + W[12];
    t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
      xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
    H := H + t1;
    d := t1 + t2;
    t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
      xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
      $80DEB1FE3B1696B1 + W[13];
    t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
      xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
    G := G + t1;
    c := t1 + t2;
    t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
      xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
      $9BDC06A725C71235 + W[14];
    t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
      xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
    F := F + t1;
    b := t1 + t2;
    t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
      xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
      $C19BF174CF692694 + W[15];
    t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
      xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
    E := E + t1;
    a := t1 + t2;
    t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
      xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
      $E49B69C19EF14AD2 + W[16];
    t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
      xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
    d := d + t1;
    H := t1 + t2;
    t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
      xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
      $EFBE4786384F25E3 + W[17];
    t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
      xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
    c := c + t1;
    G := t1 + t2;
    t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
      xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
      $0FC19DC68B8CD5B5 + W[18];
    t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
      xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
    b := b + t1;
    F := t1 + t2;
    t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
      xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
      $240CA1CC77AC9C65 + W[19];
    t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
      xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
    a := a + t1;
    E := t1 + t2;
    t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
      xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
      $2DE92C6F592B0275 + W[20];
    t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
      xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
    H := H + t1;
    d := t1 + t2;
    t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
      xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
      $4A7484AA6EA6E483 + W[21];
    t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
      xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
    G := G + t1;
    c := t1 + t2;
    t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
      xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
      $5CB0A9DCBD41FBD4 + W[22];
    t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
      xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
    F := F + t1;
    b := t1 + t2;
    t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
      xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
      $76F988DA831153B5 + W[23];
    t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
      xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
    E := E + t1;
    a := t1 + t2;
    t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
      xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
      $983E5152EE66DFAB + W[24];
    t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
      xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
    d := d + t1;
    H := t1 + t2;
    t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
      xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
      $A831C66D2DB43210 + W[25];
    t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
      xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
    c := c + t1;
    G := t1 + t2;
    t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
      xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
      $B00327C898FB213F + W[26];
    t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
      xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
    b := b + t1;
    F := t1 + t2;
    t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
      xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
      $BF597FC7BEEF0EE4 + W[27];
    t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
      xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
    a := a + t1;
    E := t1 + t2;
    t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
      xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
      $C6E00BF33DA88FC2 + W[28];
    t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
      xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
    H := H + t1;
    d := t1 + t2;
    t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
      xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
      $D5A79147930AA725 + W[29];
    t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
      xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
    G := G + t1;
    c := t1 + t2;
    t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
      xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
      $06CA6351E003826F + W[30];
    t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
      xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
    F := F + t1;
    b := t1 + t2;
    t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
      xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
      $142929670A0E6E70 + W[31];
    t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
      xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
    E := E + t1;
    a := t1 + t2;
    t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
      xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
      $27B70A8546D22FFC + W[32];
    t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
      xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
    d := d + t1;
    H := t1 + t2;
    t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
      xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
      $2E1B21385C26C926 + W[33];
    t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
      xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
    c := c + t1;
    G := t1 + t2;
    t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
      xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
      $4D2C6DFC5AC42AED + W[34];
    t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
      xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
    b := b + t1;
    F := t1 + t2;
    t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
      xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
      $53380D139D95B3DF + W[35];
    t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
      xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
    a := a + t1;
    E := t1 + t2;
    t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
      xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
      $650A73548BAF63DE + W[36];
    t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
      xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
    H := H + t1;
    d := t1 + t2;
    t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
      xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
      $766A0ABB3C77B2A8 + W[37];
    t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
      xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
    G := G + t1;
    c := t1 + t2;
    t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
      xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
      $81C2C92E47EDAEE6 + W[38];
    t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
      xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
    F := F + t1;
    b := t1 + t2;
    t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
      xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
      $92722C851482353B + W[39];
    t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
      xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
    E := E + t1;
    a := t1 + t2;
    t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
      xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
      $A2BFE8A14CF10364 + W[40];
    t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
      xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
    d := d + t1;
    H := t1 + t2;
    t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
      xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
      $A81A664BBC423001 + W[41];
    t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
      xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
    c := c + t1;
    G := t1 + t2;
    t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
      xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
      $C24B8B70D0F89791 + W[42];
    t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
      xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
    b := b + t1;
    F := t1 + t2;
    t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
      xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
      $C76C51A30654BE30 + W[43];
    t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
      xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
    a := a + t1;
    E := t1 + t2;
    t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
      xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
      $D192E819D6EF5218 + W[44];
    t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
      xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
    H := H + t1;
    d := t1 + t2;
    t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
      xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
      $D69906245565A910 + W[45];
    t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
      xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
    G := G + t1;
    c := t1 + t2;
    t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
      xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
      $F40E35855771202A + W[46];
    t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
      xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
    F := F + t1;
    b := t1 + t2;
    t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
      xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
      $106AA07032BBD1B8 + W[47];
    t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
      xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
    E := E + t1;
    a := t1 + t2;
    t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
      xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
      $19A4C116B8D2D0C8 + W[48];
    t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
      xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
    d := d + t1;
    H := t1 + t2;
    t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
      xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
      $1E376C085141AB53 + W[49];
    t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
      xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
    c := c + t1;
    G := t1 + t2;
    t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
      xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
      $2748774CDF8EEB99 + W[50];
    t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
      xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
    b := b + t1;
    F := t1 + t2;
    t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
      xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
      $34B0BCB5E19B48A8 + W[51];
    t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
      xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
    a := a + t1;
    E := t1 + t2;
    t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
      xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
      $391C0CB3C5C95A63 + W[52];
    t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
      xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
    H := H + t1;
    d := t1 + t2;
    t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
      xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
      $4ED8AA4AE3418ACB + W[53];
    t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
      xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
    G := G + t1;
    c := t1 + t2;
    t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
      xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
      $5B9CCA4F7763E373 + W[54];
    t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
      xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
    F := F + t1;
    b := t1 + t2;
    t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
      xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
      $682E6FF3D6B2B8A3 + W[55];
    t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
      xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
    E := E + t1;
    a := t1 + t2;
    t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
      xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
      $748F82EE5DEFB2FC + W[56];
    t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
      xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
    d := d + t1;
    H := t1 + t2;
    t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
      xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
      $78A5636F43172F60 + W[57];
    t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
      xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
    c := c + t1;
    G := t1 + t2;
    t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
      xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
      $84C87814A1F0AB72 + W[58];
    t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
      xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
    b := b + t1;
    F := t1 + t2;
    t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
      xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
      $8CC702081A6439EC + W[59];
    t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
      xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
    a := a + t1;
    E := t1 + t2;
    t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
      xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
      $90BEFFFA23631E28 + W[60];
    t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
      xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
    H := H + t1;
    d := t1 + t2;
    t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
      xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
      $A4506CEBDE82BDE9 + W[61];
    t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
      xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
    G := G + t1;
    c := t1 + t2;
    t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
      xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
      $BEF9A3F7B2C67915 + W[62];
    t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
      xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
    F := F + t1;
    b := t1 + t2;
    t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
      xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
      $C67178F2E372532B + W[63];
    t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
      xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
    E := E + t1;
    a := t1 + t2;
    t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
      xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
      $CA273ECEEA26619C + W[64];
    t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
      xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
    d := d + t1;
    H := t1 + t2;
    t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
      xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
      $D186B8C721C0C207 + W[65];
    t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
      xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
    c := c + t1;
    G := t1 + t2;
    t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
      xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
      $EADA7DD6CDE0EB1E + W[66];
    t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
      xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
    b := b + t1;
    F := t1 + t2;
    t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
      xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
      $F57D4F7FEE6ED178 + W[67];
    t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
      xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
    a := a + t1;
    E := t1 + t2;
    t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
      xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
      $06F067AA72176FBA + W[68];
    t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
      xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
    H := H + t1;
    d := t1 + t2;
    t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
      xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
      $0A637DC5A2C898A6 + W[69];
    t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
      xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
    G := G + t1;
    c := t1 + t2;
    t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
      xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
      $113F9804BEF90DAE + W[70];
    t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
      xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
    F := F + t1;
    b := t1 + t2;
    t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
      xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
      $1B710B35131C471B + W[71];
    t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
      xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
    E := E + t1;
    a := t1 + t2;
    t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
      xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
      $28DB77F523047D84 + W[72];
    t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
      xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
    d := d + t1;
    H := t1 + t2;
    t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
      xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
      $32CAAB7B40C72493 + W[73];
    t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
      xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
    c := c + t1;
    G := t1 + t2;
    t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
      xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
      $3C9EBE0A15C9BEBC + W[74];
    t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
      xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
    b := b + t1;
    F := t1 + t2;
    t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
      xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
      $431D67C49C100D4C + W[75];
    t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
      xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
    a := a + t1;
    E := t1 + t2;
    t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
      xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
      $4CC5D4BECB3E42B6 + W[76];
    t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
      xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
    H := H + t1;
    d := t1 + t2;
    t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
      xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
      $597F299CFC657E2A + W[77];
    t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
      xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
    G := G + t1;
    c := t1 + t2;
    t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
      xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
      $5FCB6FAB3AD6FAEC + W[78];
    t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
      xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
    F := F + t1;
    b := t1 + t2;
    t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
      xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
      $6C44198C4A475817 + W[79];
    t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
      xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
    E := E + t1;
    a := t1 + t2;
    AContext.CurrentHash.I64[0] := AContext.CurrentHash.I64[0] + a;
    AContext.CurrentHash.I64[1] := AContext.CurrentHash.I64[1] + b;
    AContext.CurrentHash.I64[2] := AContext.CurrentHash.I64[2] + c;
    AContext.CurrentHash.I64[3] := AContext.CurrentHash.I64[3] + d;
    AContext.CurrentHash.I64[4] := AContext.CurrentHash.I64[4] + E;
    AContext.CurrentHash.I64[5] := AContext.CurrentHash.I64[5] + F;
    AContext.CurrentHash.I64[6] := AContext.CurrentHash.I64[6] + G;
    AContext.CurrentHash.I64[7] := AContext.CurrentHash.I64[7] + H;
    FillChar(AContext.HashBuffer, 128, 0);
  end;
  procedure SHA512Compress;
  begin
    SHA384Compress;
  end;

begin
  case AContext.CurrentHash.HashType of
    sdt160:
      SHA160Compress;
    sdt256:
      SHA256Compress;
    sdt384:
      SHA384Compress;
    sdt512:
      SHA512Compress;
  end;
end;
{$WARNINGS ON}

procedure SHAUpdate(var AContext: TSHAContext; const p: Pointer;
  ASize: LongWord);
var
  pBuf: PByte;
  procedure SHA160Update;
  begin
    Inc(AContext.LenHi, ASize shr 29);
    Inc(AContext.LenLo, ASize shl 3);
    if AContext.LenLo < (ASize shl 3) then
      Inc(AContext.LenHi);
    pBuf := p;
    while ASize > 0 do
    begin
      if (64 - AContext.Index) <= ASize then
      begin
        Move(pBuf^, AContext.HashBuffer[AContext.Index], 64 - AContext.Index);
        Dec(ASize, 64 - AContext.Index);
        Inc(pBuf, 64 - AContext.Index);
        SHACompress(AContext);
      end
      else
      begin
        Move(pBuf^, AContext.HashBuffer[AContext.Index], ASize);
        Inc(AContext.Index, ASize);
        ASize := 0;
      end;
    end;
  end;
  procedure SHA256Update;
  begin
    Inc(AContext.LenHi, ASize shr 29);
    Inc(AContext.LenLo, ASize shl 3);
    if AContext.LenLo < (ASize shl 3) then
      Inc(AContext.LenHi);
    pBuf := p;
    while ASize > 0 do
    begin
      if (64 - AContext.Index) <= ASize then
      begin
        Move(pBuf^, AContext.HashBuffer[AContext.Index], 64 - AContext.Index);
        Dec(ASize, 64 - AContext.Index);
        Inc(pBuf, 64 - AContext.Index);
        SHACompress(AContext);
      end
      else
      begin
        Move(pBuf^, AContext.HashBuffer[AContext.Index], ASize);
        Inc(AContext.Index, ASize);
        ASize := 0;
      end;
    end;
  end;
  procedure SHA384Update;
  begin
    Inc(AContext.LenLo64, ASize * 8);
    if AContext.LenLo64 < (ASize * 8) then
      Inc(AContext.LenHi64);
    pBuf := p;
    while ASize > 0 do
    begin
      if (128 - AContext.Index) <= ASize then
      begin
        Move(pBuf^, AContext.HashBuffer[AContext.Index], 128 - AContext.Index);
        Dec(ASize, 128 - AContext.Index);
        Inc(pBuf, 128 - AContext.Index);
        SHACompress(AContext);
      end
      else
      begin
        Move(pBuf^, AContext.HashBuffer[AContext.Index], ASize);
        Inc(AContext.Index, ASize);
        ASize := 0;
      end;
    end;
  end;

  procedure SHA512Update;
  begin
    SHA384Update;
  end;

begin
  case AContext.CurrentHash.HashType of
    sdt160:
      SHA160Update;
    sdt256:
      SHA256Update;
    sdt384:
      SHA384Update;
    sdt512:
      SHA512Update;
  end;
end;

procedure SHAFinal(var AContext: TSHAContext);
  procedure SHA160Final;
  begin
    AContext.HashBuffer[AContext.Index] := $80;
    if AContext.Index >= 56 then
      SHACompress(AContext);
    PLongWord(@AContext.HashBuffer[56])^ := SwapDWord(AContext.LenHi);
    PLongWord(@AContext.HashBuffer[60])^ := SwapDWord(AContext.LenLo);
    SHACompress(AContext);
    AContext.CurrentHash.I32[0] := SwapDWord(AContext.CurrentHash.I32[0]);
    AContext.CurrentHash.I32[1] := SwapDWord(AContext.CurrentHash.I32[1]);
    AContext.CurrentHash.I32[2] := SwapDWord(AContext.CurrentHash.I32[2]);
    AContext.CurrentHash.I32[3] := SwapDWord(AContext.CurrentHash.I32[3]);
    AContext.CurrentHash.I32[4] := SwapDWord(AContext.CurrentHash.I32[4]);
  end;
  procedure SHA256Final;
  begin
    AContext.HashBuffer[AContext.Index] := $80;
    if AContext.Index >= 56 then
      SHACompress(AContext);
    PLongWord(@AContext.HashBuffer[56])^ := SwapDWord(AContext.LenHi);
    PLongWord(@AContext.HashBuffer[60])^ := SwapDWord(AContext.LenLo);
    SHACompress(AContext);
    AContext.CurrentHash.I32[0] := SwapDWord(AContext.CurrentHash.I32[0]);
    AContext.CurrentHash.I32[1] := SwapDWord(AContext.CurrentHash.I32[1]);
    AContext.CurrentHash.I32[2] := SwapDWord(AContext.CurrentHash.I32[2]);
    AContext.CurrentHash.I32[3] := SwapDWord(AContext.CurrentHash.I32[3]);
    AContext.CurrentHash.I32[4] := SwapDWord(AContext.CurrentHash.I32[4]);
    AContext.CurrentHash.I32[5] := SwapDWord(AContext.CurrentHash.I32[5]);
    AContext.CurrentHash.I32[6] := SwapDWord(AContext.CurrentHash.I32[6]);
    AContext.CurrentHash.I32[7] := SwapDWord(AContext.CurrentHash.I32[7]);
  end;

  procedure SHA384Final;
  begin
    AContext.HashBuffer[AContext.Index] := $80;
    if AContext.Index >= 112 then
      SHACompress(AContext);
    PInt64(@AContext.HashBuffer[112])^ := SwapQWord(AContext.LenHi64);
    PInt64(@AContext.HashBuffer[120])^ := SwapQWord(AContext.LenLo64);
    SHACompress(AContext);
    AContext.CurrentHash.I64[0] := SwapQWord(AContext.CurrentHash.I64[0]);
    AContext.CurrentHash.I64[1] := SwapQWord(AContext.CurrentHash.I64[1]);
    AContext.CurrentHash.I64[2] := SwapQWord(AContext.CurrentHash.I64[2]);
    AContext.CurrentHash.I64[3] := SwapQWord(AContext.CurrentHash.I64[3]);
    AContext.CurrentHash.I64[4] := SwapQWord(AContext.CurrentHash.I64[4]);
    AContext.CurrentHash.I64[5] := SwapQWord(AContext.CurrentHash.I64[5]);
  end;
  procedure SHA512Final;
  begin
    AContext.HashBuffer[AContext.Index] := $80;
    if AContext.Index >= 112 then
      SHACompress(AContext);
    PInt64(@AContext.HashBuffer[112])^ := SwapQWord(AContext.LenHi64);
    PInt64(@AContext.HashBuffer[120])^ := SwapQWord(AContext.LenLo64);
    SHACompress(AContext);
    AContext.CurrentHash.I64[0] := SwapQWord(AContext.CurrentHash.I64[0]);
    AContext.CurrentHash.I64[1] := SwapQWord(AContext.CurrentHash.I64[1]);
    AContext.CurrentHash.I64[2] := SwapQWord(AContext.CurrentHash.I64[2]);
    AContext.CurrentHash.I64[3] := SwapQWord(AContext.CurrentHash.I64[3]);
    AContext.CurrentHash.I64[4] := SwapQWord(AContext.CurrentHash.I64[4]);
    AContext.CurrentHash.I64[5] := SwapQWord(AContext.CurrentHash.I64[5]);
    AContext.CurrentHash.I64[6] := SwapQWord(AContext.CurrentHash.I64[6]);
    AContext.CurrentHash.I64[7] := SwapQWord(AContext.CurrentHash.I64[7]);
  end;

begin
  case AContext.CurrentHash.HashType of
    sdt160:
      SHA160Final;
    sdt256:
      SHA256Final;
    sdt384:
      SHA384Final;
    sdt512:
      SHA512Final;
  end;
end;

procedure SHAHash(var ADigest: TSHADigest; const p: Pointer; len: Integer;
  AType: TSHADigestType); {$IFDEF USEINLINE}inline;{$ENDIF}
var
  AContext: TSHAContext;
begin
  SHAInit(AContext, AType);
  SHAUpdate(AContext, p, len);
  SHAFinal(AContext);
  ADigest := AContext.CurrentHash;
end;

function SHA160Hash(const p: Pointer; len: Integer): TSHADigest;
begin
  SHAHash(Result, p, len, sdt160);
end;

procedure SHAHashString(const S: string; AType: TSHADigestType;
  var ADigest: TSHADigest);
{$IFDEF UNICODE}
var
  LValue: TBytes;
{$ENDIF}
begin
  if S <> '' then begin
    {$IFDEF UNICODE}
    LValue := AnsiEncode(S);
    SHAHash(ADigest, @LValue[0], Length(LValue), AType);
    {$ELSE}
    SHAHash(ADigest, Pointer(S), Length(S), AType);
    {$ENDIF}
  end else
    ADigest.HashType := TSHADigestType(-1);
end;

procedure SHAHashStream(AStream: TStream; AType: TSHADigestType;
  var ADigest: TSHADigest; AOnProgress: THashProgressNotify);
var
  AContext: TSHAContext;
  ABuf: array [0 .. 65535] of Byte;
  AReaded: Integer;
  ALastNotify: Int64;
  procedure DoProgress(AForce: Boolean);
  begin
    if (AStream.Position - ALastNotify > 10485760) or AForce then
    begin
      if TMethod(AOnProgress).Code <> nil then
      begin
{$IFDEF UNICODE}
        if TMethod(AOnProgress).Data = Pointer(-1) then
          THashProgressNotifyA(TMethod(AOnProgress).Code)
            (AStream.Position, AStream.Size)
        else
{$ENDIF}
          AOnProgress(AStream.Position, AStream.Size);
      end;
      ALastNotify := AStream.Position;
    end;
  end;

begin
  SHAInit(AContext, AType);
  AStream.Position := 0;
  ALastNotify := 0;
  repeat
    AReaded := AStream.Read(ABuf, 65536);
    if AReaded > 0 then
      SHAUpdate(AContext, @ABuf[0], AReaded);
    DoProgress(False);
  until AReaded = 0;
  SHAFinal(AContext);
  DoProgress(True);
  ADigest := AContext.CurrentHash;
end;

procedure SHAHashFile(const AFile: string; AType: TSHADigestType;
  var ADigest: TSHADigest; AOnProgress: THashProgressNotify);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFile, fmOpenRead or fmShareDenyWrite);
  try
    SHAHashStream(AStream, AType, ADigest, AOnProgress);
  finally
    FreeAndNil(AStream);
  end;
end;

function SHA160Hash(const S: string): TSHADigest;
begin
  SHAHashString(S, sdt160, Result);
end;

function SHA160Hash(AStream: TStream; AOnProgress: THashProgressNotify): TSHADigest;
begin
  SHAHashStream(AStream, sdt160, Result, AOnProgress);
end;

function SHA160File(AFileName: string;
  AOnProgress: THashProgressNotify = nil): TSHADigest;
begin
  SHAHashFile(AFileName, sdt160, Result, AOnProgress);
end;

{$IFDEF UNICODE}
function SHA160Hash(AStream: TStream; AOnProgress: THashProgressNotifyA): TSHADigest; overload;
var
  AEvent: THashProgressNotify;
begin
  if Assigned(AOnProgress) then begin
    AEvent := nil;
    try
      PHashProgressNotifyA(@TMethod(AEvent).Code)^ := AOnProgress;
      TMethod(AEvent).Data := Pointer(-1);
      Result := SHA160Hash(AStream, AEvent);
    finally
      THashProgressNotifyA(TMethod(AEvent).Code) := nil;
    end;
  end else
    Result := SHA160Hash(AStream, THashProgressNotify(nil));
end;

function SHA160File(const AFileName: string; AOnProgress: THashProgressNotifyA)  : TSHADigest; overload;
var
  AEvent: THashProgressNotify;
begin
  if Assigned(AOnProgress) then begin
    AEvent := nil;
    try
      PHashProgressNotifyA(@TMethod(AEvent).Code)^ := AOnProgress;
      TMethod(AEvent).Data := Pointer(-1);
      Result := SHA160File(AFileName, AEvent);
    finally
      THashProgressNotifyA(TMethod(AEvent).Code) := nil;
    end;
  end else
    Result := SHA160File(AFileName, nil);
end;
{$ENDIF}

function SHA256Hash(const p: Pointer; len: Integer): TSHADigest;
begin
  SHAHash(Result, p, len, sdt256);
end;

function SHA256Hash(const S: string): TSHADigest;
begin
  SHAHashString(S, sdt256, Result);
end;

function SHA256Hash(AStream: TStream; AOnProgress: THashProgressNotify)
  : TSHADigest;
begin
  SHAHashStream(AStream, sdt256, Result, AOnProgress);
end;

function SHA256File(const AFileName: string; AOnProgress: THashProgressNotify): TSHADigest;
begin
  SHAHashFile(AFileName, sdt256, Result, AOnProgress);
end;

{$IFDEF UNICODE}
function SHA256Hash(AStream: TStream; AOnProgress: THashProgressNotifyA): TSHADigest; overload;
var
  AEvent: THashProgressNotify;
begin
  if Assigned(AOnProgress) then begin
    AEvent := nil;
    try
      PHashProgressNotifyA(@TMethod(AEvent).Code)^ := AOnProgress;
      TMethod(AEvent).Data := Pointer(-1);
      Result := SHA256Hash(AStream, AEvent);
    finally
      THashProgressNotifyA(TMethod(AEvent).Code) := nil;
    end;
  end else
    Result := SHA256Hash(AStream, THashProgressNotify(nil));
end;

function SHA256File(const AFileName: string; AOnProgress: THashProgressNotifyA): TSHADigest; overload;
var
  AEvent: THashProgressNotify;
begin
  if Assigned(AOnProgress) then begin
    AEvent := nil;
    try
      PHashProgressNotifyA(@TMethod(AEvent).Code)^ := AOnProgress;
      TMethod(AEvent).Data := Pointer(-1);
      Result := SHA256File(AFileName, AEvent);
    finally
      THashProgressNotifyA(TMethod(AEvent).Code) := nil;
    end;
  end else
    Result := SHA256File(AFileName, nil);
end;
{$ENDIF}

function SHA384Hash(const p: Pointer; len: Integer): TSHADigest;
begin
  SHAHash(Result, p, len, sdt384);
end;

function SHA384Hash(const S: string): TSHADigest;
begin
  SHAHashString(S, sdt384, Result);
end;

function SHA384Hash(AStream: TStream; AOnProgress: THashProgressNotify): TSHADigest;
begin
  SHAHashStream(AStream, sdt384, Result, AOnProgress);
end;

function SHA384File(const AFileName: string; AOnProgress: THashProgressNotify): TSHADigest;
begin
  SHAHashFile(AFileName, sdt384, Result, AOnProgress);
end;

{$IFDEF UNICODE}
function SHA384Hash(AStream: TStream; AOnProgress: THashProgressNotifyA): TSHADigest; overload;
var
  AEvent: THashProgressNotify;
begin
  if Assigned(AOnProgress) then begin
    AEvent := nil;
    try
      PHashProgressNotifyA(@TMethod(AEvent).Code)^ := AOnProgress;
      TMethod(AEvent).Data := Pointer(-1);
      Result := SHA384Hash(AStream, AEvent);
    finally
      THashProgressNotifyA(TMethod(AEvent).Code) := nil;
    end;
  end else
    Result := SHA384Hash(AStream, THashProgressNotify(nil));
end;

function SHA384File(const AFileName: string; AOnProgress: THashProgressNotifyA): TSHADigest; overload;
var
  AEvent: THashProgressNotify;
begin
  if Assigned(AOnProgress) then begin
    AEvent := nil;
    try
      PHashProgressNotifyA(@TMethod(AEvent).Code)^ := AOnProgress;
      TMethod(AEvent).Data := Pointer(-1);
      Result := SHA384File(AFileName, AEvent);
    finally
      THashProgressNotifyA(TMethod(AEvent).Code) := nil;
    end;
  end else
    Result := SHA384File(AFileName, THashProgressNotify(nil));
end;
{$ENDIF}

function SHA512Hash(const p: Pointer; len: Integer): TSHADigest;
begin
  SHAHash(Result, p, len, sdt512);
end;

function SHA512Hash(const S: string): TSHADigest;
begin
  SHAHashString(S, sdt512, Result);
end;

function SHA512Hash(AStream: TStream; AOnProgress: THashProgressNotify): TSHADigest;
begin
  SHAHashStream(AStream, sdt512, Result, AOnProgress);
end;

function SHA512File(const AFileName: string; AOnProgress: THashProgressNotify): TSHADigest;
begin
  SHAHashFile(AFileName, sdt512, Result, AOnProgress);
end;

{$IFDEF UNICODE}
function SHA512Hash(AStream: TStream; AOnProgress: THashProgressNotifyA): TSHADigest; overload;
var
  AEvent: THashProgressNotify;
begin
  if Assigned(AOnProgress) then begin
    AEvent := nil;
    try
      PHashProgressNotifyA(@TMethod(AEvent).Code)^ := AOnProgress;
      TMethod(AEvent).Data := Pointer(-1);
      Result := SHA512Hash(AStream, AEvent);
    finally
      THashProgressNotifyA(TMethod(AEvent).Code) := nil;
    end;
  end else
    Result := SHA512Hash(AStream, THashProgressNotify(nil));
end;

function SHA512File(const AFileName: string; AOnProgress: THashProgressNotifyA): TSHADigest; overload;
var
  AEvent: THashProgressNotify;
begin
  if Assigned(AOnProgress) then begin
    AEvent := nil;
    try
      PHashProgressNotifyA(@TMethod(AEvent).Code)^ := AOnProgress;
      TMethod(AEvent).Data := Pointer(-1);
      Result := SHA512File(AFileName, AEvent);
    finally
      THashProgressNotifyA(TMethod(AEvent).Code) := nil;
    end;
  end else
    Result := SHA512File(AFileName, THashProgressNotify(nil));
end;
{$ENDIF}

function DigestToString(const ADigest: TSHADigest): string;
begin
  case ADigest.HashType of
    sdt160:
      Result := ToHexBytes(@ADigest.SHA160[0], 20);
    sdt256:
      Result := ToHexBytes(@ADigest.SHA256[0], 32);
    sdt384:
      Result := ToHexBytes(@ADigest.SHA256[0], 48);
    sdt512:
      Result := ToHexBytes(@ADigest.SHA256[0], 64)
  else
    SetLength(Result, 0);
  end;
end;

initialization

end.
