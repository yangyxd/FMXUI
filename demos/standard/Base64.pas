unit Base64;

interface

{$IF RTLVersion>=24}
{$LEGACYIFEND ON}
{$IFEND}

uses
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  SysUtils, Classes;

{$IFNDEF UNICODE}
type
  RawByteString = AnsiString;
{$ENDIF}
{$IF CompilerVersion = 30}
{$IFDEF NEXTGEN}
type
  RawByteString = MarshaledAString;
{$ENDIF}
{$IFEND}

const
  kCharsPerLine = 76;

  DecodeTable: array[0..79] of ShortInt = (
    62,  -1,  -1,  -1,  63,  52,  53,  54,  55,  56,  57, 58, 59, 60, 61, -1,
    -1,  -1,  -2,  -1,  -1,  -1,   0,   1,   2,   3,   4,  5,  6,  7,  8,  9,
    10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20, 21, 22, 23, 24, 25,
    -1,  -1,  -1,  -1,  -1,  -1,  26,  27,  28,  29,  30, 31, 32, 33, 34, 35,
    36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46, 47, 48, 49, 50, 51);

  EncodeTable: array[0..63] of Byte = (
    Ord('A'),Ord('B'),Ord('C'),Ord('D'),Ord('E'),Ord('F'),Ord('G'),Ord('H'),Ord('I'),Ord('J'),Ord('K'),Ord('L'),Ord('M'),
    Ord('N'),Ord('O'),Ord('P'),Ord('Q'),Ord('R'),Ord('S'),Ord('T'),Ord('U'),Ord('V'),Ord('W'),Ord('X'),Ord('Y'),Ord('Z'),
    Ord('a'),Ord('b'),Ord('c'),Ord('d'),Ord('e'),Ord('f'),Ord('g'),Ord('h'),Ord('i'),Ord('j'),Ord('k'),Ord('l'),Ord('m'),
    Ord('n'),Ord('o'),Ord('p'),Ord('q'),Ord('r'),Ord('s'),Ord('t'),Ord('u'),Ord('v'),Ord('w'),Ord('x'),Ord('y'),Ord('z'),
    Ord('0'),Ord('1'),Ord('2'),Ord('3'),Ord('4'),Ord('5'),Ord('6'),Ord('7'),Ord('8'),Ord('9'),Ord('+'),Ord('/'));

type
  TEncodeStep = (esEncodeStepA, esEncodeStepB, esEncodeStepC);
  TDecodeStep = (dsDecodeStepA, dsDecodeStepB, dsDecodeStepC, dsDecodeStepD);

  TEncodeState = record
    Step: TEncodeStep;
    Result: Byte;
    StepCount: Integer;
  end;

  TDecodeState = record
    Step: TDecodeStep;
    Result: Byte;
  end;

// 编码

function Base64Encode(const Input, Output: TStream): Int64; overload;
function Base64Encode(const Input: Pointer; ASize: Int64): TBytes; overload;
function Base64Encode(const Input: TBytes): TBytes; overload;
function Base64Encode(const Input: string): TBytes; overload;

function StrToBase64(const Input: Pointer; ASize: Int64): string; overload;
function StrToBase64(const Input: string; ConvToUTF8: Boolean = True): string; overload;
{$IFNDEF WIN32}
function StrToBase64(const Input: RawByteString): string; overload;
{$ENDIF}

// 解码

function Base64Decode(const Input, Output: TStream): Int64; overload;
function Base64Decode(const Input: string): TBytes; overload;
function Base64Decode(const Input: Pointer; ASize: Int64): TBytes; overload;
function Base64Decode(const Input: TBytes): TBytes; overload;

function Base64ToStr(const Input: string; IsUTF8: Boolean = True): string; overload;
function Base64ToStr(const Input: Pointer; ASize: Int64; IsUTF8: Boolean = True): string; overload;
{$IFNDEF WIN32}
function Base64ToStr(const Input: RawByteString; IsUTF8: Boolean = True): string; overload;
{$ENDIF}

implementation

resourcestring
  SBadUnicodeChar = '无效的Unicode字符:%d';

var
  CEncodeState: TEncodeState;
  CDecodeState: TDecodeState;
  // 按照Java格式编码，将#$0字符编码为#$C080
  JavaFormatUtf8: Boolean = True;

type
  {$IFDEF UNICODE}
  StringW = UnicodeString;
  {$ELSE}
  StringW = WideString;
  {$ENDIF}
  {$if CompilerVersion < 23}
  NativeUInt = Cardinal;
  IntPtr = NativeInt;
  {$ifend}
  {$IFDEF NEXTGEN}
  PAnsiChar = PByte;
  AnsiChar = Byte;
  {$ENDIF}

function Utf8Encode(P:PWideChar; l:Integer): RawByteString; overload;
var
  {$IFDEF MSWINDOWS}
  dl: Integer;
  {$ELSE}
  ps: PWideChar;
  pd, pds: PAnsiChar;
  c: Cardinal;
  {$ENDIF}
begin
  if p = nil then
    Result := ''
  else begin
    {$IFDEF MSWINDOWS}
    dl := l * 3;
    SetLength(Result, dl);
    dl := WideCharToMultiByte(CP_UTF8, 0, p, l, PAnsiChar(Result), dl, nil, nil);
    SetLength(Result, dl);
    {$ELSE}
    if l<=0 then begin
      ps:=p;
      while ps^<>#0 do
        Inc(ps);
      l:=ps-p;
    end;
    SetLength(Result, l*3);//UTF8每个字符最多6字节长,一次性分配足够的空间
    if l>0 then begin
      ps := p;
      pd := PAnsiChar(Result);
      pds := pd;
      while l>0 do begin
        c := Cardinal(ps^);
        Inc(ps);
        if (c>=$D800) and (c<=$DFFF) then begin//Unicode 扩展区字符
          c:=(c-$D800);
          if (ps^>=#$DC00) and (ps^<=#$DFFF) then begin
            c:=$10000+((c shl 10) + (Cardinal(ps^)-$DC00));
            Inc(ps);
            Dec(l);
          end else
            raise Exception.Create(Format(SBadUnicodeChar,[IntPtr(ps^)]));
        end;
        Dec(l);
        if c=$0 then begin
          if JavaFormatUtf8 then begin//按照Java格式编码，将#$0字符编码为#$C080
            pd^:={$IFDEF NEXTGEN}$C0{$ELSE}#$C0{$ENDIF};
            Inc(pd);
            pd^:={$IFDEF NEXTGEN}$80{$ELSE}#$80{$ENDIF};
            Inc(pd);
          end else begin
            pd^ := AnsiChar(c);
            Inc(pd);
          end;
        end else if c<=$7F then begin //1B
          pd^:=AnsiChar(c);
          Inc(pd);
        end else if c<=$7FF then begin//$80-$7FF,2B
          pd^:=AnsiChar($C0 or (c shr 6));
          Inc(pd);
          pd^:=AnsiChar($80 or (c and $3F));
          Inc(pd);
        end else if c<=$FFFF then begin //$8000 - $FFFF,3B
          pd^:=AnsiChar($E0 or (c shr 12));
          Inc(pd);
          pd^:=AnsiChar($80 or ((c shr 6) and $3F));
          Inc(pd);
          pd^:=AnsiChar($80 or (c and $3F));
          Inc(pd);
        end else if c<=$1FFFFF then begin //$01 0000-$1F FFFF,4B
          pd^:=AnsiChar($F0 or (c shr 18));//1111 0xxx
          Inc(pd);
          pd^:=AnsiChar($80 or ((c shr 12) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or ((c shr 6) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or (c and $3F));//10 xxxxxx
          Inc(pd);
        end else if c<=$3FFFFFF then begin//$20 0000 - $3FF FFFF,5B
          pd^:=AnsiChar($F8 or (c shr 24));//1111 10xx
          Inc(pd);
          pd^:=AnsiChar($F0 or ((c shr 18) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or ((c shr 12) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or ((c shr 6) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or (c and $3F));//10 xxxxxx
          Inc(pd);
        end else if c<=$7FFFFFFF then begin //$0400 0000-$7FFF FFFF,6B
          pd^:=AnsiChar($FC or (c shr 30));//1111 11xx
          Inc(pd);
          pd^:=AnsiChar($F8 or ((c shr 24) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($F0 or ((c shr 18) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or ((c shr 12) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or ((c shr 6) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or (c and $3F));//10 xxxxxx
          Inc(pd);
        end;
      end;
      pd^:={$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF};
      SetLength(Result, IntPtr(pd)-IntPtr(pds));
    end;
    {$ENDIF}
  end;
end;

function Utf8Encode(const P: StringW): RawByteString; overload;
begin
  Result := Base64.Utf8Encode(PWideChar(p), Length(p));
end;

function Utf8Decode(P: PByte; l: Integer): StringW;

  function _UTF8Decode(p: PAnsiChar; l: Integer): StringW;
  var
    ps,pe: PByte;
    pd,pds: PWord;
    c: Cardinal;
  begin
    ps := PByte(p);
    pe := ps;
    Inc(pe, l);
    System.SetLength(Result, l);
    pd := PWord(PWideChar(Result));
    pds := pd;
    while Integer(ps)<Integer(pe) do begin
      if (ps^ and $80)<>0 then begin
        if (ps^ and $FC)=$FC then begin //4000000+
          c:=(ps^ and $03) shl 30;
          Inc(ps);
          c:=c or ((ps^ and $3F) shl 24);
          Inc(ps);
          c:=c or ((ps^ and $3F) shl 18);
          Inc(ps);
          c:=c or ((ps^ and $3F) shl 12);
          Inc(ps);
          c:=c or ((ps^ and $3F) shl 6);
          Inc(ps);
          c:=c or (ps^ and $3F);
          Inc(ps);
          c:=c-$10000;
          pd^:=$D800+((c shr 10) and $3FF);
          Inc(pd);
          pd^:=$DC00+(c and $3FF);
          Inc(pd);
        end else if (ps^ and $F8)=$F8 then begin //200000-3FFFFFF
          c:=(ps^ and $07) shl 24;
          Inc(ps);
          c:=c or ((ps^ and $3F) shl 18);
          Inc(ps);
          c:=c or ((ps^ and $3F) shl 12);
          Inc(ps);
          c:=c or ((ps^ and $3F) shl 6);
          Inc(ps);
          c:=c or (ps^ and $3F);
          Inc(ps);
          c:=c-$10000;
          pd^:=$D800+((c shr 10) and $3FF);
          Inc(pd);
          pd^:=$DC00+(c and $3FF);
          Inc(pd);
        end else if (ps^ and $F0)=$F0 then begin //10000-1FFFFF
          c:=(ps^ and $0F) shr 18;
          Inc(ps);
          c:=c or ((ps^ and $3F) shl 12);
          Inc(ps);
          c:=c or ((ps^ and $3F) shl 6);
          Inc(ps);
          c:=c or (ps^ and $3F);
          Inc(ps);
          c:=c-$10000;
          pd^:=$D800+((c shr 10) and $3FF);
          Inc(pd);
          pd^:=$DC00+(c and $3FF);
          Inc(pd);
        end else if (ps^ and $E0)=$E0 then begin //800-FFFF
          c:=(ps^ and $1F) shl 12;
          Inc(ps);
          c:=c or ((ps^ and $3F) shl 6);
          Inc(ps);
          c:=c or (ps^ and $3F);
          Inc(ps);
          pd^:=c;
          Inc(pd);
        end else if (ps^ and $C0)=$C0 then begin //80-7FF
          pd^:=(ps^ and $3F) shl 6;
          Inc(ps);
          pd^:=pd^ or (ps^ and $3F);
          Inc(pd);
          Inc(ps);
        end else
          raise Exception.Create(Format('无效的UTF8字符:%d',[Integer(ps^)]));
      end else begin
        pd^ := ps^;
        Inc(ps);
        Inc(pd);
      end;
    end;
    System.SetLength(Result, (Integer(pd)-Integer(pds)) shr 1);
  end;

var
  ps: PByte;
begin
  if l<=0 then begin
    ps := PByte(p);
    while ps^<>0 do Inc(ps);
    l := Integer(ps) - Integer(p);
  end else
    ps := PByte(P);
  {$IFDEF MSWINDOWS}
  SetLength(Result, l);
  SetLength(Result, MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(p), l, PWideChar(Result), l));
  if Length(Result) = 0 then
    Result := _UTF8Decode(Pointer(ps), l);
  {$ELSE}
  Result := _UTF8Decode(Pointer(ps), l);
  {$ENDIF}
end;

function AnsiDecode(p: PAnsiChar; l:Integer): StringW;
var
  ps: PAnsiChar;
{$IFNDEF MSWINDOWS}
  ABytes:TBytes;
{$ENDIF}
begin
  if l<=0 then begin
    ps := p;
    while ps^<>{$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} do Inc(ps);
    l := IntPtr(ps) - IntPtr(p);
  end;
  if l>0 then begin
    {$IFDEF MSWINDOWS}
    System.SetLength(Result, MultiByteToWideChar(CP_ACP,0,PAnsiChar(p),l,nil,0));
    MultiByteToWideChar(CP_ACP, 0, PAnsiChar(p),l,PWideChar(Result),Length(Result));
    {$ELSE}
    System.SetLength(ABytes, l);
    Move(p^, PByte(@ABytes[0])^, l);
    {$IFDEF NEXTGEN}
    // 在移动平台, RawByteString 默认的代码页是 CP_UTF8
    Result := TEncoding.GetEncoding(CP_UTF8).GetString(ABytes, 0, l);
    {$ELSE}
    Result := TEncoding.Default.GetString(ABytes, 0, l);
    {$ENDIF}
    {$ENDIF}
  end else
    System.SetLength(Result,0);
end;

function EstimateDecodeLength(const InputLength: Int64): Int64;
begin
  Result := (InputLength shr 2) * 3 + 4;
end;

function EstimateEncodeLength(const InputLength: Int64): Int64;
begin
  {$IFNDEF WIN32}
  Result := InputLength div 3 shl 2 + 4;
  {$ELSE}
  Result := ((InputLength + 2) div 3) shl 2;
  {$ENDIF}
end;

procedure InitDecodeState(var State: TDecodeState);
begin
  State.Step := dsDecodeStepA;
  State.Result := 0;
end;

procedure InitEncodeState(var State: TEncodeState);
begin
  State.Step := esEncodeStepA;
  State.Result := 0;
  State.StepCount := 0;
end;

function DecodeValue(const Code: Byte): Integer; inline;
var
  LCode: Integer;
begin
  LCode := Code - 43;
  if (LCode < Low(DecodeTable)) or (LCode > High(DecodeTable)) then
    Result := -1
  else
    Result := DecodeTable[LCode];
end;

{$IFNDEF WIN32}
function EncodeBytes(Input, Output: PByte; InputLen: Integer; var State: TEncodeState;
  const CharSize: SmallInt): Integer;
var
  B, C: Byte;
  P, PEnd, POut: PByte;
begin
  P := Input;
  PEnd := Pointer(IntPtr(P) + InputLen);
  POut := Output;
  C := State.Result;
  while P <> PEnd do begin
    case State.Step of
      esEncodeStepA:
      begin
        B := P^;
        Inc(P);
        C := (B and $FC) shr 2;
        POut^ := EncodeTable[C];
        Inc(POut, CharSize);
        C := (B and $3) shl 4;
        State.Step := esEncodeStepB;
      end;

      esEncodeStepB:
      begin
        B := P^;
        Inc(P);
        C := C or (B and $F0) shr 4;
        POut^ := EncodeTable[C];
        Inc(POut, CharSize);
        C := (B and $F) shl 2;
        State.Step := esEncodeStepC;
      end;

      esEncodeStepC:
      begin
        B := P^;
        Inc(P);
        C := C or (B and $C0) shr 6;
        POut^ := EncodeTable[C];
        Inc(POut, CharSize);
        C := (B and $3F) shr 0;
        POut^ := EncodeTable[C];
        Inc(POut, CharSize);
        Inc(State.StepCount);
        State.Step := esEncodeStepA;
      end;
    end;
  end;
  State.Result := C;
  Result := IntPtr(POut) - IntPtr(Output);
end;

function EncodeBytesEnd(Output: PByte; var State: TEncodeState;
  const CharSize: SmallInt): Integer;
var
  POut: PByte;
begin
  POut := Output;
  case State.Step of
    esEncodeStepB:
    begin
      POut^ := EncodeTable[State.Result];
      Inc(POut, CharSize);
      POut^ := Byte('=');
      Inc(POut, CharSize);
      POut^ := Byte('=');
      Inc(POut, CharSize);
    end;
    esEncodeStepC:
    begin
      POut^ := EncodeTable[State.Result];
      Inc(POut, CharSize);
      POut^ := Byte('=');
      Inc(POut, CharSize);
    end;
  end;
  Result := IntPtr(POut) - IntPtr(Output);
end;
{$ELSE}
const
  Base64_Chars: array[0..63] of AnsiChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  Base64_Bytes: array[0..79] of Byte =
  (
    62, 0, 0, 0, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 0,
    0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    0, 0, 0, 0, 0, 0, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
    36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51
  );
function EncodeBytes(const Source; SourceSize: Integer; var Base64Buf): Integer;
asm
    push    ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax         // esi = Source
    mov     edi, ecx         // edi = Buf
    mov     eax, edx
    cdq
    mov     ecx, 3
    div     ecx              // edx = SourceSize % 3
    mov     ecx, eax         // ecx = SourceSize / 3
    test    edx, edx
    jz      @@1
    inc     eax              // eax = (SourceSize + 2) / 3
  @@1:
    push    eax
    push    edx
    lea     ebp, Base64_Chars
    jecxz   @Last
    cld
  @EncodeLoop:               //  while (ecx > 0){
    mov     edx, [esi]       //   edx = 00000000 33333333 22222222 11111111
    bswap   edx              //   edx = 11111111 22222222 33333333 00000000
    push    edx
    push    edx
    push    edx
    pop     ebx              //   ebx = edx
    shr     edx, 20
    shr     ebx, 26          //   ebx = 00111111
    and     edx, 63          //   edx = 00112222
    mov     ah, [ebp + edx]  //   *(word*)edi = (Base64_Chars[edx] << 8) |
    mov     al, [ebp + ebx]  //     Base64_Chars[ebx]
    stosw                    //   edi += 2
    pop     edx              //   edx = 11111111 22222222 33333333 00000000
    pop     ebx              //   ebx = edx
    shr     edx, 8
    shr     ebx, 14
    and     edx, 63          //   edx = 00333333
    and     ebx, 63          //   ebx = 00222233
    mov     ah, [ebp + edx]  //   *(word*)edi = (Base64_Chars[edx] << 8) |
    mov     al, [ebp + ebx]  //     Base64_Chars[ebx]
    stosw                    //   edi += 2
    add     esi, 3           //   esi += 3
    loop    @EncodeLoop      // }
  @Last:
    pop     ecx              // ecx = SourceSize % 3
    jecxz   @end             // if (ecx == 0) return
    mov     eax, 3d3d0000h   // preset 2 bytes '='
    mov     [edi], eax
    test    ecx, 2
    jnz     @@3
    mov     al, [esi]        // if (ecx == 1)
    shl     eax, 4           //   eax = *esi << 4
    jmp     @@4
  @@3:
    mov     ax, [esi]        // else
    xchg    al, ah           //   eax = ((*esi << 8) or *(esi + 1)) << 2
    shl     eax, 2
  @@4:
    add     edi, ecx         // edi += ecx
    inc     ecx              // ecx = last encode bytes
  @LastLoop:
    mov     edx, eax         // for (; cex > 0; ecx --, edi --)
    and     edx, 63          // {
    mov     dl, [ebp + edx]  //   edx = eax & 63
    mov     [edi], dl        //   *edi = Base64_Chars[edx]
    shr     eax, 6           //   eax >>= 6
    dec     edi              // }
    loop    @LastLoop
  @end:
    pop     eax
    shl     eax, 2           // return  encode bytes
    pop     ebx
    pop     edi
    pop     esi
    pop     ebp
end;
{$ENDIF}

function DecodeBytes(Input, Output: PByte; InputLen: Integer;
  CharSize: SmallInt; var State: TDecodeState): Integer;
var
  POut: PByte;
  Fragment: Integer;
  P, PEnd: PByte;
begin
  POut := Output;
  P := Input;
  PEnd := Pointer(IntPtr(P) + InputLen);
  POut^ := State.Result;
  while True do begin
    case State.Step of
      dsDecodeStepA:
      begin
        repeat
          if P = PEnd then
          begin
            State.Result := POut^;
            Result := IntPtr(POut) - IntPtr(Output);
            Exit;
          end;
          Fragment := DecodeValue(Ord(P^));
          Inc(P, CharSize);
        until (Fragment >= 0) ;
        POut^ := (Fragment and $03F) shl 2;
        State.Step := dsDecodeStepB;
      end;

      dsDecodeStepB:
      begin
        repeat
          if P = PEnd then
          begin
            State.Result := POut^;
            Result := IntPtr(POut) - IntPtr(Output);
            Exit;
          end;
          Fragment := DecodeValue(Ord(P^));
          Inc(P, CharSize);
        until (Fragment >= 0) ;
        POut^ := (POut^ or ((Fragment and $030) shr 4));
        Inc(POut);
        POut^ :=           ((Fragment and $00F) shl 4);
        State.Step := dsDecodeStepC;
      end;

      dsDecodeStepC:
      begin
        repeat
          if P = PEnd then
          begin
            State.Result := POut^;
            Result := IntPtr(POut) - IntPtr(Output);
            Exit;
          end;
          Fragment := DecodeValue(Ord(P^));
          Inc(P, CharSize);
        until (Fragment >= 0) ;
        POut^ := (POut^ or ((Fragment and $03C) shr 2));
        Inc(POut);
        POut^ :=           ((Fragment and $003) shl 6);
        State.Step := dsDecodeStepD;
      end;

      dsDecodeStepD:
      begin
        repeat
          if P = PEnd then
          begin
            State.Result := POut^;
            Result := IntPtr(POut) - IntPtr(Output);
            Exit;
          end;
          Fragment := DecodeValue(Ord(P^));
          Inc(P, CharSize);
        until (Fragment >= 0) ;
        POut^ := (POut^ or (Fragment and $03F));
        Inc(POut);
        State.Step := dsDecodeStepA;
      end;
    end;
  end;
end;

{$IFNDEF WIN32}
function Base64Encode(const Input: Pointer; ASize: Int64; OutBuffer: PByte;
  const CharSize: SmallInt): Int64; overload;
var
  State: TEncodeState;
begin
  State := CEncodeState;
  Result := EncodeBytes(Input, OutBuffer, ASize, State, CharSize);
  Result := EncodeBytesEnd(Pointer(IntPtr(OutBuffer) + Result), State, CharSize) + Result;
end;
{$ENDIF}

function StrToBase64(const Input: Pointer; ASize: Int64): string;
const
  CharSize = SizeOf(Char);
var
  {$IFNDEF WIN32}Len: Int64;{$ENDIF}
  Estimate: Integer;
begin
  Estimate := EstimateEncodeLength(ASize);
  if Estimate > 0 then begin
    SetLength(Result, Estimate);
    {$IFNDEF WIN32}
    FillChar(Pointer(Result)^, Estimate * Charsize, 0);
    Len := Base64Encode(Input, ASize, Pointer(Result), CharSize);
    SetLength(Result, Len div CharSize);
    {$ELSE}
    EncodeBytes(Input^, ASize, Result[1]);
    {$ENDIF}
  end else
    SetLength(Result, 0);
end;

function StrToBase64(const Input: string; ConvToUTF8: Boolean): string;
var
  Data: RawByteString;
begin
  {$IFNDEF WIN32}
  if ConvToUTF8 then begin
    {$IFDEF NEXTGEN}
    Data := RawByteString(Input);
    {$ELSE}
    Data := Base64.Utf8Encode(Input);
    {$ENDIF}
    Result := StrToBase64(Pointer(Data), Length(Data));
  end else begin
    Data := RawByteString(Input);
    Result := StrToBase64(Pointer(Data), Length(Data));
  end;
  {$ELSE}
  if ConvToUTF8 then begin
    Data := Base64.Utf8Encode(StringW(Input));
    Result := StrToBase64(Pointer(Data), Length(Data))
  end else
    Result := StrToBase64(Pointer(Input), Length(Input));
  {$ENDIF}
end;

{$IFNDEF WIN32}
function StrToBase64(const Input: RawByteString): string;
begin
  Result := StrToBase64(Pointer(Input), Length(Input));
end;
{$ENDIF}

function Base64Encode(const Input: Pointer; ASize: Int64;
  const CharSize: SmallInt): TBytes; overload;
var
  {$IFNDEF WIN32}Len: Int64;{$ENDIF}
  Estimate: Integer;
begin
  Estimate := EstimateEncodeLength(ASize);
  if Estimate > 0 then begin
    SetLength(Result, Estimate);
    {$IFNDEF WIN32}
    FillChar(Pointer(Result)^, Estimate, 0);
    Len := Base64Encode(Input, ASize, @Result[0], CharSize);
    SetLength(Result, Len);
    {$ELSE}
    EncodeBytes(Input^, ASize, Result[0]);
    {$ENDIF}
  end else
    SetLength(Result, 0);
end;

function Base64Encode(const Input: Pointer; ASize: Int64): TBytes;
begin
  Result := Base64Encode(Input, ASize, 1);
end;

function Base64Encode(const Input, Output: TStream): Int64;
begin
  Result := 0;
end;

function Base64Encode(const Input: TBytes): TBytes;
begin
  if Length(Input) > 0 then begin
    Result := Base64Encode(@Input[0], Length(Input), 1);
  end else
    SetLength(Result, 0);
end;

function Base64Encode(const Input: string): TBytes;
var
  Raw: RawByteString;
begin
  Raw := Base64.Utf8Encode(Input);
  Result := Base64Encode(Pointer(Raw), Length(Raw), 1);
end;

function Base64Decode(const Input, Output: TStream): Int64;
begin
  Result := 0;
end;

function Base64Decode(const Input: Pointer; ASize: Int64; const CharSize: SmallInt): TBytes; overload;
var
  Len: Integer;
  State: TDecodeState;
begin
  SetLength(Result, EstimateDecodeLength(ASize));
  State := CDecodeState;
  Len := DecodeBytes(PByte(Input), PByte(Result), ASize, CharSize, State);
  SetLength(Result, Len);
end;

function Base64Decode(const Input: string): TBytes;
const
  CharSize = SizeOf(Char);
begin
  Result := Base64Decode(PByte(Input), Length(Input), CharSize);
end;

function Base64Decode(const Input: Pointer; ASize: Int64): TBytes;
begin
  Result := Base64Decode(Input, ASize, 1);
end;

function Base64Decode(const Input: TBytes): TBytes;
begin
  if Length(Input) > 0 then begin
    Result := Base64Decode(@Input[0], Length(Input), 1);
  end else
    SetLength(Result, 0);
end;

function Base64ToStr(const Input: Pointer; ASize: Int64; IsUTF8: Boolean;
  CharSize: Integer): string; overload;
var
  Len: Integer;
  State: TDecodeState;
  Data: RawByteString;
begin
  State := CDecodeState;
  SetLength(Data, EstimateDecodeLength(ASize));
  Len := DecodeBytes(Input, Pointer(Data), ASize * CharSize, CharSize, State);
  if Len > 0 then begin
    if IsUTF8 then
      {$IFDEF NEXTGEN}
      Result := AnsiDecode(Pointer(Data), Len)
      {$ELSE}
      Result := Utf8Decode(Pointer(Data), Len)
      {$ENDIF}
    else
      Result := AnsiDecode(Pointer(Data), Len);
  end else
    Result := '';
end;

function Base64ToStr(const Input: string; IsUTF8: Boolean): string;
const
  CharSize = Sizeof(Char);
begin
  Result := Base64ToStr(Pointer(Input), Length(Input), IsUTF8, CharSize);
end;

function Base64ToStr(const Input: Pointer; ASize: Int64; IsUTF8: Boolean): string;
begin
  Result := Base64ToStr(Input, ASize, IsUTF8, 1);
end;

{$IFNDEF WIN32}
function Base64ToStr(const Input: RawByteString; IsUTF8: Boolean): string;
begin
  Result := Base64ToStr(Pointer(Input), Length(Input), IsUTF8, 1);
end;
{$ENDIF}

initialization
  InitEncodeState(CEncodeState);
  InitDecodeState(CDecodeState);

end.
