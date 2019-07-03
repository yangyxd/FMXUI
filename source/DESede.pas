{*******************************************************}
{                                                       }
{       DESede 加解密单元 (与 Java 、 C# 同步)          }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

// 注意：C# 默认需要设置 CBCMode = True
// 编解码核心使用 FlyUtils.CSharpJavaDES 修改

unit DESede;

interface

uses
  Classes, SysUtils;

type
  TPointerStream = class(TCustomMemoryStream)
  public
    constructor Create(const Data: Pointer; const ASize: NativeInt); overload;
    function Write(const Buffer; Count: Longint): Longint; overload; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; overload; override;
  end;

// 根据字符串生成密钥字节数组
function Build3DesKey(const Key: string): TBytes;

// DESede 加密
function DESedeEncrypt(const Data: string; const Key: string): RawByteString; overload;
function DESedeEncrypt(const Data: string; const Key: TBytes): RawByteString; overload;
function DESedeEncrypt(const Data: TBytes; const Key: TBytes): RawByteString; overload;
function DESedeEncrypt(Data: Pointer; Size: Cardinal; const Key: TBytes): RawByteString; overload;
function DESedeEncrypt(const InStream, OutStream: TStream; const Key: TBytes): Boolean; overload;

// DESede 解密
function DESedeDecrypt(const Data: RawByteString; const Key: string): RawByteString; overload;
function DESedeDecrypt(const Data: RawByteString; const Key: TBytes): RawByteString; overload;
function DESedeDecrypt(Data: Pointer; Size: Cardinal; const Key: string): RawByteString; overload;
function DESedeDecrypt(const Data: TBytes; const Key: TBytes): RawByteString; overload;
function DESedeDecrypt(Data: Pointer; Size: Cardinal; const Key: TBytes): RawByteString; overload;
function DESedeDecrypt(const InStream, OutStream: TStream; const Key: TBytes): Boolean; overload;

function DESedeToBytes(Data: Pointer; Size: Cardinal; const Key: string): TBytes; overload;
function DESedeToBytes(const InStream: TStream; const Key: string): TBytes; overload;

function StrToDESede(const Data, Key: string): RawByteString;
function StrToDESedeBytes(const Data, Key: string): TBytes;

function DESedeToStr(const Data: RawByteString; const Key: string): string; overload;
function DESedeToStr(const Data: TBytes; const Key: string): string; overload;

// DES 编码解码
function DESedeStream(const InStream, OutStream: TStream; const Key: TBytes;
  EncryptMode: Boolean; IvBytes: TBytes; CBCMode: Boolean = True; PaddingZero: Boolean = False): Boolean;

implementation

function Build3DesKey(const Key: string): TBytes;
var
  S: RawByteString;
begin
  SetLength(Result, 24);
  S := AnsiToUtf8(Key);
  if (Length(Result) > Length(S)) then
    // 如果temp不够24位
    Move(Pointer(S)^, Result[0], Length(S))
  else
    Move(Pointer(S)^, Result[0], Length(Result));
end;

type
  TUint32s = array of UInt32;

procedure des_createKeys(KeyBytes: TBytes; var keys: TUint32s);
const
  // declaring this locally speeds things up a bit
  pc2bytes0: array [0 .. 15] of UInt32 = (0, $4, $20000000, $20000004, $10000,
    $10004, $20010000, $20010004, $200, $204, $20000200, $20000204, $10200,
    $10204, $20010200, $20010204);
  pc2bytes1: array [0 .. 15] of UInt32 = (0, $1, $100000, $100001, $4000000,
    $4000001, $4100000, $4100001, $100, $101, $100100, $100101, $4000100,
    $4000101, $4100100, $4100101);
  pc2bytes2: array [0 .. 15] of UInt32 = (0, $8, $800, $808, $1000000, $1000008,
    $1000800, $1000808, 0, $8, $800, $808, $1000000, $1000008, $1000800,
    $1000808);
  pc2bytes3: array [0 .. 15] of UInt32 = (0, $200000, $8000000, $8200000, $2000,
    $202000, $8002000, $8202000, $20000, $220000, $8020000, $8220000, $22000,
    $222000, $8022000, $8222000);
  pc2bytes4: array [0 .. 15] of UInt32 = (0, $40000, $10, $40010, 0, $40000,
    $10, $40010, $1000, $41000, $1010, $41010, $1000, $41000, $1010, $41010);
  pc2bytes5: array [0 .. 15] of UInt32 = (0, $400, $20, $420, 0, $400, $20,
    $420, $2000000, $2000400, $2000020, $2000420, $2000000, $2000400, $2000020,
    $2000420);
  pc2bytes6: array [0 .. 15] of UInt32 = (0, $10000000, $80000, $10080000, $2,
    $10000002, $80002, $10080002, 0, $10000000, $80000, $10080000, $2,
    $10000002, $80002, $10080002);
  pc2bytes7: array [0 .. 15] of UInt32 = (0, $10000, $800, $10800, $20000000,
    $20010000, $20000800, $20010800, $20000, $30000, $20800, $30800, $20020000,
    $20030000, $20020800, $20030800);
  pc2bytes8: array [0 .. 15] of UInt32 = (0, $40000, 0, $40000, $2, $40002, $2,
    $40002, $2000000, $2040000, $2000000, $2040000, $2000002, $2040002,
    $2000002, $2040002);
  pc2bytes9: array [0 .. 15] of UInt32 = (0, $10000000, $8, $10000008, 0,
    $10000000, $8, $10000008, $400, $10000400, $408, $10000408, $400, $10000400,
    $408, $10000408);
  pc2bytes10: array [0 .. 15] of UInt32 = (0, $20, 0, $20, $100000, $100020,
    $100000, $100020, $2000, $2020, $2000, $2020, $102000, $102020,
    $102000, $102020);
  pc2bytes11: array [0 .. 15] of UInt32 = (0, $1000000, $200, $1000200, $200000,
    $1200000, $200200, $1200200, $4000000, $5000000, $4000200, $5000200,
    $4200000, $5200000, $4200200, $5200200);
  pc2bytes12: array [0 .. 15] of UInt32 = (0, $1000, $8000000, $8001000, $80000,
    $81000, $8080000, $8081000, $10, $1010, $8000010, $8001010, $80010, $81010,
    $8080010, $8081010);
  pc2bytes13: array [0 .. 15] of UInt32 = (0, $4, $100, $104, 0, $4, $100, $104,
    $1, $5, $101, $105, $1, $5, $101, $105);
  // now define the left shifts which need to be done
  shifts: array [0 .. 15] of UInt32 = (0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
    1, 1, 1, 0);
var
  lefttemp, righttemp, temp: UInt32;
  m, n, j, i: Integer;
  left, right: UInt32;
  iterations: Integer;
begin
  // how many iterations (1 for des, 3 for triple des)
  j := Length(KeyBytes);
  if j >= 24 then
    iterations := 3
  else
    iterations := 1;
  // stores the return keys
  n := 32 * iterations;
  SetLength(keys, n);
  n := 8 * iterations;
  SetLength(KeyBytes, n);
  for i := j to n - 1 do
    KeyBytes[i] := 0;

  // other variables
  m := 0;
  n := 0;
  for j := 0 to iterations - 1 do begin // either 1 or 3 iterations
    left := (ord(KeyBytes[m + 0]) shl 24) or (ord(KeyBytes[m + 1]) shl 16) or
      (ord(KeyBytes[m + 2]) shl 8) or ord(KeyBytes[m + 3]);
    right := (ord(KeyBytes[m + 4]) shl 24) or (ord(KeyBytes[m + 5]) shl 16) or
      (ord(KeyBytes[m + 6]) shl 8) or ord(KeyBytes[m + 7]);
    m := m + 8;
    temp := ((left shr 4) xor right) and $0F0F0F0F;
    right := right xor temp;
    left := left xor (temp shl 4);
    temp := ((right shr 16) xor left) and $0000FFFF;
    left := left xor temp;
    right := right xor (temp shl 16);
    temp := ((left shr 2) xor right) and $33333333;
    right := right xor temp;
    left := left xor (temp shl 2);
    temp := ((right shr 16) xor left) and $0000FFFF;
    left := left xor temp;
    right := right xor (temp shl 16);
    temp := ((left shr 1) xor right) and $55555555;
    right := right xor temp;
    left := left xor (temp shl 1);
    temp := ((right shr 8) xor left) and $00FF00FF;
    left := left xor temp;
    right := right xor (temp shl 8);
    temp := ((left shr 1) xor right) and $55555555;
    right := right xor temp;
    left := left xor (temp shl 1);
    // the right side needs to be shifted and to get the last four bits of the left side
    temp := (left shl 8) or ((right shr 20) and $000000F0);
    // left needs to be put upside down
    left := (right shl 24) or ((right shl 8) and $FF0000) or
      ((right shr 8) and $FF00) or ((right shr 24) and $F0);
    right := temp;
    // now go through and perform these shifts on the left and right keys
    for i := low(shifts) to high(shifts) do begin
      // shift the keys either one or two bits to the left
      if shifts[i] > 0 then begin
        left := (left shl 2) or (left shr 26);
        right := (right shl 2) or (right shr 26);
        // left := left shl 0;
        // right:= right shl 0;
      end else begin
        left := (left shl 1) or (left shr 27);
        right := (right shl 1) or (right shr 27);
        // left := left shl 0;
        // right:= right shl 0;
      end;
      left := left and $FFFFFFF0;
      right := right and $FFFFFFF0;
      // now apply PC-2, in such a way that E is easier when encrypting or decrypting
      // this conversion will look like PC-2 except only the last 6 bits of each byte are used
      // rather than 48 consecutive bits and the order of lines will be according to
      // how the S selection functions will be applied: S2, S4, S6, S8, S1, S3, S5, S7
      lefttemp := pc2bytes0[left shr 28] or pc2bytes1[(left shr 24) and $F] or
        pc2bytes2[(left shr 20) and $F] or pc2bytes3[(left shr 16) and $F] or
        pc2bytes4[(left shr 12) and $F] or pc2bytes5[(left shr 8) and $F] or
        pc2bytes6[(left shr 4) and $F];
      righttemp := pc2bytes7[right shr 28] or pc2bytes8[(right shr 24) and $F]
        or pc2bytes9[(right shr 20) and $F] or pc2bytes10[(right shr 16) and $F]
        or pc2bytes11[(right shr 12) and $F] or pc2bytes12[(right shr 8) and $F]
        or pc2bytes13[(right shr 4) and $F];
      temp := ((righttemp shr 16) xor lefttemp) and $0000FFFF;
      keys[n + 0] := lefttemp xor temp;
      keys[n + 1] := righttemp xor (temp shl 16);
      n := n + 2;
    end;
  end; // for each iterations
  // return the keys we've created
end; // end of des_createKeys

function DESedeStream(const InStream, OutStream: TStream; const Key: TBytes;
  EncryptMode: Boolean; IvBytes: TBytes; CBCMode: Boolean = True; PaddingZero: Boolean = False): Boolean;
const
  spfunction1: array [0 .. 63] of UInt32 = ($1010400, 0, $10000, $1010404,
    $1010004, $10404, $4, $10000, $400, $1010400, $1010404, $400, $1000404,
    $1010004, $1000000, $4, $404, $1000400, $1000400, $10400, $10400, $1010000,
    $1010000, $1000404, $10004, $1000004, $1000004, $10004, 0, $404, $10404,
    $1000000, $10000, $1010404, $4, $1010000, $1010400, $1000000, $1000000,
    $400, $1010004, $10000, $10400, $1000004, $400, $4, $1000404, $10404,
    $1010404, $10004, $1010000, $1000404, $1000004, $404, $10404, $1010400,
    $404, $1000400, $1000400, 0, $10004, $10400, 0, $1010004);
  spfunction2: array [0 .. 63] of UInt32 = ($80108020, $80008000, $8000,
    $108020, $100000, $20, $80100020, $80008020, $80000020, $80108020,
    $80108000, $80000000, $80008000, $100000, $20, $80100020, $108000, $100020,
    $80008020, 0, $80000000, $8000, $108020, $80100000, $100020, $80000020, 0,
    $108000, $8020, $80108000, $80100000, $8020, 0, $108020, $80100020, $100000,
    $80008020, $80100000, $80108000, $8000, $80100000, $80008000, $20,
    $80108020, $108020, $20, $8000, $80000000, $8020, $80108000, $100000,
    $80000020, $100020, $80008020, $80000020, $100020, $108000, 0, $80008000,
    $8020, $80000000, $80100020, $80108020, $108000);
  spfunction3: array [0 .. 63] of UInt32 = ($208, $8020200, 0, $8020008,
    $8000200, 0, $20208, $8000200, $20008, $8000008, $8000008, $20000, $8020208,
    $20008, $8020000, $208, $8000000, $8, $8020200, $200, $20200, $8020000,
    $8020008, $20208, $8000208, $20200, $20000, $8000208, $8, $8020208, $200,
    $8000000, $8020200, $8000000, $20008, $208, $20000, $8020200, $8000200, 0,
    $200, $20008, $8020208, $8000200, $8000008, $200, 0, $8020008, $8000208,
    $20000, $8000000, $8020208, $8, $20208, $20200, $8000008, $8020000,
    $8000208, $208, $8020000, $20208, $8, $8020008, $20200);
  spfunction4: array [0 .. 63] of UInt32 = ($802001, $2081, $2081, $80, $802080,
    $800081, $800001, $2001, 0, $802000, $802000, $802081, $81, 0, $800080,
    $800001, $1, $2000, $800000, $802001, $80, $800000, $2001, $2080, $800081,
    $1, $2080, $800080, $2000, $802080, $802081, $81, $800080, $800001, $802000,
    $802081, $81, 0, 0, $802000, $2080, $800080, $800081, $1, $802001, $2081,
    $2081, $80, $802081, $81, $1, $2000, $800001, $2001, $802080, $800081,
    $2001, $2080, $800000, $802001, $80, $800000, $2000, $802080);
  spfunction5: array [0 .. 63] of UInt32 = ($100, $2080100, $2080000, $42000100,
    $80000, $100, $40000000, $2080000, $40080100, $80000, $2000100, $40080100,
    $42000100, $42080000, $80100, $40000000, $2000000, $40080000, $40080000, 0,
    $40000100, $42080100, $42080100, $2000100, $42080000, $40000100, 0,
    $42000000, $2080100, $2000000, $42000000, $80100, $80000, $42000100, $100,
    $2000000, $40000000, $2080000, $42000100, $40080100, $2000100, $40000000,
    $42080000, $2080100, $40080100, $100, $2000000, $42080000, $42080100,
    $80100, $42000000, $42080100, $2080000, 0, $40080000, $42000000, $80100,
    $2000100, $40000100, $80000, 0, $40080000, $2080100, $40000100);
  spfunction6: array [0 .. 63] of UInt32 = ($20000010, $20400000, $4000,
    $20404010, $20400000, $10, $20404010, $400000, $20004000, $404010, $400000,
    $20000010, $400010, $20004000, $20000000, $4010, 0, $400010, $20004010,
    $4000, $404000, $20004010, $10, $20400010, $20400010, 0, $404010, $20404000,
    $4010, $404000, $20404000, $20000000, $20004000, $10, $20400010, $404000,
    $20404010, $400000, $4010, $20000010, $400000, $20004000, $20000000, $4010,
    $20000010, $20404010, $404000, $20400000, $404010, $20404000, 0, $20400010,
    $10, $4000, $20400000, $404010, $4000, $400010, $20004010, 0, $20404000,
    $20000000, $400010, $20004010);
  spfunction7: array [0 .. 63] of UInt32 = ($200000, $4200002, $4000802, 0,
    $800, $4000802, $200802, $4200800, $4200802, $200000, 0, $4000002, $2,
    $4000000, $4200002, $802, $4000800, $200802, $200002, $4000800, $4000002,
    $4200000, $4200800, $200002, $4200000, $800, $802, $4200802, $200800, $2,
    $4000000, $200800, $4000000, $200800, $200000, $4000802, $4000802, $4200002,
    $4200002, $2, $200002, $4000000, $4000800, $200000, $4200800, $802, $200802,
    $4200800, $802, $4000002, $4200802, $4200000, $200800, 0, $2, $4200802, 0,
    $200802, $4200000, $800, $4000002, $4000800, $800, $200002);
  spfunction8: array [0 .. 63] of UInt32 = ($10001040, $1000, $40000, $10041040,
    $10000000, $10001040, $40, $10000000, $40040, $10040000, $10041040, $41000,
    $10041000, $41040, $1000, $40, $10040000, $10000040, $10001000, $1040,
    $41000, $40040, $10040040, $10041000, $1040, 0, 0, $10040040, $10000040,
    $10001000, $41040, $40000, $41040, $40000, $10041000, $1000, $40, $10040040,
    $1000, $41040, $10001000, $40, $10000040, $10040000, $10040040, $10000000,
    $40000, $10001040, 0, $10041040, $40040, $10000040, $10040000, $10001000,
    $10001040, 0, $10041040, $41000, $41000, $1040, $1040, $40040, $10000000,
    $10041000);
var
  Keys: TUint32s;
  i, j, keyslen, iterations: Integer;
  m, len, BufLen: Integer;
  looping: array of Integer;
  endloop, loopinc: Integer;
  cbcleft, cbcleft2, cbcright, cbcright2: UInt32;
  temp, right1, right2, left, right: UInt32;
  ReadCount, ProcessCount, PaddingCount: UInt64;
  StrByte, OutByte: array [0..7] of Byte;
begin
  if InStream = nil then
    raise Exception.Create('Error: InStream is nil.');
  if OutStream = nil then
    raise Exception.Create('Error: OutStream is nil.');

  ProcessCount := InStream.Size - InStream.Position;

  if EncryptMode then begin
    PaddingCount := 8 - ProcessCount mod 8;
    if PaddingCount = 8 then
      PaddingCount := 0;
  end else
    PaddingCount := 0;

  j := Length(IvBytes);
  SetLength(IvBytes, 8);
  for i := j to 7 do
    IvBytes[i] := 0;

  SetLength(keys, 0);
  des_createKeys(Key, keys);
  keyslen := length(keys);
  if keyslen = 32 then
    iterations := 3
  else
    iterations := 9;

  ReadCount := 0;
  m := 0;
  cbcleft := 0;
  cbcleft2 := 0;
  cbcright := 0;
  cbcright2 := 0;

  if iterations = 3 then begin
    if EncryptMode then begin
      SetLength(looping, 3);
      looping[0] := 0;
      looping[1] := 32;
      looping[2] := 2;
    end else begin
      SetLength(looping, 3);
      looping[0] := 30;
      looping[1] := -2;
      looping[2] := -2;
    end;
  end else begin
    if EncryptMode then begin
      SetLength(looping, 9);
      looping[0] := 0;
      looping[1] := 32;
      looping[2] := 2;
      looping[3] := 62;
      looping[4] := 30;
      looping[5] := -2;
      looping[6] := 64;
      looping[7] := 96;
      looping[8] := 2;
    end else begin
      SetLength(looping, 9);
      looping[0] := 94;
      looping[1] := 62;
      looping[2] := -2;
      looping[3] := 32;
      looping[4] := 64;
      looping[5] := 2;
      looping[6] := 30;
      looping[7] := -2;
      looping[8] := -2;
    end;
  end;

  if CBCMode then begin // CBC mode (这里也是关键C#DES加密默认是CBC模式)
    cbcleft := (ord(IvBytes[0]) shl 24) or (ord(IvBytes[1]) shl 16) or
      (ord(IvBytes[2]) shl 8) or ord(IvBytes[3]);
    cbcright := (ord(IvBytes[4]) shl 24) or (ord(IvBytes[5]) shl 16) or
      (ord(IvBytes[6]) shl 8) or ord(IvBytes[7]);
  end;

  // loop through each 64 bit chunk of the message
  len := ProcessCount + PaddingCount;
  while m < len do begin
    BufLen := 8;
    if (ReadCount + BufLen) > ProcessCount then
      BufLen := ProcessCount - ReadCount;
    if BufLen > 0 then
      InStream.Read(StrByte, BufLen);
    if BufLen < 0 then BufLen := 0;
    ReadCount := ReadCount + 8;
    if BufLen < 8 then begin
      // 尾部不足 8，补 PaddingCount
      for I := BufLen to 8 - 1 do begin
        if PaddingZero then
          StrByte[I] := 0
        else
          StrByte[I] := PaddingCount;
      end;
    end;
    m := m + 8;

    left := (ord(StrByte[0]) shl 24) or (ord(StrByte[1]) shl 16) or
      (ord(StrByte[2]) shl 8) or ord(StrByte[3]);
    right := (ord(StrByte[4]) shl 24) or (ord(StrByte[5]) shl 16) or
      (ord(StrByte[6]) shl 8) or ord(StrByte[7]);

    // for Cipher Block Chaining mode, xor the message with the previous result
    if CBCMode then
    begin
      if EncryptMode then
      begin
        left := left xor cbcleft;
        right := right xor cbcright;
      end
      else
      begin
        cbcleft2 := cbcleft;
        cbcright2 := cbcright;
        cbcleft := left;
        cbcright := right;
      end;
    end;

    // first each 64 but chunk of the message must be permuted according to IP
    temp := ((left shr 4) xor right) and $0F0F0F0F;
    right := right xor temp;
    left := left xor (temp shl 4);
    temp := ((left shr 16) xor right) and $0000FFFF;
    right := right xor temp;
    left := left xor (temp shl 16);
    temp := ((right shr 2) xor left) and $33333333;
    left := left xor temp;
    right := right xor (temp shl 2);
    temp := ((right shr 8) xor left) and $00FF00FF;
    left := left xor temp;
    right := right xor (temp shl 8);
    temp := ((left shr 1) xor right) and $55555555;
    right := right xor temp;
    left := left xor (temp shl 1);
    left := ((left shl 1) or (left shr 31));
    right := ((right shl 1) or (right shr 31));

    // do this either 1 or 3 times for each chunk of the message
    j := 0;
    while j < iterations do begin
      endloop := looping[j + 1];
      loopinc := looping[j + 2];
      // now go through and perform the encryption or decryption
      i := looping[j];
      while i <> endloop do begin
        if (i >= 0) and (i < keyslen) then
          right1 := right xor keys[i]
        else
          right1 := right xor 0;
        if (i >= 0) and (i < keyslen - 1) then
          right2 := ((right shr 4) or (right shl 28)) xor keys[i + 1]
        else
          right2 := ((right shr 4) or (right shl 28)) xor 0;
        // the result is attained by passing these bytes through the S selection functions
        temp := left;
        left := right;
        right := temp xor (spfunction2[(right1 shr 24) and $3F] or
          spfunction4[(right1 shr 16) and $3F] or spfunction6[(right1 shr 8) and
          $3F] or spfunction8[right1 and $3F] or spfunction1[(right2 shr 24) and
          $3F] or spfunction3[(right2 shr 16) and $3F] or
          spfunction5[(right2 shr 8) and $3F] or spfunction7[right2 and $3F]);
        i := i + loopinc;
      end;
      temp := left;
      left := right;
      right := temp; // unreverse left and right
      j := j + 3;
    end; // for either 1 or 3 iterations

    // move then each one bit to the right
    left := ((left shr 1) or (left shl 31));
    right := ((right shr 1) or (right shl 31));
    // now perform IP-1, which is IP in the opposite direction
    temp := ((left shr 1) xor right) and $55555555;
    right := right xor temp;
    left := left xor (temp shl 1);
    temp := ((right shr 8) xor left) and $00FF00FF;
    left := left xor temp;
    right := right xor (temp shl 8);
    temp := ((right shr 2) xor left) and $33333333;
    left := left xor temp;
    right := right xor (temp shl 2);
    temp := ((left shr 16) xor right) and $0000FFFF;
    right := right xor temp;
    left := left xor (temp shl 16);
    temp := ((left shr 4) xor right) and $0F0F0F0F;
    right := right xor temp;
    left := left xor (temp shl 4);

    // for Cipher Block Chaining mode, xor the message with the previous result
    if CBCMode then begin
      if EncryptMode then begin
        cbcleft := left;
        cbcright := right;
      end else begin
        left := left xor cbcleft2;
        right := right xor cbcright2;
      end;
    end;

    OutByte[0] := left shr 24;
    OutByte[1] := (left shr 16) and $FF;
    OutByte[2] := (left shr 8) and $FF;
    OutByte[3] := left and $FF;
    OutByte[4] := right shr 24;
    OutByte[5] := (right shr 16) and $FF;
    OutByte[6] := (right shr 8) and $FF;
    OutByte[7] := right and $FF;

    BufLen := 8;
    if not EncryptMode then begin
      PaddingCount := 0;
      if (ReadCount >= ProcessCount) then begin

        if PaddingZero then begin
          for I := 7 downto 0 do begin
            if OutByte[I] <> 0 then begin
              BufLen := I + 1;
              break;
            end;
          end;

        end else begin
          PaddingCount := OutByte[7];
          if PaddingCount > 8 then PaddingCount := 0;
          if PaddingCount > 0 then
            BufLen := 8 - PaddingCount;
          if BufLen < 0 then BufLen := 0;
        end;

      end;
    end;

    if BufLen > 0 then
      OutStream.Write(OutByte, BufLen);
  end;

  SetLength(keys, 0);
  Result := True;
end;

function DESedeEncrypt(const Data: string; const Key: string): RawByteString;
var
  sKey: TBytes;
begin
  sKey := build3DesKey(Key);
  Result := DESedeEncrypt(Data, sKey);
end;

function DESedeEncrypt(const Data: string; const Key: TBytes): RawByteString;
var
  S: RawByteString;
begin
  S := AnsiToUtf8(Data); // 转为 UTF-8
  Result := DESedeEncrypt(@S[1], Length(S), Key);
end;

function DESedeEncrypt(const Data: TBytes; const Key: TBytes): RawByteString;
begin
  if Length(Data) = 0 then
    Result := ''
  else
    Result := DESedeEncrypt(@Data[0], Length(Data), Key);
end;

function DESedeEncrypt(Data: Pointer; Size: Cardinal; const Key: TBytes): RawByteString;
var
  FIn: TPointerStream;
  FOut: TPointerStream;
  P: Cardinal;
begin
  Result := '';
  if (Size = 0) or (Length(Key) <> 24) then Exit;
  FIn := TPointerStream.Create();
  FOut := TPointerStream.Create();
  try
    FIn.SetPointer(Data, Size);
    FIn.Position := 0;
    if Size mod 8 > 0 then
      P := Size div 8 * 8 + 8
    else
      P := Size;
    SetLength(Result, P);
    FOut.SetPointer(Pointer(Result), P);
    FOut.Position := 0;
    DESedeEncrypt(FIn, FOut, Key);
  finally
    FIn.Free;
    FOut.Free;
  end;
end;

function DESedeEncrypt(const InStream, OutStream: TStream; const Key: TBytes): Boolean;
begin
  Result := DESedeStream(InStream, OutStream, Key, True, [], False, False);
end;

function DESedeDecrypt(const Data: RawByteString; const Key: string): RawByteString;
var
  sKey: TBytes;
begin
  sKey := build3DesKey(Key);
  Result := DESedeDecrypt(Data, sKey);
end;

function DESedeDecrypt(const Data: RawByteString; const Key: TBytes): RawByteString;
begin
  Result := DESedeDecrypt(Pointer(Data), Length(Data), Key);
end;

function DESedeDecrypt(Data: Pointer; Size: Cardinal; const Key: string): RawByteString;
var
  sKey: TBytes;
begin
  sKey := build3DesKey(Key);
  Result := DESedeDecrypt(Data, Size, sKey);
end;

function DESedeDecrypt(const Data: TBytes; const Key: TBytes): RawByteString;
begin
  if Length(Data) = 0 then
    Result := ''
  else
    Result := DESedeDecrypt(@Data[0], Length(Data), Key);
end;

function DESedeDecrypt(Data: Pointer; Size: Cardinal; const Key: TBytes): RawByteString;
var
  FIn: TPointerStream;
  FOut: TPointerStream;
begin
  Result := '';
  if (Size = 0) or (Length(Key) <> 24) then Exit;
  FIn := TPointerStream.Create();
  FOut := TPointerStream.Create();
  try
    FIn.SetPointer(Data, Size);
    FIn.Position := 0;
    SetLength(Result, Size);
    FOut.SetPointer(Pointer(Result), Size);
    FOut.Position := 0;
    DESedeDecrypt(FIn, FOut, Key);
    SetLength(Result, FOut.Position);
  finally
    FIn.Free;
    FOut.Free;
  end;
end;

function DESedeDecrypt(const InStream, OutStream: TStream; const Key: TBytes): Boolean;
begin
  Result := DESedeStream(InStream, OutStream, Key, False, [], False, False);
end;

function DESedeToStr(const Data: RawByteString; const Key: string): string;
var
  S: RawByteString;
begin
  S := DESedeDecrypt(Data, Key);
  Result := string(Utf8ToAnsi(S));
end;

function DESedeToStr(const Data: TBytes; const Key: string): string;
var
  S: RawByteString;
  sKey: TBytes;
begin
  sKey := build3DesKey(Key);
  S := DESedeDecrypt(Data, sKey);
  Result := string(Utf8ToAnsi(S));
end;

function DESedeToBytes(Data: Pointer; Size: Cardinal; const Key: string): TBytes;
var
  FIn: TPointerStream;
begin
  FIn := TPointerStream.Create(Data, Size);
  try
    Result := DESedeToBytes(FIn, Key);
  finally
    FreeAndNil(FIn);
  end;
end;

function DESedeToBytes(const InStream: TStream; const Key: string): TBytes;
var
  sKey: TBytes;
  FOut: TPointerStream;
  ASize: Int64;
begin
  SetLength(Result, 0);
  ASize := InStream.Size;
  sKey := build3DesKey(Key);
  if (ASize = 0) or (Length(sKey) <> 24) then Exit;
  FOut := TPointerStream.Create();
  try
    SetLength(Result, ASize);
    FOut.SetPointer(@Result[0], ASize);
    FOut.Position := 0;
    DESedeDecrypt(InStream, FOut, sKey);
    SetLength(Result, FOut.Position);
  finally
    FOut.Free;
  end;
end;

function StrToDESede(const Data, Key: string): RawByteString;
begin
  Result := DESedeEncrypt(Data, Key);
end;

function StrToDESedeBytes(const Data, Key: string): TBytes;
var
  FIn: TPointerStream;
  FOut: TPointerStream;
  sKey: TBytes;
  S: RawByteString;
  P: Cardinal;
begin
  SetLength(Result, 0);
  sKey := build3DesKey(Key);
  if (Data = '') or (Length(sKey) <> 24) then Exit;
  S := AnsiToUtf8(Data); // 转为 UTF-8
  FIn := TPointerStream.Create();
  FOut := TPointerStream.Create();
  try
    P := length(S);
    FIn.SetPointer(Pointer(S), P);
    FIn.Position := 0;
    if P mod 8 > 0 then
      P := P div 8 * 8 + 8;
    SetLength(Result, P);
    FOut.SetPointer(@Result[0], P);
    FOut.Position := 0;
    DESedeEncrypt(FIn, FOut, sKey);
  finally
    FIn.Free;
    FOut.Free;
  end;
end;

{ TPointerStream }

function TPointerStream.Write(const Buffer; Count: Longint): Longint;
var
  Pos: Longint;
begin
  if Count > 0 then begin
    Pos := Position;
    System.Move(Buffer, (PByte(Memory) + Pos)^, Count);
    Position := Pos + Count;
  end;
  Result := Count;
end;

constructor TPointerStream.Create(const Data: Pointer; const ASize: NativeInt);
begin
  inherited Create();
  SetPointer(Data, ASize);
end;

function TPointerStream.Write(const Buffer: TBytes; Offset,
  Count: Longint): Longint;
var
  Pos: Longint;
begin
  if (Count >= 0) then begin
    Pos := Position;
    System.Move(Buffer[Offset], (PByte(Memory) + Pos)^, Count);
    Position := Pos + Count;
  end;
  Result := Count;
end;

end.
