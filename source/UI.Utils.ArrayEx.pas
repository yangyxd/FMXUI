{*******************************************************}
{                                                       }
{       泛型动态数组扩展类                              }
{                                                       }
{       版权所有 (C) 2016 武稀松                        }
{                                                       }
{*******************************************************}

unit UI.Utils.ArrayEx;

interface

uses System.Generics.Defaults, System.SysUtils;

type
  TArrayEx<T> = record
  strict private
  type
    TEnumerator = class
    private
      FValue: TArray<T>;
      FIndex: NativeInt;
      function GetCurrent: T;
    public
      constructor Create(const AValue: TArray<T>);
      function MoveNext: Boolean;
      property Current: T read GetCurrent;
    end;
  public
    function GetEnumerator(): TEnumerator;
  strict private
    FData: TArray<T>;
    function GetRawData: TArray<T>;
    function GetElements(Index: Integer): T;
    procedure SetElements(Index: Integer; const Value: T);
  private
    class function EqualArray(A, B: TArray<T>): Boolean; static;
    class function CompareT(const A, B: T): Boolean; static;
    class procedure CopyArray(var FromArray, ToArray: TArray<T>;
      FromIndex: NativeInt = 0; ToIndex: NativeInt = 0;
      Count: NativeInt = -1); static;
    class procedure MoveArray(var AArray: array of T;
      FromIndex, ToIndex, Count: Integer); static;
    class function DynArrayToTArray(const Value: array of T): TArray<T>; static;
    class function Min(A, B: NativeInt): NativeInt; static;
    procedure QuickSort(const Comparer: IComparer<T>; L, R: Integer);
  public // operators
    class operator Implicit(Value: TArray<T>): TArrayEx<T>; overload;
    class operator Implicit(Value: array of T): TArrayEx<T>; overload;
    (*
      这个无解,Delphi不允许array of T作为返回值.也就是这个转换是被废了.只好用AssignTo
      class operator Implicit(Value:  TArrayEx<T>):array of T; overload;
    *)
    class operator Implicit(Value: TArrayEx<T>): TArray<T>; overload;
    class operator Explicit(Value: TArrayEx<T>): TArray<T>; overload;
    class operator Explicit(Value: array of T): TArrayEx<T>; overload;

    class operator Add(const A, B: TArrayEx<T>): TArrayEx<T>; overload;
    class operator Add(const A: TArrayEx<T>; const B: T): TArrayEx<T>; overload;
    class operator Add(const A: T; B: TArrayEx<T>): TArrayEx<T>; overload;
    class operator Add(A: TArrayEx<T>; B: array of T): TArrayEx<T>; overload;
    class operator Add(A: array of T; B: TArrayEx<T>): TArrayEx<T>; overload;
    class operator In (A: T; B: TArrayEx<T>): Boolean; overload;
    //
    class operator Equal(A, B: TArrayEx<T>): Boolean; overload;
    class operator Equal(A: TArrayEx<T>; B: TArray<T>): Boolean; overload;
    class operator Equal(A: TArray<T>; B: TArrayEx<T>): Boolean; overload;
    class operator Equal(A: array of T; B: TArrayEx<T>): Boolean; overload;
    class operator Equal(A: TArrayEx<T>; B: array of T): Boolean; overload;

  public
    procedure SetLen(Value: NativeInt); inline;
    function GetLen: NativeInt; inline;
    function ByteLen: NativeInt; inline;
    class function Create(Value: array of T): TArrayEx<T>; overload; static;
    class function Create(Value: TArrayEx<T>): TArrayEx<T>; overload; static;
    class function Create(const Value: T): TArrayEx<T>; overload; static;
    function Clone(): TArrayEx<T>;
    procedure Clear;
    procedure SetValue(Value: array of T);
    function ToArray(): TArray<T>;
    function SubArray(AFrom, ACount: NativeInt): TArrayEx<T>;
    procedure Delete(AFrom, ACount: NativeInt); overload;
    procedure Delete(AIndex: NativeInt); overload;
    procedure Append(Values: TArrayEx<T>); overload;
    procedure Append(const Value: T); overload;
    procedure Append(Values: array of T); overload;
    procedure Append(Values: TArray<T>); overload;
    function Insert(AIndex: NativeInt; const Value: T): NativeInt; overload;
    function Insert(AIndex: NativeInt; const Values: array of T): NativeInt; overload;
    function Insert(AIndex: NativeInt; const Values: TArray<T>): NativeInt; overload;
    function Insert(AIndex: NativeInt; const Values: TArrayEx<T>): NativeInt; overload;
    procedure Unique();
    // 排序
    procedure Sort(); overload;
    procedure Sort(const Comparer: IComparer<T>); overload;
    procedure Sort(const Comparer: IComparer<T>; Index, Count: Integer); overload;
    // 搜索
    function BinarySearch(const Item: T; out FoundIndex: Integer;
      const Comparer: IComparer<T>; Index, Count: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer;
      const Comparer: IComparer<T>): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer): Boolean; overload;

    property Size: NativeInt read GetLen write SetLen;
    property Len: NativeInt read GetLen write SetLen;
    property RawData: TArray<T> read GetRawData;
    property Elements[Index: Integer]: T read GetElements
      write SetElements; default;
  end;

implementation

uses System.RTLConsts;

class operator TArrayEx<T>.Add(const A, B: TArrayEx<T>): TArrayEx<T>;
begin
  Result := A.Clone;
  Result.Append(B);
end;

class operator TArrayEx<T>.Add(const A: TArrayEx<T>; const B: T): TArrayEx<T>;
begin
  Result := A.Clone;
  Result.Append(B);
end;

class operator TArrayEx<T>.Add(const A: T; B: TArrayEx<T>): TArrayEx<T>;
begin
  Result.SetValue([A]);
  Result.Append(B);
end;

class operator TArrayEx<T>.Add(A: TArrayEx<T>; B: array of T): TArrayEx<T>;
begin
  Result := A.Clone;
  Result.Append(B);
end;

class operator TArrayEx<T>.Add(A: array of T; B: TArrayEx<T>): TArrayEx<T>;
begin
  Result.FData := DynArrayToTArray(A);
  Result.Append(B);
end;

class operator TArrayEx<T>.In(A: T; B: TArrayEx<T>): Boolean;
var
  Tmp: T;
begin
  Result := False;
  for Tmp in B.FData do
    if CompareT(A, Tmp) then
    begin
      Result := True;
      Break;
    end;
end;

class operator TArrayEx<T>.Equal(A, B: TArrayEx<T>): Boolean;
begin
  Result := EqualArray(A.FData, B.FData);
end;

class operator TArrayEx<T>.Equal(A: TArrayEx<T>; B: TArray<T>): Boolean;
begin
  Result := EqualArray(A.FData, B);
end;

class operator TArrayEx<T>.Equal(A: TArray<T>; B: TArrayEx<T>): Boolean;
begin
  Result := EqualArray(A, B.FData);
end;

class operator TArrayEx<T>.Equal(A: array of T; B: TArrayEx<T>): Boolean;
begin
  Result := EqualArray(DynArrayToTArray(A), B.FData);
end;

class operator TArrayEx<T>.Equal(A: TArrayEx<T>; B: array of T): Boolean;
begin
  Result := EqualArray(A.FData, DynArrayToTArray(B));
end;

function TArrayEx<T>.BinarySearch(const Item: T; out FoundIndex: Integer;
  const Comparer: IComparer<T>; Index, Count: Integer): Boolean;
var
  L, H: Integer;
  mid, cmp: Integer;
begin
  if (Index < Low(FData)) or ((Index > High(FData)) and (Count > 0)) or
    (Index + Count - 1 > High(FData)) or (Count < 0) or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Count = 0 then
  begin
    FoundIndex := Index;
    Exit(False);
  end;

  Result := False;
  L := Index;
  H := Index + Count - 1;
  while L <= H do
  begin
    mid := L + (H - L) shr 1;
    cmp := Comparer.Compare(FData[mid], Item);
    if cmp < 0 then
      L := mid + 1
    else
    begin
      H := mid - 1;
      if cmp = 0 then
        Result := True;
    end;
  end;
  FoundIndex := L;
end;

function TArrayEx<T>.BinarySearch(const Item: T; out FoundIndex: Integer;
  const Comparer: IComparer<T>): Boolean;
begin
  Result := BinarySearch(Item, FoundIndex, Comparer, Low(FData), Length(FData));
end;

function TArrayEx<T>.BinarySearch(const Item: T;
  out FoundIndex: Integer): Boolean;
begin
  Result := BinarySearch(Item, FoundIndex, TComparer<T>.Default, Low(FData),
    Length(FData));
end;

function TArrayEx<T>.ByteLen: NativeInt;
begin
  Result := Length(FData) * Sizeof(T);
end;

class function TArrayEx<T>.Min(A, B: NativeInt): NativeInt;
begin
  Result := A;
  if Result > B then
    Result := B;
end;

class procedure TArrayEx<T>.CopyArray(var FromArray, ToArray: TArray<T>;
  FromIndex, ToIndex, Count: NativeInt);
var
  i: Integer;
begin
  if Count = 0 then
    Exit;
  if Count < 0 then
    Count := Min(Length(FromArray), Length(ToArray));
  if Length(FromArray) < (FromIndex + Count) then
    Count := Length(FromArray) - FromIndex;
  if Length(ToArray) < (ToIndex + Count) then
    Count := Length(ToArray) - ToIndex;

  if Count > 0 then
    for i := 0 to Count - 1 do
      ToArray[ToIndex + i] := FromArray[FromIndex + i];
end;

class procedure TArrayEx<T>.MoveArray(var AArray: array of T;
  FromIndex, ToIndex, Count: Integer);
var
  i: Integer;
begin
  if Count > 0 then
  begin
    if FromIndex < ToIndex then
      for i := Count - 1 downto 0 do
        AArray[ToIndex + i] := AArray[FromIndex + i]
    else if FromIndex > ToIndex then
      for i := 0 to Count - 1 do
        AArray[ToIndex + i] := AArray[FromIndex + i];
  end;
end;

procedure TArrayEx<T>.QuickSort(const Comparer: IComparer<T>; L, R: Integer);
var
  i, J: Integer;
  pivot, temp: T;
begin
  if (Length(FData) = 0) or ((R - L) <= 0) then
    Exit;
  repeat
    i := L;
    J := R;
    pivot := FData[L + (R - L) shr 1];
    repeat
      while Comparer.Compare(FData[i], pivot) < 0 do
        Inc(i);
      while Comparer.Compare(FData[J], pivot) > 0 do
        Dec(J);
      if i <= J then
      begin
        if i <> J then
        begin
          temp := FData[i];
          FData[i] := FData[J];
          FData[J] := temp;
        end;
        Inc(i);
        Dec(J);
      end;
    until i > J;
    if L < J then
      QuickSort(Comparer, L, J);
    L := i;
  until i >= R;
end;

class function TArrayEx<T>.DynArrayToTArray(const Value: array of T): TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, Length(Value));
  for i := Low(Value) to High(Value) do
    Result[i] := Value[i];
end;

class function TArrayEx<T>.EqualArray(A, B: TArray<T>): Boolean;
var
  i: Integer;
begin
  Result := True;
  if A = B then
    Exit;
  if Length(A) <> Length(B) then
  begin
    Result := False;
  end
  else
  begin
    for i := Low(A) to High(A) do
      if not CompareT(A[i], B[i]) then
      begin
        Result := False;
        Break;
      end;
  end;
end;

class function TArrayEx<T>.CompareT(const A, B: T): Boolean;
var
  Compare: IComparer<T>;
begin
  Compare := TComparer<T>.Default;
  Result := Compare.Compare(A, B) = 0;
end;
// class function TArrayEx<T>.CompareT(const A, B: T): Boolean;
// var
// p1, p2: PByte;
// i: Integer;
// begin
// Result := True;
// p1 := PByte(@A);
// p2 := PByte(@B);
// for i := 0 to Sizeof(T) - 1 do
// begin
// //
// if p1^ <> p2^ then
// begin
// Result := False;
// Exit;
// end;
// Inc(p1);
// Inc(p2);
// end;
// end;

function TArrayEx<T>.GetElements(Index: Integer): T;
begin
  Result := FData[Index];
end;

function TArrayEx<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(FData);
end;

function TArrayEx<T>.GetLen: NativeInt;
begin
  Result := Length(FData);
end;

function TArrayEx<T>.GetRawData: TArray<T>;
begin
  Result := FData;
end;

class operator TArrayEx<T>.Implicit(Value: TArrayEx<T>): TArray<T>;
begin
  SetLength(Result, Length(Value.FData));
  CopyArray(Value.FData, Result, 0, 0, Length(Value.FData));
end;

class operator TArrayEx<T>.Explicit(Value: array of T): TArrayEx<T>;
begin
  Result.SetValue(Value);
end;

class operator TArrayEx<T>.Implicit(Value: array of T): TArrayEx<T>;
begin
  Result.SetValue(Value);
end;

class operator TArrayEx<T>.Implicit(Value: TArray<T>): TArrayEx<T>;
begin
  SetLength(Result.FData, Length(Value));
  CopyArray(Value, Result.FData, 0, 0, Length(Value));
end;

class operator TArrayEx<T>.Explicit(Value: TArrayEx<T>): TArray<T>;
begin
  SetLength(Result, Length(Value.FData));
  CopyArray(Value.FData, Result, 0, 0, Length(Value.FData));
end;

procedure TArrayEx<T>.SetElements(Index: Integer; const Value: T);
begin
  FData[Index] := Value;
end;

procedure TArrayEx<T>.SetLen(Value: NativeInt);
begin
  SetLength(FData, Value);
end;

procedure TArrayEx<T>.SetValue(Value: array of T);
begin
  FData := DynArrayToTArray(Value);
end;

procedure TArrayEx<T>.Sort;
begin
  QuickSort(TComparer<T>.Default, Low(FData), High(FData));
end;

procedure TArrayEx<T>.Sort(const Comparer: IComparer<T>; Index, Count: Integer);
begin
  if (Index < Low(FData)) or ((Index > High(FData)) and (Count > 0)) or
    (Index + Count - 1 > High(FData)) or (Count < 0) or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Count <= 1 then
    Exit;
  QuickSort(Comparer, Index, Index + Count - 1);
end;

procedure TArrayEx<T>.Sort(const Comparer: IComparer<T>);
begin
  QuickSort(Comparer, Low(FData), High(FData));
end;

function TArrayEx<T>.ToArray(): TArray<T>;
begin
  SetLength(Result, Length(FData));
  CopyArray(FData, Result, 0, 0, Length(FData));
end;

class function TArrayEx<T>.Create(Value: array of T): TArrayEx<T>;
begin
  Result.SetValue(Value);
end;

class function TArrayEx<T>.Create(Value: TArrayEx<T>): TArrayEx<T>;
begin
  Result := Value.Clone;
end;

class function TArrayEx<T>.Create(const Value: T): TArrayEx<T>;
begin
  Result.SetValue([Value]);
end;

procedure TArrayEx<T>.Clear;
begin
  SetLength(FData, 0);
end;

function TArrayEx<T>.Clone(): TArrayEx<T>;
begin
  Result := SubArray(0, Length(FData));
end;

function TArrayEx<T>.SubArray(AFrom, ACount: NativeInt): TArrayEx<T>;
begin
  SetLength(Result.FData, ACount);
  CopyArray(FData, Result.FData, AFrom, 0, ACount);
end;

procedure TArrayEx<T>.Delete(AFrom, ACount: NativeInt);
begin
  if AFrom >= Length(FData) then
    Exit;
  if (AFrom + ACount) > Length(FData) then
    ACount := Length(FData) - AFrom;

  MoveArray(FData, AFrom + ACount, AFrom, Length(FData) - (AFrom + ACount));
  SetLength(FData, Length(FData) - ACount);
end;

procedure TArrayEx<T>.Delete(AIndex: NativeInt);
begin
  Delete(AIndex, 1);
end;

procedure TArrayEx<T>.Append(Values: TArrayEx<T>);
begin
  Insert(Length(FData), Values);
end;

procedure TArrayEx<T>.Append(Values: TArray<T>);
begin
  Insert(Length(FData), Values);
end;

procedure TArrayEx<T>.Append(const Value: T);
begin
  SetLength(FData, Length(FData) + 1);
  FData[High(FData)] := Value;
end;

procedure TArrayEx<T>.Append(Values: array of T);
begin
  Insert(Length(FData), Values);
end;

function TArrayEx<T>.Insert(AIndex: NativeInt; const Value: T): NativeInt;
var
  i: Integer;
begin
  Result := -1;
  if (AIndex > Length(FData)) or (AIndex < 0) then
    Exit;
  SetLength(FData, Length(FData) + 1);
  MoveArray(FData, AIndex, AIndex + 1, Length(FData) - AIndex);
  FData[AIndex] := Value;
  Result := AIndex;
end;

function TArrayEx<T>.Insert(AIndex: NativeInt; const Values: array of T)
  : NativeInt;
var
  i: Integer;
begin
  SetLength(FData, Length(Values));
  MoveArray(FData, AIndex, AIndex + Length(Values), Length(FData) - AIndex);
  for i := 0 to Length(Values) - 1 do
    FData[AIndex + i] := Values[i];
  Result := AIndex;
end;

function TArrayEx<T>.Insert(AIndex: NativeInt; const Values: TArray<T>)
  : NativeInt;
var
  i: Integer;
begin
  SetLength(FData, Length(FData) + Length(Values));
  MoveArray(FData, AIndex, AIndex + Length(Values), Length(FData) - AIndex);
  for i := 0 to Length(Values) - 1 do
    FData[AIndex + i] := Values[i];
  Result := AIndex;
end;

function TArrayEx<T>.Insert(AIndex: NativeInt; const Values: TArrayEx<T>)
  : NativeInt;
begin
  Result := Insert(AIndex, Values.ToArray);
end;

procedure TArrayEx<T>.Unique();
var
  i, J: Integer;
  Tmp: TArrayEx<T>;
  Flag: Boolean;
begin

  for i := High(FData) downto Low(FData) do
  begin
    Flag := False;
    for J := High(Tmp.FData) downto Low(Tmp.FData) do
    begin
      if CompareT(FData[i], Tmp[J]) then
      begin
        Flag := True;
        Break;
      end;
    end;
    if not Flag then
      Tmp.Append(FData[i]);
  end;
  FData := Tmp.FData;
end;

{ TArrayEx<T>.TEnumerator }

constructor TArrayEx<T>.TEnumerator.Create(const AValue: TArray<T>);
begin
  FValue := AValue;
  FIndex := -1;
end;

function TArrayEx<T>.TEnumerator.GetCurrent: T;
begin
  Result := FValue[FIndex];
end;

function TArrayEx<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := False;
  if (FIndex >= Length(FValue)) then
    Exit;

  Inc(FIndex);
  Result := FIndex < Length(FValue);
end;

end.
