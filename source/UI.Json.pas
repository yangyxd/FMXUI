unit UI.Json;

interface

uses
  System.JSON, System.SysUtils;

type
  TJSONObjectHelper = class Helper for System.JSON.TJSONObject
  private
    procedure SetBoolean(const Key: string; const Value: Boolean);
    procedure SetFloat(const Key: string; const Value: Double);
    procedure SetInt64(const Key: string; const Value: Int64);
    procedure SetJsonObject(const Key: string; const Value: TJSONObject);
    procedure SetString(const Key, Value: string);
    procedure SetJsonArray(const Key: string; const Value: TJSONArray);
    procedure SetDateTime(const Key: string; const Value: TDateTime);
  public
    procedure Parse(const Value: string); overload;

    function Exist(const Key: string): Boolean;

    procedure Add(const Key: string; const Value: string; const DefaultValue: string = ''); overload;
    procedure Add(const Key: string; const Value: Boolean; const DefaultValue: Boolean = False); overload;
    procedure Add(const Key: string; const Value: Int64; const DefaultValue: Int64 = 0); overload;
    procedure Add(const Key: string; const Value: Double; const DefaultValue: Double = 0); overload;
    procedure AddDateTime(const Key: string; const Value: TDateTime; const DefaultValue: TDateTime = 0); overload;
    function AddJsonArray(const Key: string): TJSONArray; overload;
    function AddJsonObject(const Key: string): TJSONObject; overload;

    function GetBoolean(const Key: string): Boolean;
    function GetFloat(const Key: string): Double;
    function GetInt64(const Key: string): Int64;
    function GetString(const Key: string): string;
    function GetDateTime(const Key: string): TDateTime;
    function GetJsonArray(const Key: string): TJSONArray;
    function GetJsonObject(const Key: string): TJSONObject;

    function TryGetBoolean(const Key: string; var Value: Boolean): Boolean;
    function TryGetFloat(const Key: string; var Value: Double): Boolean; overload;
    function TryGetFloat(const Key: string; var Value: Single): Boolean; overload;
    function TryGetInt(const Key: string; var Value: Integer): Boolean; overload;
    function TryGetInt(const Key: string; var Value: Int64): Boolean; overload;
    function TryGetInt(const Key: string; var Value: NativeInt): Boolean; overload;
    function TryGetInt(const Key: string; var Value: Cardinal): Boolean; overload;
    function TryGetString(const Key: string; var Value: string): Boolean;
    function TryGetDateTime(const Key: string; var Value: TDateTime): Boolean;

    property S[const Key: string]: string read GetString write SetString;
    property I[const Key: string]: Int64 read GetInt64 write SetInt64;
    property F[const Key: string]: Double read GetFloat write SetFloat;
    property B[const Key: string]: Boolean read GetBoolean write SetBoolean;
    property D[const Key: string]: TDateTime read GetDateTime write SetDateTime;
    property O[const Key: string]: TJSONObject read GetJsonObject write SetJsonObject;
    property A[const Key: string]: TJSONArray read GetJsonArray write SetJsonArray;
  end;

implementation

{ TJSONObjectHelper }

procedure TJSONObjectHelper.Add(const Key: string; const Value: Double; const DefaultValue: Double);
begin
  if Value = DefaultValue then
    Exit;
  Self.AddPair(Key, TJSONNumber.Create(Value));
end;

procedure TJSONObjectHelper.Add(const Key: string; const Value: Int64; const DefaultValue: Int64);
begin
  if Value = DefaultValue then
    Exit;
  Self.AddPair(Key, TJSONNumber.Create(Value));
end;

procedure TJSONObjectHelper.Add(const Key, Value, DefaultValue: string);
begin
  if Value = DefaultValue then
    Exit;
  Self.AddPair(Key, Value);
end;

procedure TJSONObjectHelper.Add(const Key: string; const Value, DefaultValue: Boolean);
begin
  if Value = DefaultValue then
    Exit;
  Self.AddPair(Key, TJSONBool.Create(Value));
end;

procedure TJSONObjectHelper.AddDateTime(const Key: string; const Value, DefaultValue: TDateTime);
begin
  if Value = DefaultValue then
    Exit;
  Self.AddPair(Key, FormatDateTime('yyyy-mm-dd hh:nn:ss', Value));
end;

function TJSONObjectHelper.AddJsonArray(const Key: string): TJSONArray;
begin
  Result := TJSONArray.Create;
  Self.AddPair(Key, Result);
end;

function TJSONObjectHelper.AddJsonObject(const Key: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  Self.AddPair(Key, Result);
end;

function TJSONObjectHelper.Exist(const Key: string): Boolean;
begin
  Result := Assigned(GetValue(Key));
end;

function TJSONObjectHelper.GetBoolean(const Key: string): Boolean;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) then
    Result := V.GetValue<Boolean>()
  else
    Result := False;
end;

function TJSONObjectHelper.GetDateTime(const Key: string): TDateTime;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) then
    Result := V.GetValue<TDateTime>()
  else
    Result := 0;
end;

function TJSONObjectHelper.GetFloat(const Key: string): Double;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) then
    Result := V.GetValue<Double>()
  else
    Result := 0;
end;

function TJSONObjectHelper.GetInt64(const Key: string): Int64;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) then
    Result := V.GetValue<Int64>()
  else
    Result := 0;
end;

function TJSONObjectHelper.GetJsonArray(const Key: string): TJSONArray;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) then begin
    if V is TJSONArray then
      Result := V as TJSONArray
    else
      Result := nil;
  end else
    Result := nil;
end;

function TJSONObjectHelper.GetJsonObject(const Key: string): TJSONObject;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) and (V is TJSONObject) then
    Result := V as TJSONObject
  else
    Result := nil;
end;

function TJSONObjectHelper.GetString(const Key: string): string;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) then
    Result := V.GetValue<string>()
  else
    Result := '';
end;

procedure TJSONObjectHelper.Parse(const Value: string);
var
  V: TArray<Byte>;
begin
  V := TEncoding.Default.GetBytes(Value);
  Self.Parse(V, 0);
end;

procedure TJSONObjectHelper.SetBoolean(const Key: string; const Value: Boolean);
begin
  RemovePair(Key);
  Add(Key, Value);
end;

procedure TJSONObjectHelper.SetDateTime(const Key: string;
  const Value: TDateTime);
begin
  RemovePair(Key);
  Add(Key, Value);
end;

procedure TJSONObjectHelper.SetFloat(const Key: string; const Value: Double);
begin
  RemovePair(Key);
  Add(Key, Value);
end;

procedure TJSONObjectHelper.SetInt64(const Key: string; const Value: Int64);
begin
  RemovePair(Key);
  Add(Key, Value);
end;

procedure TJSONObjectHelper.SetJsonArray(const Key: string;
  const Value: TJSONArray);
begin
  RemovePair(Key);
  AddPair(Key, Value);
end;

procedure TJSONObjectHelper.SetJsonObject(const Key: string;
  const Value: TJSONObject);
begin
  RemovePair(Key);
  AddPair(Key, Value);
end;

procedure TJSONObjectHelper.SetString(const Key, Value: string);
begin
  RemovePair(Key);
  Add(Key, Value);
end;

function TJSONObjectHelper.TryGetBoolean(const Key: string;
  var Value: Boolean): Boolean;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) then begin
    Result := True;
    Value := V.GetValue<Boolean>()
  end else
    Result := False;
end;

function TJSONObjectHelper.TryGetDateTime(const Key: string;
  var Value: TDateTime): Boolean;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) then begin
    Result := True;
    Value := V.GetValue<TDateTime>()
  end else
    Result := False;
end;

function TJSONObjectHelper.TryGetFloat(const Key: string;
  var Value: Double): Boolean;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) then begin
    Result := True;
    Value := V.GetValue<Double>()
  end else
    Result := False;
end;

function TJSONObjectHelper.TryGetFloat(const Key: string;
  var Value: Single): Boolean;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) then begin
    Result := True;
    Value := V.GetValue<Single>()
  end else
    Result := False;
end;

function TJSONObjectHelper.TryGetInt(const Key: string;
  var Value: NativeInt): Boolean;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) then begin
    Result := True;
    Value := V.GetValue<NativeInt>()
  end else
    Result := False;
end;

function TJSONObjectHelper.TryGetInt(const Key: string;
  var Value: Int64): Boolean;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) then begin
    Result := True;
    Value := V.GetValue<Int64>()
  end else
    Result := False;
end;

function TJSONObjectHelper.TryGetInt(const Key: string;
  var Value: Integer): Boolean;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) then begin
    Result := True;
    Value := V.GetValue<Integer>()
  end else
    Result := False;
end;

function TJSONObjectHelper.TryGetInt(const Key: string;
  var Value: Cardinal): Boolean;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) then begin
    Result := True;
    Value := V.GetValue<Cardinal>()
  end else
    Result := False;
end;

function TJSONObjectHelper.TryGetString(const Key: string;
  var Value: string): Boolean;
var
  V: TJSONValue;
begin
  V := GetValue(Key);
  if Assigned(V) then begin
    Result := True;
    Value := V.GetValue<string>()
  end else
    Result := False;
end;

end.
