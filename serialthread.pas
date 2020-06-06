unit serialthread;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, syncobjs,
  {$ifdef HASAMIGA}
  Exec, AmigaDos,
  {$endif}
  Math, dateutils;

const
  {$ifndef HASAMIGA}
  CMD_NONSTD = 0;
  {$endif}

  DEFAULTDEVICENAME = 'usbmodem.device';
  DEFAULTUNITNUMBER = 0;
  DEFAULTBAUDRATE = 4800;
  //
  TERMINATORA = $0a030303;
  TERMINATORB = $03030303;
  //
  SDCMD_QUERY = CMD_NONSTD;     // $09
  SDCMD_BREAK = CMD_NONSTD + 1; // $0A
  SDCMD_SETPARAMS = CMD_NONSTD + 2; // $0B

  SERB_XDISABLED = 7;	    // xOn-xOff feature disabled bit
  SERF_XDISABLED = 1 shl 7;	// xOn-xOff feature disabled mask
  SERB_EOFMODE = 6;         // EOF mode enabled bit
  SERF_EOFMODE = 1 shl 6;   // EOF mode enabled mask
  SERB_PARTY_ON	= 0;	    // parity-enabled bit
  SERF_PARTY_ON	= 1 shl 0;  // parity-enabled mask
  
type
  TIOTArray = record
    TermArray0: LongWord;
    TermArray1: LongWord;
  end;

  {$ifdef HASAMIGA}
  TIOExtSer = record
    IOSer: TIOStdReq;
    io_CtlChar: LongWord;    // control characters */
    io_RBufLen: LongWord;    //;    /* length in bytes of serial read buffer */
    io_ExtFlags: LongWord;    //;   /* additional serial flags */
    io_Baud: LongWord;    //;       /* baud rate */
    io_BrkTime: LongWord;    //;    /* duration of break in microseconds */
    io_TermArray: TiOTArray;  //* termination character array */
    io_ReadLen: Byte;    //;    /* number of bits per read character */
    io_WriteLen: Byte;    //;   /* number of bits per write character */
    io_StopBits: Byte;    //;   /* number of stopbits for read */
    io_SerFlags: Byte;    //;   /* serial device flags */
    io_Status: Word;    //;     /* status of serial port and lines */
  end;
  PIOExtSer = ^TIOExtSer; 
  {$endif}

  TGPSData = record
    Lan, Lon: Double;
    Speed: Double;
    Date: TDateTime;
    Height: Double;
    NumSatelites: Integer;
    SatID: array of record
      ID: Integer;
      Idx: Integer;
    end;
    CurSats: Boolean;
    Sats: array[Boolean] of array of record
      ID: Integer;
      Elevation: Integer;
      Azimuth: Integer;
      SNR: Integer;
      Idx: Integer;
    end;
  end;

  { TSerThread }

  TSerThread = class(TThread)
  private
    {$ifdef HASAMIGA}
    mp: PMsgPort;
    io: PIOExtSer;
    iod: PIORequest;
    {$endif}
    DeviceName: string;
    UnitNumber: Integer;
    BaudRate: Integer;
    DevOpen: Boolean;
    IORunning: Boolean;
    Data: TGPSData;
    Crit: TCriticalSection;
    procedure GetText(SL: TStringList);
    procedure GetRMC(SL: TStringList);
    procedure GetVTG(SL: TStringList);
    procedure GetGLL(SL: TStringList);
    procedure GetGGA(SL: TStringList);
    procedure GetGSA(SL: TStringList);
    procedure GetGSV(SL: TStringList);
    procedure ParseMsg(Msg: string);
  protected
    procedure InitSerial;
    procedure FinishSerial;
    procedure Execute; override;
  public
    InitDone: Boolean;
    constructor Create(ADeviceName: string; AUnitNumber: Integer; ABaudRate: Integer); reintroduce;
    destructor Destroy; override;
    procedure GetData(var ExData: TGPSData);
  end;

  procedure DebugOut(Msg: String);


implementation

procedure DebugOut(Msg: String);
begin
  {$ifdef HASAMIGA}
  sysdebugln(Msg);
  {$else}
  writeln(Msg);
  {$endif}
end;

{$ifdef HASAMIGA}
function CreateExtIO(const Mp: PMsgPort; Size: Integer): PIORequest;
begin
  Result := nil;
  if not Assigned(mp) then
    Exit;
  Result := System.AllocMem(Size);
  if Assigned(Result) then
  begin
    Result^.io_Message.mn_Node.ln_Type := NT_REPLYMSG;
    Result^.io_Message.mn_ReplyPort := Mp;
    Result^.io_Message.mn_Length := Size;
  end;  
end;

procedure DeleteExtIO(ioReq: PIORequest);
begin
  if Assigned(ioReq) then
  begin
    ioReq^.io_Message.mn_Node.ln_Type := Byte(-1);
    ioReq^.io_Device := Pointer(-1);
    ioReq^.io_Unit := Pointer(-1);
    System.FreeMem(ioReq);
  end;
end;
{$endif}

procedure TSerThread.InitSerial;
var
  Res: Integer;
begin
  InitDone := False;
  {$ifdef HASAMIGA}
  mp := nil;
  io := nil;
  iod := nil;
  DevOpen := False;
  IORunning := False;
  
  mp := CreateMsgPort;
  if not Assigned(Mp) then
  begin
    DebugOut('Error open MessagePort');
    Exit;
  end;
  //
  io := PIOExtSer(CreateExtIO(mp, sizeof(TIOExtSer)));
  if not assigned(io) then
  begin
    DebugOut('cannot alloc io');
    Exit;
  end;
  iod := Pointer(io);
  Res := OpenDevice(PChar(DeviceName), UnitNumber, iod,0);
  if Res <> 0 then
  begin
    DebugOut('unable to open device ' +  IntToStr(Res));
    Exit;
  end;
  DevOpen := True;

  io^.io_SerFlags := (io^.io_SerFlags or SERF_EOFMODE or SERF_XDISABLED) and (not SERF_PARTY_ON);
  io^.io_Baud := BaudRate;
  io^.io_TermArray.TermArray0 := TERMINATORA;
  io^.io_TermArray.TermArray1 := TERMINATORB;
  io^.IOSer.io_Command := SDCMD_SETPARAMS;
  Res := DoIO(iod);
  if Res <> 0 then
  begin
    DebugOut('Error set params ' + IntToStr(Res));
    Exit;
  end;
  {$endif}
  InitDone := True;
end;

procedure TSerThread.FinishSerial;
begin
  {$ifdef HASAMIGA}
  if IORunning then
  begin
    AbortIO(iod);
    WaitIO(iod);
  end;  
  if DevOpen then
    CloseDevice(iod);
  if Assigned(io) then
    DeleteExtIO(PIORequest(io));
  if Assigned(Mp) then
    DeleteMsgPort(Mp);
  {$endif}
end;

procedure TSerThread.GetText(SL: TStringList);
begin
  if SL.Count > 4 then
  begin
    DebugOut('TextMessage: ' + SL[4]);
  end;
end;


function NMEAtoDegree(NStr: string): Double;
var
  Deg: Integer;
begin
  Result := StrToFloatDef(NStr, Nan);
  if IsNan(Result) then
    Exit;
  Result := Result / 100;
  Deg := Trunc(Result);
  Result := Result - Deg;
  Result := (Result * 100)/60;
  Result := Deg + Result;
end;

procedure TSerThread.GetRMC(SL: TStringList);
var
  la, lo: Double;
  ti, da: TDateTime;
  str: string;
  P1: Integer;
  h,m,s: SmallInt;
  y,mo,d: SmallInt;
begin
  if SL.Count > 9 then
  begin
    la := NMEAtoDegree(SL[3]);
    if isNan(la) then
      Exit;
    if SL[4] = 'S' then
      la := -la;
    Lo := NMEAtoDegree(SL[5]);
    if isNan(lo) then
      Exit;
    if SL[6] = 'W' then
      lo := -lo;
    // time
    str := SL[1];
    P1 := Pos('.', str);
    if P1 > 0 then
      Delete(str, P1, Length(str));
    if Length(str) = 6 then
    begin
      h := StrToIntDef(Copy(str, 1, 2), -1);
      m := StrToIntDef(Copy(str, 3, 2), -1);
      s := StrToIntDef(Copy(str, 5, 2), -1);
    end;
    str := SL[9];
    if Length(str) = 6 then
    begin
      d := StrToIntDef(Copy(str, 1, 2), -1);
      mo := StrToIntDef(Copy(str, 3, 2), -1);
      y := StrToIntDef(Copy(str, 5, 2), -1);
    end;
    ti := 1.1;
    try
      if (h >= 0) and (m >= 0) and (s >= 0) and (d >= 0) and (mo >= 0) and (y >= 0) then
      begin
        Ti := EncodeDateTime(y, mo, d, h, m, s, 0);
        Data.Date := ti;
      end;
    except
    end;
    Data.Lan := la;
    Data.Lon := lo;
    DebugOut('Position: ' + FloatToStrF(Data.Lan, ffFixed, 8, 6) + ';' + FloatToStrF(Data.Lon, ffFixed, 8, 6) + ' ' + DateTimeToStr(ti));
  end;
end;

procedure TSerThread.GetVTG(SL: TStringList);
var
  sp: Double;
begin
  if SL.Count > 7 then
  begin
    sp := StrToFloatDef(SL[6], Nan);
    if IsNan(sp) then
    begin
      DebugOut('speed ' + SL[6]);
      Exit;
    end;  
    Data.Speed := sp;
    DebugOut('Speed: ' + FloatToStrF(sp, ffFixed, 8,2) + ' km/h');
  end;
end;

procedure TSerThread.GetGLL(SL: TStringList);
var
  la, lo: Double;
  ti, da: TDateTime;
  str: string;
  P1: Integer;
  h,m,s: SmallInt;
begin
  if SL.Count > 6 then
  begin
    la := NMEAtoDegree(SL[1]);
    if isNan(la) then
      Exit;
    if SL[2] = 'S' then
      la := -la;
    lo := NMEAtoDegree(SL[3]);
    if isNan(lo) then
      Exit;
    if SL[4] = 'W' then
      lo := -lo;
    // time
    str := SL[5];
    P1 := Pos('.', str);
    if P1 > 0 then
      Delete(str, P1, Length(str));
    if Length(str) = 6 then
    begin
      h := StrToIntDef(Copy(str, 1, 2), -1);
      m := StrToIntDef(Copy(str, 3, 2), -1);
      s := StrToIntDef(Copy(str, 5, 2), -1);
    end;
    ti := 1.1;
    try
    if (h >= 0) and (m >= 0) and (s >= 0) then
      Ti := EncodeTime(h, m, s, 0);
    except
    end;
    DebugOut('Pos: ' + FloatToStrF(Data.Lan, ffFixed, 8, 6) + ';' + FloatToStrF(Data.Lon, ffFixed, 8, 6) + ' ' + TimeToStr(ti));
  end;
end;

procedure TSerThread.GetGGA(SL: TStringList);
var
  la, lo: Double;
  ti, da: TDateTime;
  str: string;
  P1: Integer;
  h,m,s: SmallInt;
  y,mo,d: SmallInt;
  Sat: Integer;
  Hei: Double;
begin
  if SL.Count > 9 then
  begin
    la := NMEAtoDegree(SL[2]);
    if isNan(la) then
      Exit;
    if SL[3] = 'S' then
      la := -la;
    lo := NMEAtoDegree(SL[4]);
    if isNan(lo) then
      Exit;
    if SL[5] = 'W' then
      lo := -lo;
    // time
    str := SL[1];
    P1 := Pos('.', str);
    if P1 > 0 then
      Delete(str, P1, Length(str));
    if Length(str) = 6 then
    begin
      h := StrToIntDef(Copy(str, 1, 2), -1);
      m := StrToIntDef(Copy(str, 3, 2), -1);
      s := StrToIntDef(Copy(str, 5, 2), -1);
    end;
    str := SL[4];
    if Length(str) = 6 then
    begin
      d := StrToIntDef(Copy(str, 1, 2), -1);
      mo := StrToIntDef(Copy(str, 3, 2), -1);
      y := StrToIntDef(Copy(str, 5, 2), -1);
    end;
    ti := 1.1;
    try
    if (h >= 0) and (m >= 0) and (s >= 0) and (d >= 0) and (mo >= 0) and (y >= 0) then
    begin
      Ti := EncodeDateTime(y, mo, d, h, m, s, 0);
      Data.Date := ti;
    end;
    except
    end;
    Data.Lan := la;
    Data.Lon := lo;
    DebugOut('Position: ' + FloatToStrF(Data.Lan, ffFixed, 8, 6) + ';' + FloatToStrF(Data.Lon, ffFixed, 8, 6) + ' ' + DateTimeToStr(ti));
    //
    Sat := StrToIntDef(SL[7], -1);
    Hei := StrToFloatDef(SL[9], Nan);
    if Sat >= 0 then
      Data.NumSatelites := Sat;
    if not isNan(Hei) then
      Data.Height := Hei;
    if (Sat >= 0) and (not isNan(Hei)) then
      DebugOut('Height: ' + FloatToStrF(Hei, ffFixed, 8,1) + 'm ; NumSat: ' + IntToStr(Sat) + '/12');
      
  end;
end;

procedure TSerThread.GetGSA(SL: TStringList);
var
  i: Integer;
  str: string;
begin
  if SL.Count > 14 then
  begin
    str := 'SatIDs: ';
    for i:= 1 to 12 do
    begin
      Data.SatID[i].ID := StrToIntDef(SL[2 + i], -1);
      str := str + IntToStr(Data.SatID[i].ID) + ', ';
    end;
    DebugOut(str);
    DebugOut('Mode ' + SL[2]);
  end;
end;

procedure TSerThread.GetGSV(SL: TStringList);
var
  Idx, i: Integer;
  Num, Elevation, Azimuth, SNR: Integer;
begin
  Idx := 4;
  if SL.Count < 3 then
    Exit;
  if StrToIntDef(SL[2], -1) = 1 then
  begin
    Data.CurSats := not Data.CurSats;
    SetLength(Data.Sats[Data.CurSats], 0);
  end;
  while (Idx + 4) < SL.Count do
  begin
    // Number
    Num := StrToIntDef(SL[Idx], -1);
    Inc(Idx);    
    // Elevation
    Elevation := StrToIntDef(SL[Idx], -1);
    Inc(Idx);
    // Azimuth
    Azimuth := StrToIntDef(SL[Idx], -1);
    Inc(Idx);
    // SNR in db
    SNR := StrToIntDef(SL[Idx], -1);
    Inc(Idx);
    i := Length(Data.Sats[Data.CurSats]);
    SetLength(Data.Sats[Data.CurSats], i + 1);
    Data.Sats[Data.CurSats,i].ID := Num;
    Data.Sats[Data.CurSats,i].Elevation := Elevation;
    Data.Sats[Data.CurSats,i].Azimuth := Azimuth;
    Data.Sats[Data.CurSats,i].SNR := SNR;
    DebugOut(IntToStr(i) + '. Found: ' + IntToStr(Num) + ' Ele: ' + IntToStr(Elevation) + ' Azi: ' + IntToStr(Azimuth));
    DebugOut('size: ' + IntToStr(Length(Data.Sats[Data.CurSats])));
  end;
end;

procedure TSerThread.ParseMsg(Msg: string);
var
  SL: TStringList;
  P1: Integer;
begin
  if (Length(Msg) > 5) and (Msg[1] = '$') then
  begin
    SL := TStringList.Create;
    try
      Delete(Msg, 1,3); // remove $GP
      P1 := Pos('*', Msg);
      if P1 > 0 then
        Delete(Msg, P1, Length(Msg)); // Remove Checksum
      //  
      //ExtractStrings([','], [], PChar(Msg), SL);
      SL.Text := StringReplace(Msg, ',', #13#10, [rfReplaceAll]);
      if SL.Count = 0 then
        Exit;
      //
      Crit.Enter;
      try
        DebugOut(Msg);
        case SL[0] of
          'TXT': GetText(SL);
          'RMC': GetRMC(SL);
          'VTG': GetVTG(SL);
          'GLL': GetGLL(SL);
          'GGA': GetGGA(SL);
          'GSA': GetGSA(SL);
          'GSV': GetGSV(SL);
          else
            DebugOut('Unknown ' + SL[0] + ' "' + Msg + '"');
        end;
      finally
        Crit.Leave;
      end;

      //
    finally
      SL.Free;
    end;
  end;
end;

procedure TSerThread.Execute;
var
  Buffer: array[0..256] of Char;
begin
  InitSerial;
  if not InitDone then
  begin
    Terminate;
    Exit;
  end;  
  try
    repeat
      {$ifdef HASAMIGA}
      FillChar(Buffer[0], 257, #0);
      //
      io^.IOSer.io_Length := 256;
      io^.IOSer.io_Data := @Buffer[0];
      io^.IOSer.io_Command := CMD_READ;
      IORunning := True;
      SendIO(iod);
      //
      while CheckIO(iod) = nil do
      begin
        Sleep(10);
        if Terminated then
	  Break;
      end;
      IORunning := False;
      WaitIO(iod);
      ParseMsg(Buffer);
      //write(Buffer);
    {$else}
    Sleep(25)
    {$endif}
    until Terminated;

    Terminate;
  except
    on E:Exception do
      DebugOut('Exception in SerialTask: ' + E.Message);
  end;
  FinishSerial;
end;

constructor TSerThread.Create(ADeviceName: string; AUnitNumber: Integer; ABaudRate: Integer);
begin
  DebugOut('Create serial with Device: ' + ADevicename + ' Unit: ' + IntToStr(AUnitNumber) + ' BaudRate: ' + IntToStr(ABaudRate));
  Crit := TCriticalSection.Create;
  DeviceName := ADeviceName;
  UnitNumber := AUnitNumber;
  BaudRate := ABaudRate;
  SetLength(Data.SatID, 13);
  inherited Create(True);
  Start;
end;

destructor TSerThread.Destroy;
begin
  Crit.Free;
  inherited Destroy;
end;

procedure TSerThread.GetData(var ExData: TGPSData);
begin
  Crit.Enter;
  ExData.CurSats := True;
  ExData.Sats[ExData.CurSats] := Copy(Data.Sats[not Data.CurSats], 0, Length(Data.Sats[not Data.CurSats]));
  ExData.Height := Data.Height;
  ExData.Date := Data.Date;
  ExData.Lan := Data.Lan;
  ExData.Lon := Data.Lon;
  ExData.SatID := Copy(Data.SatID, 0, Length(Data.SatID));
  ExData.NumSatelites := Data.NumSatelites;
  ExData.Speed := Data.Speed;
  Crit.Leave;
end;

initialization
  FormatSettings.DecimalSeparator := '.';
finalization

end.
