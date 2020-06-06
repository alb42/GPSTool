unit GPSWinUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, SerialThread, Math;

type

  { TGPSForm }

  TGPSForm = class(TForm)
    SatPower: TPaintBox;
    SatView: TPaintBox;
    Panel1: TPanel;
    SatPanel: TPanel;
    ValueGrid: TStringGrid;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SatPowerPaint(Sender: TObject);
    procedure SatViewPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    SerThread: TSerThread;
    Data: TGPSData;
  public
    { public declarations }
  end;

var
  GPSForm: TGPSForm;

implementation

{$ifdef HASAMIGA}
uses
  icon, workbench;
{$endif}

{$R *.lfm}

type
  TToolTypeArray= array of AnsiString;

{$ifdef HASAMIGA}
function GetToolTypes(Filename: AnsiString): TToolTypeArray;
var
  DObj: PDiskObject;
  Tooltype: PPChar;
  Idx: Integer;
begin
  SetLength(GetToolTypes, 0);
  DObj := GetDiskObject(PChar(FileName));
  if not Assigned(Dobj) then
    Exit;
  Tooltype := DObj^.do_Tooltypes;
  while Assigned(ToolType^) do
  begin
    Idx := Length(GetToolTypes);
    SetLength(GetToolTypes, Idx + 1);
    GetToolTypes[Idx] := ToolType^;
    Inc(ToolType);
  end;
  FreeDiskObject(DObj);
end;
{$else}
function GetToolTypes(Filename: AnsiString): TToolTypeArray;
begin
  SetLength(Result, 0);
end;

{$endif}

{ TGPSForm }

procedure TGPSForm.FormCreate(Sender: TObject);
var
  Dev: string;
  Un, Bau, i: Integer;
  Tta: TToolTypeArray;
  Temp: string;
begin
  Dev := DEFAULTDEVICENAME;
  Un := DEFAULTUNITNUMBER;
  Bau := DEFAULTBAUDRATE;
  if ParamCount >= 1 then
    Dev := ParamStr(1);
  if ParamCount >= 2 then
    Un := StrToIntDef(ParamStr(2), Un);
  if ParamCount >= 3 then
    Bau := StrToIntDef(ParamStr(3), Bau);
  Tta := GetToolTypes(ParamStr(0));
  for i := 0 to High(Tta) do
  begin
    Temp := Tta[i];
    if Pos('DEVICE=', Temp) = 1 then
    begin
      Delete(Temp, 1, 7);
      Dev := Trim(Temp);
      Continue;
    end;
    if Pos('UNIT=', Temp) = 1 then
    begin
      Delete(Temp, 1, 5);
      Un := StrToIntDef(Temp, Un);
      Continue;
    end;
    if Pos('BAUD=', Temp) = 1 then
    begin
      Delete(Temp, 1, 5);
      Bau := StrToIntDef(Temp, Bau);
      Continue;
    end;
  end;
  SerThread := TSerThread.Create(Dev, Un, Bau);
end;

procedure TGPSForm.FormDestroy(Sender: TObject);
begin
  SerThread.Terminate;
  SerThread.WaitFor;
  SerThread.Free;
end;

procedure TGPSForm.SatPowerPaint(Sender: TObject);
var
  Hi, i, Incr, Offset, YOff, SNR, SigHeight: Integer;
  Ca: TCanvas;
  Str: string;
  Col: TColor;
begin
  Ca := SatPower.Canvas;
  Ca.Brush.Color := clWhite;
  Ca.Brush.Style := bsSolid;
  Ca.FillRect(SatPower.ClientRect);
  Incr := (SatPower.Width - 50) div 12;
  Offset := 0;
  Hi := Ca.TextHeight('1');
  Ca.Brush.Color := clBlue;
  Ca.Brush.Style := bsSolid;
  Ca.Font.Color := clBlue;
  for i := 1 to High(Data.SatID) do
  begin
    YOff := SatPower.Height - (Hi + 5);
    Offset := Offset + Incr;
    Str := '--';
    if Data.SatID[i].ID >= 0 then
    begin
      Str := IntToStr(Data.SatID[i].ID);
      if Data.SatID[i].ID < 10 then
        Str := '0' + Str;
    end;
    SNR := 0;
    if Data.SatID[i].Idx >= 0 then
      SNR := Data.Sats[True, Data.SatID[i].Idx].SNR;
    if Data.SatID[i].ID >= 0 then
    begin
      if SNR = 0 then
        Col := clRed
      else
        Col := clBlue;
      end
    else
    begin
      Col := clDkGray;
    end;
    Ca.Brush.Color := Col;
    Ca.Font.Color := Col;
    Ca.Brush.Style := bsClear;
    Ca.TextOut(Offset,  YOff, Str);
    YOff := YOff - Hi;
    SigHeight := Round(Max(1, (SNR/99) * (YOff - 5)));
    Ca.Brush.Style := bsSolid;
    Ca.FillRect(Offset, YOff - SigHeight, Offset + Incr div 2, YOff);
  end;
end;

procedure TGPSForm.SatViewPaint(Sender: TObject);
var
  Ca: TCanvas;
  i: Integer;
  s, c, Scale: Single;
  Hi, x,y: Integer;
  Col: TColor;
  Str: string;
begin
  Ca := SatView.Canvas;
  Ca.Brush.Color := clWhite;
  Ca.Brush.Style := bsSolid;
  Ca.FillRect(SatView.ClientRect);
  Ca.Brush.Style := bsClear;
  Ca.Pen.Color := clBlack;
  Ca.Pen.Mode := pmCopy;
  Ca.Line(128, 0, 128, SatView.ClientHeight);
  Ca.Line(0, 128, SatView.ClientWidth, 128);
  Ca.EllipseC(128, 128, 127, 127);
  Ca.EllipseC(128, 128, 63, 63);
  Ca.EllipseC(128, 128, 2, 2);
  Hi := CA.TextHeight('1234567890');
  Ca.Brush.Color := clBlue;
  Ca.Pen.Color := clBlue;
  Ca.Font.Color := clBlue;
  for i := 0 to High(Data.Sats[True]) do
  begin
    if Data.Sats[True, i].ID > 0 then
    begin
      Ca.Brush.Style := bsSolid;
      sincos(DegToRad(Data.Sats[True, i].Azimuth), s, c);
      Scale := ((90 - Data.Sats[True, i].Elevation) / 90) * 128;
      x := Round((s * Scale) + 128);
      y := Round((-c * Scale) + 128);
      DebugOut('Sats '+ IntToStr(i) + ' ' + IntToStr(Data.Sats[True, i].ID) + ' ' + IntToStr(Data.Sats[True, i].Azimuth) + ';' + IntToStr(Data.Sats[True, i].Elevation));
      DebugOut('     ' + IntToStr(X) + ';' + IntToStr(y));
      if Data.Sats[True, i].Idx < 0 then
        Col := clDkGray
      else
        Col := clBlue;
      Ca.Brush.Color := Col;
      Ca.Pen.Color := Col;
      Ca.Font.Color := Col;
      Ca.FillRect(X - 4,y - 4, x + 4, y + 4);
      Ca.Brush.Style := bsClear;
      Str := IntToStr(Data.Sats[True, i].ID);
      if Data.Sats[True, i].ID < 10 then
        Str := '0' + Str;
      Ca.TextOut(X - 4, y - Hi - 6, Str);
    end;
  end;
end;

procedure TGPSForm.Timer1Timer(Sender: TObject);
var
  i, j: Integer;
begin
  if SerThread.InitDone then
  begin
    SerThread.GetData(Data);
    ValueGrid.Cells[1, 0] := FloatToStrF(Data.Lan, ffFixed, 8,6) + ';' + FloatToStrF(Data.Lon, ffFixed, 8,6);
    ValueGrid.Cells[1, 1] := FloatToStrF(Data.Height, ffFixed, 8,1) + ' m';
    ValueGrid.Cells[1, 2] := FloatToStrF(Data.Speed, ffFixed, 8,1) + ' km/h';
    ValueGrid.Cells[1, 3] := IntToStr(Data.NumSatelites) + ' / 12';
    ValueGrid.Cells[1, 4] := DateTimeToStr(Data.Date);
    {$ifndef HASAMIGA}
    SetLength(Data.Sats[True], 2);
    Data.Sats[True, 0].Elevation := 69;
    Data.Sats[True, 0].Azimuth := 0;
    Data.Sats[True, 0].ID := 12;
    Data.Sats[True, 0].SNR := 12;
    Data.Sats[True, 1].Elevation := 45;
    Data.Sats[True, 1].Azimuth := 45;
    Data.Sats[True, 1].ID := 11;
    Data.Sats[True, 1].SNR := 50;
    Data.SatID[1].ID := 12;
    Data.SatID[1].Idx := 0;
    Data.SatID[2].ID := 11;
    Data.SatID[2].Idx := 1;
    {$endif}
    for j := 1 to High(Data.SatID) do
      Data.SatID[j].Idx := -1;
    for i := 0 to High(Data.Sats[True]) do
    begin
      Data.Sats[True, i].Idx := -1;
      for j := 1 to High(Data.SatID) do
      begin
        if Data.SatID[j].ID = Data.Sats[True, i].ID then
        begin
          Data.SatID[j].Idx := i;
          Data.Sats[True, i].Idx := j;
          Break;
        end;
      end;
    end;
    SatView.Invalidate;
    SatPower.Invalidate;
  end;
end;

end.

