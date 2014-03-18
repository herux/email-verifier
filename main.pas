unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Buttons, CsvDocument, dnssend, tlntsend;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnLoadCSV: TButton;
    btnVerifyAll: TButton;
    btnSave: TButton;
    CSV_OpenDialog: TOpenDialog;
    lvEmails: TListView;
    mmoLog: TMemo;
    ProgressBar1: TProgressBar;
    CSV_SaveDialog: TSaveDialog;
    procedure btnLoadCSVClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnVerifyAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDoc: TCSVDocument;
    procedure PopulateData(dataStrings: TStringList);
    function GetDataCSV(CSVFilename: String): TStringList;
    procedure VerifyAllEmails;
    function VerifyEmail(EmailAddress: String): boolean;
  public
    { public declarations }
  end;

const
  cVerified = 'Verified';
  cFail = 'Fail';

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnLoadCSVClick(Sender: TObject);
var
  dataStrings: TStringList;
begin

  if CSV_OpenDialog.Execute then
  begin
    FDoc.LoadFromFile(CSV_OpenDialog.FileName);
    dataStrings := GetDataCSV(CSV_OpenDialog.FileName);
    try
      PopulateData(dataStrings);
    finally
      dataStrings.Free;
    end;
  end;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  if CSV_SaveDialog.Execute then
      FDoc.SaveToFile(CSV_SaveDialog.FileName);
end;

procedure TfrmMain.btnVerifyAllClick(Sender: TObject);
begin
  VerifyAllEmails;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FDoc := TCSVDocument.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FDoc.Free;
end;

procedure TfrmMain.PopulateData(dataStrings: TStringList);
var
   I: Integer;
   emailItem: TListItem;
begin
  lvEmails.BeginUpdate;
  progressBar1.Position:=0;
  progressBar1.Max:= dataStrings.Count - 1;
  for I:= 0 to dataStrings.Count - 1 do
  begin
      emailItem := lvEmails.Items.Add;
      emailItem.Caption:= dataStrings[I];
      progressBar1.Position:= i;
      application.ProcessMessages;
  end;
  lvEmails.EndUpdate;
end;

function TfrmMain.GetDataCSV(CSVFilename: String): TStringList;
var
   i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to FDoc.RowCount - 1 do
  begin
     Result.Add(FDoc.Cells[0, i]);
  end;
end;

procedure TfrmMain.VerifyAllEmails;
var
  i : Integer;
  emailAddress: String;
  item: TListItem;
begin
  lvEmails.BeginUpdate;
  try
    ProgressBar1.Position:= 0;
    ProgressBar1.Max:=lvEmails.Items.Count - 1;
    for i:= 0 to lvEmails.Items.Count - 1 do
    begin
       item := lvEmails.Items[i];
       emailAddress := item.Caption;
       try
          if VerifyEmail(emailAddress) then
          begin
           item.SubItems.Add(cVerified);
           FDoc.Cells[1, i] := cVerified;
          end else begin
           item.SubItems.Add(cFail);
           FDoc.Cells[1, i] := cFail;
          end;
       except on E:Exception do begin
          item.SubItems.Add(cFail);
          FDoc.Cells[1, i] := cFail;
          end;
       end;
       ProgressBar1.Position:= i;
       application.ProcessMessages;
    end;
  finally
    lvEmails.EndUpdate;
  end;

end;

function TfrmMain.VerifyEmail(EmailAddress: String): boolean;

var
   dnsList: TStringList;
   domainParser: TStringList;
   domainName: String;
   telnet: TTelnetSend;
   i: Integer;

          function Read: String;
             procedure Strip0;
              var
                 I : Integer;
              begin
                   i:=1;
                   while i<=Length(Result) do
                   begin
                        if (Result[i]=#0)and (Result[i-1]=#13)
                        then System.Delete(Result,i-1,2)
                        else Inc(i);
                   end;
              end;

          begin
               Result := telnet.RecvString;
               while Result <> '' do
               begin
                    Strip0;
                    mmoLog.Lines.Add(Result);
                    Result:=telnet.RecvString;
               end;
          end;

begin
   dnsList := TStringList.Create;
   domainParser:=  TStringList.Create;
   try
      domainParser.delimiter := '@';
      domainParser.delimitedText := EmailAddress;
      if domainParser.count = 0 then
      begin
        Result := False;
        exit;
      end;
      domainName := domainParser[1];
      GetMailServers('bisnis2030.com', domainName, dnsList);
      if dnsList.count = 0 then
      begin
        Result := False;
        Exit;
      end;

      if dnsList.Count = 0 then
      begin
        Result := False;
        mmoLog.Lines.Add('Name server not found for email address: '+EmailAddress);
        Exit;
      end;

      for i:= 0 to dnsList.Count - 1 do
      begin
         mmoLog.Lines.add(dnsList[i]);
      end;

      telnet := TTelnetSend.Create;
      try
        try
           telnet.TargetHost := dnsList[0];
           telnet.TargetPort := '25';
           telnet.Timeout := 1000;
           telnet.TermType:= 'dumb';
           telnet.Login;
           Read;
           telnet.Send('HELO untuk.cek.subcriber@gmail.com'#13#10);
           Read;
           telnet.Send('MAIL FROM:<untuk.cek.subcriber@gmail.com>'#13#10);
           Read;
           telnet.Send('RCPT TO:<'+EmailAddress+'>');
           if Pos( Read, 'OK') >= 1 then
              Result := True
           else
              Result := False;
           telnet.Logout;
        except on E:Exception do
           mmoLog.Lines.Add('Error connecting to nameserver: '+dnsList[0]);
        end;
      finally
        telnet.Free;
      end;

      Result := True;
   finally
     dnsList.Free;
     domainParser.free;
   end;
end;

end.

