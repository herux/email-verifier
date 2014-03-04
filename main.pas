unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Buttons, CsvDocument, dnssend;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnLoadCSV: TButton;
    btnVerifyAll: TButton;
    CSV_OpenDialog: TOpenDialog;
    lvEmails: TListView;
    mmoLog: TMemo;
    ProgressBar1: TProgressBar;
    procedure btnLoadCSVClick(Sender: TObject);
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
  for I:= 0 to dataStrings.Count - 1 do
  begin
      emailItem := lvEmails.Items.Add;
      emailItem.Caption:= dataStrings[I];
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
       ProgressBar1.Position:= i;
       emailAddress := item.Caption;
       try
          if VerifyEmail(emailAddress) then
           item.SubItems.Add('Verified')
          else
           item.SubItems.Add('Fail');
       except on E:Exception do
          item.SubItems.Add('Fail');
       end;
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
   i: Integer;
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
      for i:= 0 to dnsList.Count - 1 do
      begin
         mmoLog.Lines.add(dnsList[i]);
      end;
      Result := True;
   finally
     dnsList.Free;
     domainParser.free;
   end;
end;

end.

