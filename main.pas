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
    procedure VerifyEmail(EmailAddress: String);
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
//       item := lvEmails.Items[i];
       ProgressBar1.Position:= i;
       emailAddress := lvEmails.Items[i].Caption;
       try
          VerifyEmail(emailAddress);
          lvEmails.Items[i].SubItems.Add('Verified');
       except on E:Exception do
          lvEmails.Items[i].SubItems.Add('Fail');
       end;
    end;
  finally
    lvEmails.EndUpdate;
  end;

end;

procedure TfrmMain.VerifyEmail(EmailAddress: String);
var
   dns: TDNSSend;
   dnsList: TStringList;
   splitters: TStringList;
   domainName: String;
   i: Integer;
begin
   dnsList := TStringList.Create;
   try
      GetMailServers('bisnis2030.com', 'gmail.com', dnsList);
      for i:= 0 to dnsList.Count - 1 do
      begin
//         mmoLog.Lines.Assign(dnsList);
      end;
   finally
     FreeAndNil(dns);
   end;
end;

end.

