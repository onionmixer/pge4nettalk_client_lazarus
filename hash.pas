unit hash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs;

type
  { THashThread }

  THashThread = class(TThread)
  private
    fFileList: TStringList;
    fEvent: TEvent;
    fSection: TCriticalSection;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure addFile(Target, FileName: String);
  end;

implementation

uses
  main,
  LazUTF8Classes,
  flcHash;

{ THashThread }

procedure THashThread.Execute;
var
  target, filename: String;
  cut: Integer;
  fsIn: TFileStreamUTF8;
  data: PByte;
  read: Integer;
  md5: T128BitDigest;
  sha256: T256BitDigest;
  md5str: String;
  sha256str: String;
begin
  data := GetMem(1024*1024);

  while not Terminated do
  begin
    if fEvent.WaitFor(5000) = wrSignaled then
      fEvent.ResetEvent;

    filename := '';
    fSection.Enter;
    if fFileList.Count > 0 then
    begin
      filename := fFileList[0];
      fFileList.Delete(0);
    end;
    fSection.Leave;

    if filename = '' then
      continue;

    cut := Pos('|', filename);
    target := Copy(filename, 1, cut - 1);
    Delete(filename, 1, cut);

    try
      fsIn := TFileStreamUTF8.Create(filename, fmOpenRead);
      MD5InitDigest(md5);
      SHA256InitDigest(sha256);
      while not Terminated do
      begin
        read := fsIn.Read(data^, 1024*1024);
        if fsIn.Position < fsIn.Size then
        begin
          MD5Buf(md5, data^, read);
          SHA256Buf(sha256, data^, read);
        end
        else
        begin
          cut := (read div 64) * 64;
          if cut > 0 then
          begin
            MD5Buf(md5, data^, cut);
            SHA256Buf(sha256, data^, cut);
          end;
          MD5FinalBuf(md5, (data + cut)^, read - cut, fsIn.Size);
          SHA256FinalBuf(sha256, (data + cut)^, read - cut, fsIn.Size);
          break;
        end;
      end;
      FreeAndNil(fsIn);
      md5str := DigestToHexA(md5, sizeof(md5));
      sha256str := DigestToHexA(sha256, sizeof(sha256));
    except
      on E: Exception do
      begin
        FormMain.HashResult(target, filename, E.Message, '');
        continue;
      end;
    end;

    if Terminated then
      break;

    FormMain.HashResult(target, filename, md5str, sha256str);
  end;

  FreeMem(data);
end;

constructor THashThread.Create;
begin
  fFileList := TStringList.Create;
  fEvent := TSimpleEvent.Create;
  fSection := syncobjs.TCriticalSection.Create;

  inherited Create(False);
  FreeOnTerminate := True;
end;

destructor THashThread.Destroy;
begin
  FreeAndNil(fFileList);
  FreeAndNil(fEvent);
  FreeAndNil(fSection);
end;

procedure THashThread.addFile(Target, FileName: String);
begin
  fSection.Enter;
  fFileList.Add(Target + '|' + FileName);
  fSection.Leave;
  fEvent.SetEvent;
end;

end.

