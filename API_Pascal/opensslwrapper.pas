{
  OpenSSL DLL import for root.
}

unit OpenSSLWrapper;

interface

{$IFDEF DARWIN}
{$LINKLIB crypto}
{$ENDIF}

const
{$IFDEF WINDOWS}
  LIBEAY = 'libeay32.dll';
{$ENDIF}
{$IFDEF DARWIN}
  LIBEAY = '/usr/lib/libcrypto.dylib';
{$ELSE}
{$IFDEF UNIX}
  LIBEAY = 'libcrypto.so';
{$ENDIF}
{$ENDIF}
  RSA_PKCS1_PADDING = 1;
  AES_MAXNR = 14;

type
  PRSA = Pointer;
  PAES_KEY = ^AES_KEY;
  AES_KEY = record
    rd_key: array[0..(4 * (AES_MAXNR + 1)) - 1] of Cardinal;
    rounds: Integer;
  end;

function d2i_RSAPublicKey(var rsa: PRSA; const in_: PPAnsiChar; len: Longint): PRSA; cdecl; external LIBEAY;
function RSA_public_encrypt(flen: Integer; const from: PAnsiChar; const to_: PAnsiChar; rsa: PRSA; padding: Integer): Integer; cdecl; external LIBEAY;
function RSA_public_decrypt(flen: Integer; const from: PAnsiChar; const to_: PAnsiChar; rsa: PRSA; padding: Integer): Integer; cdecl; external LIBEAY;
procedure RSA_free(r: PRSA); cdecl; external LIBEAY;

function AES_set_encrypt_key(const userKey: PAnsiChar; const bits: Integer; key: PAES_KEY): Integer; cdecl; external LIBEAY;
function AES_set_decrypt_key(const userKey: PAnsiChar; const bits: Integer; key: PAES_KEY): Integer; cdecl; external LIBEAY;
procedure AES_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const key: PAES_KEY); cdecl; external LIBEAY;
procedure AES_decrypt(const in_: PAnsiChar; out_: PAnsiChar; const key: PAES_KEY); cdecl; external LIBEAY;

implementation

end.
