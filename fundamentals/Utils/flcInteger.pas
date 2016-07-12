{******************************************************************************}
{                                                                              }
{   File name:        flcInteger.pas                                           }
{   File version:     5.15                                                     }
{   Description:      Integer functions                                        }
{                                                                              }
{   Copyright:        Copyright (c) 2007-2016, David J Butler                  }
{                     All rights reserved.                                     }
{                     Redistribution and use in source and binary forms, with  }
{                     or without modification, are permitted provided that     }
{                     the following conditions are met:                        }
{                     Redistributions of source code must retain the above     }
{                     copyright notice, this list of conditions and the        }
{                     following disclaimer.                                    }
{                     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND   }
{                     CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED          }
{                     WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED   }
{                     WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A          }
{                     PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL     }
{                     THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,    }
{                     INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR             }
{                     CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,    }
{                     PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF     }
{                     USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)         }
{                     HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER   }
{                     IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING        }
{                     NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE   }
{                     USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE             }
{                     POSSIBILITY OF SUCH DAMAGE.                              }
{                                                                              }
{   Github:           https://github.com/fundamentalslib                       }
{   E-mail:           fundamentals.library at gmail.com                        }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   2007/01/20  0.01  Word64.                                                  }
{   2007/01/22  0.02  Word128.                                                 }
{   2007/01/23  0.03  Int128.                                                  }
{   2007/01/24  0.04  Hash functions.                                          }
{   2007/08/08  4.05  Revised for Fundamentals 4.                              }
{   2007/08/22  4.06  PowerAndMod.                                             }
{   2008/10/11  4.07  Added 512-4096 bit versions using templates.             }
{   2008/12/15  4.08  Additional 128 bit assembly versions.                    }
{   2010/01/07  4.09  Minor revision.                                          }
{   2010/08/06  4.10  Revision.                                                }
{   2013/10/18  4.11  Unicode string update.                                   }
{   2014/04/30  4.12  Revision for FreePascal 2.6.2 Linux.                     }
{   2014/05/01  4.13  Revision for Delphi XE6 Win32/Win64.                     }
{   2016/01/09  5.14  Revised for Fundamentals 5.                              }
{   2016/01/10  5.15  Bug fix in 32-bit assembly routine.                      }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi XE7 Win32                    5.15  2016/01/10                       }
{   Delphi XE7 Win64                    5.15  2016/01/10                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE INTEGER_TEST}
  {.DEFINE INTEGER_TEST_OUTPUT}
{$ENDIF}
{$ENDIF}


unit flcInteger;

interface



{                                                                              }
{ Word structures                                                              }
{                                                                              }
type
  Word8   = Byte;
  PWord8  = ^Word8;
  Word16  = Word;
  PWord16 = ^Word16;
  Word32  = LongWord;
  PWord32 = ^Word32;

  Word32Pair = packed record
    case Integer of
    0 : (A, B    : LongWord);
    1 : (Bytes   : array[0..7] of Byte);
    2 : (Words   : array[0..3] of Word);
    3 : (Word32s : array[0..1] of LongWord);
  end;
  PWord32Pair = ^Word32Pair;

  Word64 = packed record
    case Integer of
    0 : (Bytes     : array[0..7] of Byte);
    1 : (Words     : array[0..3] of Word);
    2 : (LongWords : array[0..1] of LongWord);
  end;
  PWord64 = ^Word64;

  Word128 = packed record
    case Integer of
    0 : (Bytes     : array[0..15] of Byte);
    1 : (Words     : array[0..7] of Word);
    2 : (LongWords : array[0..3] of LongWord);
    3 : (Word64s   : array[0..1] of Word64);
  end;
  PWord128 = ^Word128;

  Word256 = packed record
    case Integer of
    0 : (Bytes     : array[0..31] of Byte);
    1 : (Words     : array[0..15] of Word);
    2 : (LongWords : array[0..7] of LongWord);
    3 : (Word64s   : array[0..3] of Word64);
    4 : (Word128s  : array[0..1] of Word128);
  end;
  PWord256 = ^Word256;

  Word512 = packed record
    case Integer of
    0 : (Bytes     : array[0..63] of Byte);
    1 : (Words     : array[0..31] of Word);
    2 : (LongWords : array[0..15] of LongWord);
    3 : (Word64s   : array[0..7] of Word64);
    4 : (Word128s  : array[0..3] of Word128);
    5 : (Word256s  : array[0..1] of Word256);
  end;
  PWord512 = ^Word512;

  Word1024 = packed record
    case Integer of
    0 : (Bytes     : array[0..127] of Byte);
    1 : (Words     : array[0..63] of Word);
    2 : (LongWords : array[0..31] of LongWord);
    3 : (Word64s   : array[0..15] of Word64);
    4 : (Word128s  : array[0..7] of Word128);
    5 : (Word256s  : array[0..3] of Word256);
    6 : (Word512s  : array[0..1] of Word512);
  end;
  PWord1024 = ^Word1024;

  Word2048 = packed record
    case Integer of
    0 : (Bytes     : array[0..255] of Byte);
    1 : (Words     : array[0..127] of Word);
    2 : (LongWords : array[0..63] of LongWord);
    3 : (Word64s   : array[0..31] of Word64);
    4 : (Word128s  : array[0..15] of Word128);
    5 : (Word256s  : array[0..7] of Word256);
    6 : (Word512s  : array[0..3] of Word512);
    7 : (Word1024s : array[0..1] of Word1024);
  end;
  PWord2048 = ^Word2048;

  Word4096 = packed record
    case Integer of
    0 : (Bytes     : array[0..511] of Byte);
    1 : (Words     : array[0..255] of Word);
    2 : (LongWords : array[0..127] of LongWord);
    3 : (Word64s   : array[0..63] of Word64);
    4 : (Word128s  : array[0..31] of Word128);
    5 : (Word256s  : array[0..15] of Word256);
    6 : (Word512s  : array[0..7] of Word512);
    7 : (Word1024s : array[0..3] of Word1024);
    8 : (Word2048s : array[0..1] of Word2048);
  end;
  PWord4096 = ^Word4096;

  Word8192 = packed record
    case Integer of
    0 : (Bytes     : array[0..1023] of Byte);
    1 : (Words     : array[0..511] of Word);
    2 : (LongWords : array[0..255] of LongWord);
    3 : (Word64s   : array[0..127] of Word64);
    4 : (Word128s  : array[0..63] of Word128);
    5 : (Word256s  : array[0..31] of Word256);
    6 : (Word512s  : array[0..15] of Word512);
    7 : (Word1024s : array[0..7] of Word1024);
    8 : (Word2048s : array[0..3] of Word2048);
    9 : (Word4096s : array[0..1] of Word4096);
  end;
  PWord8192 = ^Word8192;

const
  Word8Size    = SizeOf(Word8);
  Word16Size   = SizeOf(Word16);
  Word32Size   = SizeOf(Word32);
  Word64Size   = SizeOf(Word64);
  Word128Size  = SizeOf(Word128);
  Word256Size  = SizeOf(Word256);
  Word512Size  = SizeOf(Word512);
  Word1024Size = SizeOf(Word1024);
  Word2048Size = SizeOf(Word2048);
  Word4096Size = SizeOf(Word4096);
  Word8192Size = SizeOf(Word8192);

const
  MinWord8      = Low(Word8);
  MaxWord8      = High(Word8);
  MinWord16     = Low(Word16);
  MaxWord16     = High(Word16);
  MinWord32     = Word32(Low(Word32));
  MaxWord32     = Word32(High(Word32));



{                                                                              }
{ Integer structures                                                           }
{                                                                              }
type
  Int8   = ShortInt;
  PInt8  = ^Int8;
  Int16  = SmallInt;
  PInt16 = ^Int16;
  Int32  = LongInt;
  PInt32 = ^Int32;

  Int32Pair = packed record
    case Integer of
    0 : (A, B      : LongInt);
    1 : (Bytes     : array[0..7] of Byte);
    2 : (Words     : array[0..3] of Word);
    3 : (Word32s   : array[0..1] of LongWord);
    4 : (ShortInts : array[0..7] of ShortInt);
    5 : (SmallInts : array[0..3] of SmallInt);
    6 : (LongInts  : array[0..1] of LongInt);
  end;
  PInt32Pair = ^Int32Pair;

  Int64Rec = packed record
    case Integer of
    0 : (Bytes     : array[0..7] of Byte);
    1 : (Words     : array[0..3] of Word);
    2 : (LongWords : array[0..1] of LongWord);
    3 : (LongInts  : array[0..1] of LongInt);
  end;
  PInt64Rec = ^Int64Rec;

  Int128 = packed record
    case Integer of
    0 : (Bytes     : array[0..15] of Byte);
    1 : (Words     : array[0..7] of Word);
    2 : (LongWords : array[0..3] of LongWord);
    3 : (Word64s   : array[0..1] of Word64);
    4 : (LongInts  : array[0..3] of LongInt);
    5 : (Int64s    : array[0..1] of Int64);
  end;
  PInt128 = ^Int128;

  Int256 = packed record
    case Integer of
    0 : (Bytes     : array[0..31] of Byte);
    1 : (Words     : array[0..15] of Word);
    2 : (LongWords : array[0..7] of LongWord);
    3 : (Word64s   : array[0..3] of Word64);
    4 : (LongInts  : array[0..7] of LongInt);
    5 : (Int64s    : array[0..3] of Int64);
  end;
  PInt256 = ^Int256;

  Int512 = packed record
    Sign  : Int8;
    Value : Word512;
  end;
  PInt512 = ^Int512;

  Int1024 = packed record
    Sign  : Int8;
    Value : Word1024;
  end;
  PInt1024 = ^Int1024;

  Int2048 = packed record
    Sign  : Int8;
    Value : Word2048;
  end;
  PInt2048 = ^Int2048;

  Int4096 = packed record
    Sign  : Int8;
    Value : Word4096;
  end;
  PInt4096 = ^Int4096;

const
  Int8Size    = SizeOf(Int8);
  Int16Size   = SizeOf(Int16);
  Int32Size   = SizeOf(LongInt);
  Int64Size   = SizeOf(Int64);
  Int128Size  = SizeOf(Int128);
  Int256Size  = SizeOf(Int256);
  Int512Size  = SizeOf(Int512);
  Int1024Size = SizeOf(Int1024);
  Int2048Size = SizeOf(Int2048);
  Int4096Size = SizeOf(Int4096);

const
  MinInt8   = Low(Int8);
  MaxInt8   = High(Int8);
  MinInt16  = Low(Int16);
  MaxInt16  = High(Int16);
  MinInt32  = Int32(Low(Int32));
  MaxInt32  = Int32(High(Int32));
  MinInt64  = Int64(Low(Int64));
  MaxInt64  = Int64(High(Int64));



{                                                                              }
{ UnicodeString                                                                }
{                                                                              }
{$IFNDEF SupportUnicodeString}
type
  UnicodeString = WideString;
  PUnicodeString = ^UnicodeString;
{$ENDIF}



{                                                                              }
{ Word8                                                                        }
{                                                                              }
function  Word8Min(const A, B: Byte): Byte; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word8Max(const A, B: Byte): Byte; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word8IsInt8Range(const A: Byte): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word8Compare(const A, B: Byte): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word8IsBitSet(const A: Byte; const B: Integer): Boolean; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word8SetBit(var A: Byte; const B: Integer);              {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word8ClearBit(var A: Byte; const B: Integer);            {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word8ToggleBit(var A: Byte; const B: Integer);           {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word8SetBitScanForward(const A: Byte): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word8SetBitScanReverse(const A: Byte): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word8ClearBitScanForward(const A: Byte): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word8ClearBitScanReverse(const A: Byte): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word8Shl(var A: Byte; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word8Shr(var A: Byte; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word8Rol(var A: Byte; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word8Ror(var A: Byte; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

procedure Word8Shl1(var A: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word8Shr1(var A: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word8Rol1(var A: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word8Ror1(var A: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word8BitCount(const A: Byte): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word8GCD(const A, B: Byte): Byte;                                {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word8ExtendedEuclid(const A, B: Byte; var X, Y: SmallInt): Byte; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word8IsPrime(const A: Byte): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word8NextPrime(const A: Byte): Byte;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word8Hash(const A: Byte): Byte; {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Word16                                                                       }
{                                                                              }
function  Word16Lo(const A: Word): Byte; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word16Hi(const A: Word): Byte; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word16Min(const A, B: Word): Word; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word16Max(const A, B: Word): Word; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word16IsWord8Range(const A: Word): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word16IsInt8Range(const A: Word): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word16IsInt16Range(const A: Word): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word16Compare(const A, B: Word): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word16IsBitSet(const A: Word; const B: Integer): Boolean; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word16SetBit(var A: Word; const B: Integer);              {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word16ClearBit(var A: Word; const B: Integer);            {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word16ToggleBit(var A: Word; const B: Integer);           {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word16SetBitScanForward(const A: Word): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word16SetBitScanReverse(const A: Word): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word16ClearBitScanForward(const A: Word): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word16ClearBitScanReverse(const A: Word): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word16Shl(var A: Word; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word16Shr(var A: Word; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word16Rol(var A: Word; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word16Ror(var A: Word; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

procedure Word16Shl1(var A: Word); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word16Shr1(var A: Word); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word16Rol1(var A: Word); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word16Ror1(var A: Word); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word16BitCount(const A: Word): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word16SwapEndian(const A: Word): Word; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word16GCD(const A, B: Word): Word;                               {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word16ExtendedEuclid(const A, B: Word; var X, Y: LongInt): Word; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word16IsPrime(const A: Word): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word16NextPrime(const A: Word): Word;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word16Hash(const A: Word): Word; {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Word32                                                                       }
{                                                                              }
function  Word32IsOdd(const A: LongWord): Boolean; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word32Lo(const A: LongWord): Word; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word32Hi(const A: LongWord): Word; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word32Min(const A, B: LongWord): LongWord; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word32Max(const A, B: LongWord): LongWord; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word32IsWord8Range(const A: LongWord): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word32IsWord16Range(const A: LongWord): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word32IsInt8Range(const A: LongWord): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word32IsInt16Range(const A: LongWord): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word32IsInt32Range(const A: LongWord): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word32Compare(const A, B: LongWord): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word32IsBitSet(const A: LongWord; const B: Integer): Boolean; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word32SetBit(var A: LongWord; const B: Integer);              {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word32ClearBit(var A: LongWord; const B: Integer);            {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word32ToggleBit(var A: LongWord; const B: Integer);           {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word32SetBitScanForward(const A: LongWord): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word32SetBitScanReverse(const A: LongWord): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word32ClearBitScanForward(const A: LongWord): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word32ClearBitScanReverse(const A: LongWord): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word32Shl(var A: LongWord; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word32Shr(var A: LongWord; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word32Rol(var A: LongWord; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word32Ror(var A: LongWord; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

procedure Word32Shl1(var A: LongWord); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word32Shr1(var A: LongWord); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word32Rol1(var A: LongWord); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word32Ror1(var A: LongWord); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word32BitCount(const A: LongWord): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word32SwapEndian(const A: LongWord): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word32MultiplyWord32(const A, B: LongWord; out R: Word64);                        {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word32MultiplyDivWord32(const A, B, C: LongWord; var Q: Word64; var R: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word32GCD(const A, B: LongWord): LongWord;                             {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word32ExtendedEuclid(const A, B: LongWord; var X, Y: Int64): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word32PowerAndMod(const A, E, M: LongWord): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word32IsPrime(const A: LongWord): Boolean;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word32IsPrimeMR(const A: LongWord): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word32NextPrime(const A: LongWord): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word32Hash(const A: LongWord): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Word32Pair                                                                   }
{                                                                              }
procedure Word32PairInitZero(var A: Word32Pair);                         {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word32PairInitWord32(var C: Word32Pair; const A, B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word32PairToWord32(const C: Word32Pair; var A, B: LongWord);   {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Word64                                                                       }
{                                                                              }
const
  Word64ConstZero    : Word64 = (LongWords: ($00000000, $00000000));
  Word64ConstOne     : Word64 = (LongWords: ($00000001, $00000000));
  Word64ConstMinimum : Word64 = (LongWords: ($00000000, $00000000));
  Word64ConstMaximum : Word64 = (LongWords: ($FFFFFFFF, $FFFFFFFF));

procedure Word64InitZero(var A: Word64);    {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word64InitOne(var A: Word64);     {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word64InitMinimum(var A: Word64); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word64InitMaximum(var A: Word64); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word64IsZero(const A: Word64): Boolean;    {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word64IsOne(const A: Word64): Boolean;     {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word64IsMinimum(const A: Word64): Boolean; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word64IsMaximum(const A: Word64): Boolean; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word64IsOdd(const A: Word64): Boolean;     {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word64IsWord8Range(const A: Word64): Boolean;  {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word64IsWord16Range(const A: Word64): Boolean; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word64IsWord32Range(const A: Word64): Boolean; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word64IsInt8Range(const A: Word64): Boolean;   {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word64IsInt16Range(const A: Word64): Boolean;  {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word64IsInt32Range(const A: Word64): Boolean;  {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word64IsInt64Range(const A: Word64): Boolean;  {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

procedure Word64InitWord32(var A: Word64; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word64InitInt32(var A: Word64; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64InitInt64(var A: Word64; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64InitFloat(var A: Word64; const B: Extended);  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word64ToWord32(const A: Word64): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64ToInt32(const A: Word64): LongInt;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64ToInt64(const A: Word64): Int64;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64ToFloat(const A: Word64): Extended;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word64Lo(const A: Word64): LongWord; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word64Hi(const A: Word64): LongWord; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word64EqualsWord32(const A: Word64; const B: LongWord): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64EqualsWord64(const A, B: Word64): Boolean;                 {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64EqualsInt32(const A: Word64; const B: LongInt): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64EqualsInt64(const A: Word64; const B: Int64): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64EqualsFloat(const A: Word64; const B: Extended): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word64CompareWord32(const A: Word64; const B: LongWord): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64CompareWord64(const A, B: Word64): Integer;                 {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64CompareInt32(const A: Word64; const B: LongInt): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64CompareInt64(const A: Word64; const B: Int64): Integer;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64CompareFloat(const A: Word64; const B: Extended): Integer;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word64Min(const A, B: Word64): Word64; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64Max(const A, B: Word64): Word64; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word64Not(var A: Word64);                        {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word64OrWord64(var A: Word64; const B: Word64);  {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word64AndWord64(var A: Word64; const B: Word64); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure Word64XorWord64(var A: Word64; const B: Word64); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word64IsBitSet(const A: Word64; const B: Integer): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64SetBit(var A: Word64; const B: Integer);              {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64ClearBit(var A: Word64; const B: Integer);            {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64ToggleBit(var A: Word64; const B: Integer);           {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word64SetBitScanForward(const A: Word64): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64SetBitScanReverse(const A: Word64): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64ClearBitScanForward(const A: Word64): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64ClearBitScanReverse(const A: Word64): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word64Shl(var A: Word64; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64Shr(var A: Word64; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64Rol(var A: Word64; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64Ror(var A: Word64; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word64Shl1(var A: Word64); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64Shr1(var A: Word64); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64Rol1(var A: Word64); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64Ror1(var A: Word64); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word64Swap(var A, B: Word64);      {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64ReverseBits(var A: Word64);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64ReverseBytes(var A: Word64); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word64BitCount(const A: Word64): Integer;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64IsPowerOfTwo(const A: Word64): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word64SwapEndian(var A: Word64); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word64Inc(var A: Word64); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word64AddWord8(var A: Word64; const B: Byte);      {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64AddWord16(var A: Word64; const B: Word);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64AddWord32(var A: Word64; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64AddWord64(var A: Word64; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word64SubtractWord32(var A: Word64; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64SubtractWord64(var A: Word64; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word64MultiplyWord8(var A: Word64; const B: Byte);           {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64MultiplyWord16(var A: Word64; const B: Word);          {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64MultiplyWord32(var A: Word64; const B: LongWord);      {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64MultiplyWord64InPlace(var A: Word64; const B: Word64); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64MultiplyWord64(const A, B: Word64; var R: Word128);    {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word64Sqr(var A: Word64); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word64DivideWord8(const A: Word64; const B: Byte; out Q: Word64; out R: Byte);          {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64DivideWord16(const A: Word64; const B: Word; out Q: Word64; out R: Word);         {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64DivideWord32(const A: Word64; const B: LongWord; out Q: Word64; out R: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64DivideWord64(const A, B: Word64; var Q, R: Word64);                               {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word64ModWord64(const A, B: Word64; var R: Word64); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word64ToStr(const A: Word64): String;         {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64ToStrA(const A: Word64): AnsiString;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64ToStrW(const A: Word64): WideString;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64ToStrU(const A: Word64): UnicodeString; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  StrToWord64(const A: String): Word64;         {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  StrToWord64A(const A: AnsiString): Word64;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  StrToWord64W(const A: WideString): Word64;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  StrToWord64U(const A: UnicodeString): Word64; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word64GCD(const A, B: Word64): Word64; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64ExtendedEuclid(const A, B: Word64; var X, Y: Int128): Word64; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word64ISqrt(var A: Word64);            {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64Sqrt(const A: Word64): Extended; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word64Power(var A: Word64; const B: LongWord);    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word64PowerAndMod(const A, E, M: Word64): Word64; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word64IsPrime(const A: Word64): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word64NextPrime(var A: Word64);          {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word64Hash(const A: Word64): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Word128                                                                      }
{                                                                              }
const
  Word128ConstZero    : Word128 = (LongWords: ($00000000, $00000000, $00000000, $00000000));
  Word128ConstOne     : Word128 = (LongWords: ($00000001, $00000000, $00000000, $00000000));
  Word128ConstMinimum : Word128 = (LongWords: ($00000000, $00000000, $00000000, $00000000));
  Word128ConstMaximum : Word128 = (LongWords: ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF));

procedure Word128InitZero(var A: Word128);    {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128InitOne(var A: Word128);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128InitMinimum(var A: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128InitMaximum(var A: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word128IsZero(const A: Word128): Boolean;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128IsOne(const A: Word128): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128IsMinimum(const A: Word128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128IsMaximum(const A: Word128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128IsOdd(const A: Word128): Boolean;     {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word128IsWord8Range(const A: Word128): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128IsWord16Range(const A: Word128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128IsWord32Range(const A: Word128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128IsWord64Range(const A: Word128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128IsInt8Range(const A: Word128): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128IsInt16Range(const A: Word128): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128IsInt32Range(const A: Word128): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128IsInt64Range(const A: Word128): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128IsInt128Range(const A: Word128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word128InitWord32(var A: Word128; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128InitWord64(var A: Word128; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128InitInt32(var A: Word128; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128InitInt64(var A: Word128; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128InitFloat(var A: Word128; const B: Extended);  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word128ToWord32(const A: Word128): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128ToWord64(const A: Word128): Word64;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128ToInt32(const A: Word128): LongInt;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128ToInt64(const A: Word128): Int64;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128ToFloat(const A: Word128): Extended;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word128Lo(const A: Word128): Word64; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  Word128Hi(const A: Word128): Word64; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word128EqualsWord32(const A: Word128; const B: LongWord): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128EqualsWord64(const A: Word128; const B: Word64): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128EqualsWord128(const A, B: Word128): Boolean;                {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128EqualsInt32(const A: Word128; const B: LongInt): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128EqualsInt64(const A: Word128; const B: Int64): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word128CompareWord32(const A: Word128; const B: LongWord): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128CompareWord64(const A: Word128; const B: Word64): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128CompareWord128(const A, B: Word128): Integer;                {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128CompareInt32(const A: Word128; const B: LongInt): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128CompareInt64(const A: Word128; const B: Int64): Integer;     {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word128Min(const A, B: Word128): Word128; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128Max(const A, B: Word128): Word128; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word128Not(var A: Word128);                          {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128OrWord128(var A: Word128; const B: Word128);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128AndWord128(var A: Word128; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128XorWord128(var A: Word128; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word128IsBitSet(const A: Word128; const B: Integer): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128SetBit(var A: Word128; const B: Integer);              {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128ClearBit(var A: Word128; const B: Integer);            {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128ToggleBit(var A: Word128; const B: Integer);           {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word128SetBitScanForward(const A: Word128): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128SetBitScanReverse(const A: Word128): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128ClearBitScanForward(const A: Word128): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128ClearBitScanReverse(const A: Word128): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word128Shl(var A: Word128; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128Shr(var A: Word128; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128Rol(var A: Word128; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128Ror(var A: Word128; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word128Shl1(var A: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128Shr1(var A: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128Rol1(var A: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128Ror1(var A: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word128Swap(var A, B: Word128);      {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128ReverseBits(var A: Word128);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128ReverseBytes(var A: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word128BitCount(const A: Word128): Integer;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128IsPowerOfTwo(const A: Word128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word128Inc(var A: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word128AddWord8(var A: Word128; const B: Byte);      {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128AddWord16(var A: Word128; const B: Word);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128AddWord32(var A: Word128; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128AddWord64(var A: Word128; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128AddWord128(var A: Word128; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word128SubtractWord32(var A: Word128; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128SubtractWord64(var A: Word128; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128SubtractWord128(var A: Word128; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word128MultiplyWord8(var A: Word128; const B: Byte);         {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128MultiplyWord16(var A: Word128; const B: Word);        {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128MultiplyWord32(var A: Word128; const B: LongWord);    {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128MultiplyWord64(var A: Word128; const B: Word64);      {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128MultiplyWord128Low(var A: Word128; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128MultiplyWord128(const A, B: Word128; var R: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word128Sqr(var A: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word128DivideWord8(const A: Word128; const B: Byte; var Q: Word128; var R: Byte);          {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128DivideWord16(const A: Word128; const B: Word; var Q: Word128; var R: Word);         {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128DivideWord32(const A: Word128; const B: LongWord; var Q: Word128; var R: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128DivideWord64(const A: Word128; const B: Word64; var Q: Word128; var R: Word64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128DivideWord128(const A, B: Word128; var Q, R: Word128);                              {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word128ModWord128(const A, B: Word128; var R: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word128ToStr(const A: Word128): String;         {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128ToStrA(const A: Word128): AnsiString;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128ToStrW(const A: Word128): WideString;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128ToStrU(const A: Word128): UnicodeString; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  StrToWord128(const A: String): Word128;         {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  StrToWord128A(const A: AnsiString): Word128;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  StrToWord128W(const A: WideString): Word128;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  StrToWord128U(const A: UnicodeString): Word128; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word128GCD(const A, B: Word128): Word128; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word128ISqrt(var A: Word128);            {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128Sqrt(const A: Word128): Extended; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word128Power(var A: Word128; const B: LongWord);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word128PowerAndMod(const A, E, M: Word128): Word128; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word128IsPrime(const A: Word128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word128NextPrime(var A: Word128);          {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word128Hash(const A: Word128): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Word256                                                                      }
{                                                                              }
const
  Word256ConstZero : Word256 = (LongWords: (
      $00000000, $00000000, $00000000, $00000000,
      $00000000, $00000000, $00000000, $00000000));
  Word256ConstOne : Word256 = (LongWords: (
      $00000001, $00000000, $00000000, $00000000,
      $00000000, $00000000, $00000000, $00000000));
  Word256ConstMinimum : Word256 = (LongWords: (
      $00000000, $00000000, $00000000, $00000000,
      $00000000, $00000000, $00000000, $00000000));
  Word256ConstMaximum : Word256 = (LongWords: (
      $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
      $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF));

procedure Word256InitZero(var A: Word256);    {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256InitOne(var A: Word256);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256InitMinimum(var A: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256InitMaximum(var A: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word256IsZero(const A: Word256): Boolean;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256IsOne(const A: Word256): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256IsMinimum(const A: Word256): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256IsMaximum(const A: Word256): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256IsOdd(const A: Word256): Boolean;     {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word256IsWord32Range(const A: Word256): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256IsWord64Range(const A: Word256): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256IsWord128Range(const A: Word256): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256IsInt32Range(const A: Word256): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256IsInt64Range(const A: Word256): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256IsInt128Range(const A: Word256): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word256InitWord32(var A: Word256; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256InitWord64(var A: Word256; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256InitWord128(var A: Word256; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256InitInt32(var A: Word256; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256InitInt64(var A: Word256; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256InitInt128(var A: Word256; const B: Int128);   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word256ToWord32(const A: Word256): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256ToWord64(const A: Word256): Word64;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256ToWord128(const A: Word256): Word128; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256ToInt32(const A: Word256): LongInt;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256ToInt64(const A: Word256): Int64;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256ToInt128(const A: Word256): Int128;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256ToFloat(const A: Word256): Extended;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word256Lo(const A: Word256): Word128; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256Hi(const A: Word256): Word128; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word256EqualsWord32(const A: Word256; const B: LongWord): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256EqualsWord64(const A: Word256; const B: Word64): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256EqualsWord128(const A: Word256; const B: Word128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256EqualsWord256(const A, B: Word256): Boolean;                {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256EqualsInt32(const A: Word256; const B: LongInt): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256EqualsInt64(const A: Word256; const B: Int64): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256EqualsInt128(const A: Word256; const B: Int128): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word256CompareWord32(const A: Word256; const B: LongWord): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256CompareWord64(const A: Word256; const B: Word64): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256CompareWord128(const A: Word256; const B: Word128): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256CompareWord256(const A, B: Word256): Integer;                {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256CompareInt32(const A: Word256; const B: LongInt): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256CompareInt64(const A: Word256; const B: Int64): Integer;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256CompareInt128(const A: Word256; const B: Int128): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word256Min(const A, B: Word256): Word256; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word256Max(const A, B: Word256): Word256; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word256Not(var A: Word256);                          {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256OrWord256(var A: Word256; const B: Word256);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256AndWord256(var A: Word256; const B: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256XorWord256(var A: Word256; const B: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word256IsBitSet(const A: Word256; const B: Integer): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256SetBit(var A: Word256; const B: Integer);              {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256ClearBit(var A: Word256; const B: Integer);            {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256ToggleBit(var A: Word256; const B: Integer);           {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word256Shl1(var A: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256Shr1(var A: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256Rol1(var A: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256Ror1(var A: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word256Swap(var A, B: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word256AddWord32(var A: Word256; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256AddWord64(var A: Word256; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256AddWord128(var A: Word256; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256AddWord256(var A: Word256; const B: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word256SubtractWord32(var A: Word256; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256SubtractWord64(var A: Word256; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256SubtractWord128(var A: Word256; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256SubtractWord256(var A: Word256; const B: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word256MultiplyWord8(var A: Word256; const B: Byte);         {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256MultiplyWord16(var A: Word256; const B: Word);        {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256MultiplyWord32(var A: Word256; const B: LongWord);    {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256MultiplyWord64(var A: Word256; const B: Word64);      {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256MultiplyWord128(var A: Word256; const B: Word128);    {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word256MultiplyWord256(const A, B: Word256; var R: Word512); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word256Sqr(const A: Word256; var R: Word512); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word256DivideWord128(const A: Word256; const B: Word128; var Q: Word256; var R: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word256Hash(const A: Word256): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}


{                                                                              }
{ Word512                                                                      }
{                                                                              }
procedure Word512InitZero(var A: Word512);    {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512InitOne(var A: Word512);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512InitMinimum(var A: Word512); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512InitMaximum(var A: Word512); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word512IsZero(const A: Word512): Boolean;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512IsOne(const A: Word512): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512IsMinimum(const A: Word512): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512IsMaximum(const A: Word512): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512IsOdd(const A: Word512): Boolean;     {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word512IsWord32Range(const A: Word512): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512IsWord64Range(const A: Word512): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512IsWord128Range(const A: Word512): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512IsInt32Range(const A: Word512): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512IsInt64Range(const A: Word512): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512IsInt128Range(const A: Word512): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word512InitWord32(var A: Word512; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512InitWord64(var A: Word512; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512InitWord128(var A: Word512; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512InitInt32(var A: Word512; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512InitInt64(var A: Word512; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512InitInt128(var A: Word512; const B: Int128);   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word512ToWord32(const A: Word512): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512ToWord64(const A: Word512): Word64;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512ToWord128(const A: Word512): Word128; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512ToInt32(const A: Word512): LongInt;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512ToInt64(const A: Word512): Int64;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512ToInt128(const A: Word512): Int128;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512ToFloat(const A: Word512): Extended;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word512Lo(const A: Word512): Word256; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512Hi(const A: Word512): Word256; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word512EqualsWord32(const A: Word512; const B: LongWord): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512EqualsWord64(const A: Word512; const B: Word64): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512EqualsWord128(const A: Word512; const B: Word128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512EqualsWord512(const A, B: Word512): Boolean;                {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512EqualsInt32(const A: Word512; const B: LongInt): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512EqualsInt64(const A: Word512; const B: Int64): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512EqualsInt128(const A: Word512; const B: Int128): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word512CompareWord32(const A: Word512; const B: LongWord): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512CompareWord64(const A: Word512; const B: Word64): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512CompareWord128(const A: Word512; const B: Word128): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512CompareWord512(const A, B: Word512): Integer;                {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512CompareInt32(const A: Word512; const B: LongInt): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512CompareInt64(const A: Word512; const B: Int64): Integer;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512CompareInt128(const A: Word512; const B: Int128): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word512Min(const A, B: Word512): Word512; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word512Max(const A, B: Word512): Word512; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word512Not(var A: Word512);                          {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512OrWord512(var A: Word512; const B: Word512);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512AndWord512(var A: Word512; const B: Word512); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512XorWord512(var A: Word512; const B: Word512); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word512IsBitSet(const A: Word512; const B: Integer): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512SetBit(var A: Word512; const B: Integer);              {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512ClearBit(var A: Word512; const B: Integer);            {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512ToggleBit(var A: Word512; const B: Integer);           {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word512Shl1(var A: Word512); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512Shr1(var A: Word512); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512Rol1(var A: Word512); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512Ror1(var A: Word512); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word512Swap(var A, B: Word512); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word512AddWord32(var A: Word512; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512AddWord64(var A: Word512; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512AddWord128(var A: Word512; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512AddWord256(var A: Word512; const B: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word512AddWord512(var A: Word512; const B: Word512); {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Word1024                                                                     }
{                                                                              }
procedure Word1024InitZero(var A: Word1024);    {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024InitOne(var A: Word1024);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024InitMinimum(var A: Word1024); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024InitMaximum(var A: Word1024); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word1024IsZero(const A: Word1024): Boolean;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024IsOne(const A: Word1024): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024IsMinimum(const A: Word1024): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024IsMaximum(const A: Word1024): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024IsOdd(const A: Word1024): Boolean;     {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word1024IsWord32Range(const A: Word1024): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024IsWord64Range(const A: Word1024): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024IsWord128Range(const A: Word1024): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024IsInt32Range(const A: Word1024): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024IsInt64Range(const A: Word1024): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024IsInt128Range(const A: Word1024): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word1024InitWord32(var A: Word1024; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024InitWord64(var A: Word1024; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024InitWord128(var A: Word1024; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024InitInt32(var A: Word1024; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024InitInt64(var A: Word1024; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024InitInt128(var A: Word1024; const B: Int128);   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word1024ToWord32(const A: Word1024): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024ToWord64(const A: Word1024): Word64;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024ToWord128(const A: Word1024): Word128; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024ToInt32(const A: Word1024): LongInt;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024ToInt64(const A: Word1024): Int64;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024ToInt128(const A: Word1024): Int128;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024ToFloat(const A: Word1024): Extended;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word1024Lo(const A: Word1024): Word256; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024Hi(const A: Word1024): Word256; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word1024EqualsWord32(const A: Word1024; const B: LongWord): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024EqualsWord64(const A: Word1024; const B: Word64): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024EqualsWord128(const A: Word1024; const B: Word128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024EqualsWord1024(const A, B: Word1024): Boolean;                {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024EqualsInt32(const A: Word1024; const B: LongInt): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024EqualsInt64(const A: Word1024; const B: Int64): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024EqualsInt128(const A: Word1024; const B: Int128): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word1024CompareWord32(const A: Word1024; const B: LongWord): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024CompareWord64(const A: Word1024; const B: Word64): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024CompareWord128(const A: Word1024; const B: Word128): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024CompareWord1024(const A, B: Word1024): Integer;                {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024CompareInt32(const A: Word1024; const B: LongInt): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024CompareInt64(const A: Word1024; const B: Int64): Integer;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024CompareInt128(const A: Word1024; const B: Int128): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word1024Min(const A, B: Word1024): Word1024; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word1024Max(const A, B: Word1024): Word1024; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word1024Not(var A: Word1024);                          {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024OrWord1024(var A: Word1024; const B: Word1024);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024AndWord1024(var A: Word1024; const B: Word1024); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024XorWord1024(var A: Word1024; const B: Word1024); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word1024IsBitSet(const A: Word1024; const B: Integer): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024SetBit(var A: Word1024; const B: Integer);              {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024ClearBit(var A: Word1024; const B: Integer);            {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024ToggleBit(var A: Word1024; const B: Integer);           {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word1024Shl1(var A: Word1024); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024Shr1(var A: Word1024); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024Rol1(var A: Word1024); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024Ror1(var A: Word1024); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word1024Swap(var A, B: Word1024); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word1024AddWord32(var A: Word1024; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024AddWord64(var A: Word1024; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024AddWord128(var A: Word1024; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024AddWord256(var A: Word1024; const B: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word1024AddWord1024(var A: Word1024; const B: Word1024); {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Word2048                                                                     }
{                                                                              }
procedure Word2048InitZero(var A: Word2048);    {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048InitOne(var A: Word2048);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048InitMinimum(var A: Word2048); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048InitMaximum(var A: Word2048); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word2048IsZero(const A: Word2048): Boolean;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048IsOne(const A: Word2048): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048IsMinimum(const A: Word2048): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048IsMaximum(const A: Word2048): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048IsOdd(const A: Word2048): Boolean;     {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word2048IsWord32Range(const A: Word2048): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048IsWord64Range(const A: Word2048): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048IsWord128Range(const A: Word2048): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048IsInt32Range(const A: Word2048): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048IsInt64Range(const A: Word2048): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048IsInt128Range(const A: Word2048): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word2048InitWord32(var A: Word2048; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048InitWord64(var A: Word2048; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048InitWord128(var A: Word2048; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048InitInt32(var A: Word2048; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048InitInt64(var A: Word2048; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048InitInt128(var A: Word2048; const B: Int128);   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word2048ToWord32(const A: Word2048): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048ToWord64(const A: Word2048): Word64;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048ToWord128(const A: Word2048): Word128; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048ToInt32(const A: Word2048): LongInt;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048ToInt64(const A: Word2048): Int64;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048ToInt128(const A: Word2048): Int128;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048ToFloat(const A: Word2048): Extended;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word2048Lo(const A: Word2048): Word256; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048Hi(const A: Word2048): Word256; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word2048EqualsWord32(const A: Word2048; const B: LongWord): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048EqualsWord64(const A: Word2048; const B: Word64): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048EqualsWord128(const A: Word2048; const B: Word128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048EqualsWord2048(const A, B: Word2048): Boolean;                {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048EqualsInt32(const A: Word2048; const B: LongInt): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048EqualsInt64(const A: Word2048; const B: Int64): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048EqualsInt128(const A: Word2048; const B: Int128): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word2048CompareWord32(const A: Word2048; const B: LongWord): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048CompareWord64(const A: Word2048; const B: Word64): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048CompareWord128(const A: Word2048; const B: Word128): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048CompareWord2048(const A, B: Word2048): Integer;                {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048CompareInt32(const A: Word2048; const B: LongInt): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048CompareInt64(const A: Word2048; const B: Int64): Integer;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048CompareInt128(const A: Word2048; const B: Int128): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word2048Min(const A, B: Word2048): Word2048; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word2048Max(const A, B: Word2048): Word2048; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word2048Not(var A: Word2048);                          {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048OrWord2048(var A: Word2048; const B: Word2048);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048AndWord2048(var A: Word2048; const B: Word2048); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048XorWord2048(var A: Word2048; const B: Word2048); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word2048IsBitSet(const A: Word2048; const B: Integer): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048SetBit(var A: Word2048; const B: Integer);              {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048ClearBit(var A: Word2048; const B: Integer);            {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048ToggleBit(var A: Word2048; const B: Integer);           {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word2048Shl1(var A: Word2048); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048Shr1(var A: Word2048); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048Rol1(var A: Word2048); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048Ror1(var A: Word2048); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word2048Swap(var A, B: Word2048); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word2048AddWord32(var A: Word2048; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048AddWord64(var A: Word2048; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048AddWord128(var A: Word2048; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048AddWord256(var A: Word2048; const B: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word2048AddWord2048(var A: Word2048; const B: Word2048); {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Word4096                                                                     }
{                                                                              }
procedure Word4096InitZero(var A: Word4096);    {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096InitOne(var A: Word4096);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096InitMinimum(var A: Word4096); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096InitMaximum(var A: Word4096); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word4096IsZero(const A: Word4096): Boolean;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096IsOne(const A: Word4096): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096IsMinimum(const A: Word4096): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096IsMaximum(const A: Word4096): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096IsOdd(const A: Word4096): Boolean;     {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Word4096IsWord32Range(const A: Word4096): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096IsWord64Range(const A: Word4096): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096IsWord128Range(const A: Word4096): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096IsInt32Range(const A: Word4096): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096IsInt64Range(const A: Word4096): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096IsInt128Range(const A: Word4096): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word4096InitWord32(var A: Word4096; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096InitWord64(var A: Word4096; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096InitWord128(var A: Word4096; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096InitInt32(var A: Word4096; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096InitInt64(var A: Word4096; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096InitInt128(var A: Word4096; const B: Int128);   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word4096ToWord32(const A: Word4096): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096ToWord64(const A: Word4096): Word64;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096ToWord128(const A: Word4096): Word128; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096ToInt32(const A: Word4096): LongInt;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096ToInt64(const A: Word4096): Int64;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096ToInt128(const A: Word4096): Int128;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096ToFloat(const A: Word4096): Extended;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word4096Lo(const A: Word4096): Word256; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096Hi(const A: Word4096): Word256; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word4096EqualsWord32(const A: Word4096; const B: LongWord): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096EqualsWord64(const A: Word4096; const B: Word64): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096EqualsWord128(const A: Word4096; const B: Word128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096EqualsWord4096(const A, B: Word4096): Boolean;                {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096EqualsInt32(const A: Word4096; const B: LongInt): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096EqualsInt64(const A: Word4096; const B: Int64): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096EqualsInt128(const A: Word4096; const B: Int128): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word4096CompareWord32(const A: Word4096; const B: LongWord): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096CompareWord64(const A: Word4096; const B: Word64): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096CompareWord128(const A: Word4096; const B: Word128): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096CompareWord4096(const A, B: Word4096): Integer;                {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096CompareInt32(const A: Word4096; const B: LongInt): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096CompareInt64(const A: Word4096; const B: Int64): Integer;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096CompareInt128(const A: Word4096; const B: Int128): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word4096Min(const A, B: Word4096): Word4096; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Word4096Max(const A, B: Word4096): Word4096; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word4096Not(var A: Word4096);                          {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096OrWord4096(var A: Word4096; const B: Word4096);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096AndWord4096(var A: Word4096; const B: Word4096); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096XorWord4096(var A: Word4096; const B: Word4096); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Word4096IsBitSet(const A: Word4096; const B: Integer): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096SetBit(var A: Word4096; const B: Integer);              {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096ClearBit(var A: Word4096; const B: Integer);            {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096ToggleBit(var A: Word4096; const B: Integer);           {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word4096Shl1(var A: Word4096); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096Shr1(var A: Word4096); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096Rol1(var A: Word4096); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096Ror1(var A: Word4096); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word4096Swap(var A, B: Word4096); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Word4096AddWord32(var A: Word4096; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096AddWord64(var A: Word4096; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096AddWord128(var A: Word4096; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096AddWord256(var A: Word4096; const B: Word256); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Word4096AddWord4096(var A: Word4096; const B: Word4096); {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Int8                                                                         }
{                                                                              }
function  Int8Sign(const A: ShortInt): Integer;             {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int8Abs(const A: ShortInt; var B: Byte): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int8IsWord8Range(const A: ShortInt): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int8Compare(const A, B: ShortInt): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int8Hash(const A: ShortInt): Byte; {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Int16                                                                        }
{                                                                              }
function  Int16Sign(const A: SmallInt): Integer;             {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int16Abs(const A: SmallInt; var B: Word): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int16IsInt8Range(const A: SmallInt): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int16IsWord8Range(const A: SmallInt): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int16IsWord16Range(const A: SmallInt): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int16Compare(const A, B: SmallInt): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int16Hash(const A: SmallInt): Word; {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Int32                                                                        }
{                                                                              }
function  Int32Sign(const A: LongInt): Integer;                 {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int32Abs(const A: LongInt; var B: LongWord): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int32IsInt8Range(const A: LongInt): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int32IsInt16Range(const A: LongInt): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int32IsWord8Range(const A: LongInt): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int32IsWord16Range(const A: LongInt): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int32IsWord32Range(const A: LongInt): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int32Compare(const A, B: LongInt): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int32Hash(const A: LongInt): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Int64                                                                        }
{                                                                              }
function  Int64Sign(const A: Int64): Integer;               {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int64Abs(const A: Int64; var B: Word64): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int64IsInt8Range(const A: Int64): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int64IsInt16Range(const A: Int64): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int64IsInt32Range(const A: Int64): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int64IsWord8Range(const A: Int64): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int64IsWord16Range(const A: Int64): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int64IsWord32Range(const A: Int64): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int64IsWord64Range(const A: Int64): Boolean; {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Int64Compare(const A, B: Int64): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int64Hash(const A: Int64): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Int128                                                                       }
{                                                                              }
const
  Int128ConstZero     : Int128 = (LongWords: ($00000000, $00000000, $00000000, $00000000));
  Int128ConstOne      : Int128 = (LongWords: ($00000001, $00000000, $00000000, $00000000));
  Int128ConstMinusOne : Int128 = (LongWords: ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF));
  Int128ConstMinimum  : Int128 = (LongWords: ($00000000, $00000000, $00000000, $80000000));
  Int128ConstMaximum  : Int128 = (LongWords: ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $7FFFFFFF));

procedure Int128InitZero(var A: Int128);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128InitOne(var A: Int128);      {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128InitMinusOne(var A: Int128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128InitMinimum(var A: Int128);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128InitMaximum(var A: Int128);  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int128IsNegative(const A: Int128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128IsZero(const A: Int128): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128IsOne(const A: Int128): Boolean;      {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128IsMinusOne(const A: Int128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128IsMinimum(const A: Int128): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128IsMaximum(const A: Int128): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128IsOdd(const A: Int128): Boolean;      {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

function  Int128Sign(const A: Int128): Integer;                  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128Negate(var A: Int128);                           {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128InitNegWord128(var A: Int128; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128AbsInPlace(var A: Int128): Boolean;              {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128Abs(const A: Int128; var B: Word128): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int128IsWord8Range(const A: Int128): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128IsWord16Range(const A: Int128): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128IsWord32Range(const A: Int128): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128IsWord64Range(const A: Int128): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128IsWord128Range(const A: Int128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128IsInt8Range(const A: Int128): Boolean;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128IsInt16Range(const A: Int128): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128IsInt32Range(const A: Int128): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128IsInt64Range(const A: Int128): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int128InitWord32(var A: Int128; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128InitWord64(var A: Int128; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128InitWord128(var A: Int128; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128InitInt32(var A: Int128; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128InitInt64(var A: Int128; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128InitFloat(var A: Int128; const B: Extended);  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int128ToWord32(const A: Int128): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128ToWord64(const A: Int128): Word64;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128ToWord128(const A: Int128): Word128; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128ToInt32(const A: Int128): LongInt;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128ToInt64(const A: Int128): Int64;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128ToFloat(const A: Int128): Extended;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int128EqualsWord32(const A: Int128; const B: LongWord): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128EqualsWord64(const A: Int128; const B: Word64): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128EqualsWord128(const A: Int128; const B: Word128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128EqualsInt32(const A: Int128; const B: LongInt): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128EqualsInt64(const A: Int128; const B: Int64): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128EqualsInt128(const A, B: Int128): Boolean;                 {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int128CompareWord32(const A: Int128; const B: LongWord): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128CompareWord64(const A: Int128; const B: Word64): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128CompareWord128(const A: Int128; const B: Word128): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128CompareInt32(const A: Int128; const B: LongInt): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128CompareInt64(const A: Int128; const B: Int64): Integer;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128CompareInt128(const A, B: Int128): Integer;                 {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int128Min(const A, B: Int128): Int128; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128Max(const A, B: Int128): Int128; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int128Not(var A: Int128);                        {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128OrInt128(var A: Int128; const B: Int128);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128AndInt128(var A: Int128; const B: Int128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128XorInt128(var A: Int128; const B: Int128); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int128IsBitSet(const A: Int128; const B: Integer): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128SetBit(var A: Int128; const B: Integer);              {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128ClearBit(var A: Int128; const B: Integer);            {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128ToggleBit(var A: Int128; const B: Integer);           {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int128SetBitScanForward(const A: Int128): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128SetBitScanReverse(const A: Int128): Integer;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128ClearBitScanForward(const A: Int128): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128ClearBitScanReverse(const A: Int128): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int128Shl(var A: Int128; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128Shr(var A: Int128; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128Rol(var A: Int128; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128Ror(var A: Int128; const B: Byte); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int128Shl1(var A: Int128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128Shr1(var A: Int128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128Rol1(var A: Int128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128Ror1(var A: Int128); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int128Swap(var A, B: Int128);      {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128ReverseBits(var A: Int128);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128ReverseBytes(var A: Int128); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int128BitCount(const A: Int128): Integer;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128IsPowerOfTwo(const A: Int128): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int128AddWord32(var A: Int128; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128AddWord64(var A: Int128; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128AddWord128(var A: Int128; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128AddInt32(var A: Int128; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128AddInt64(var A: Int128; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128AddInt128(var A: Int128; const B: Int128);   {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int128SubtractWord32(var A: Int128; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128SubtractWord64(var A: Int128; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128SubtractWord128(var A: Int128; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128SubtractInt32(var A: Int128; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128SubtractInt64(var A: Int128; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128SubtractInt128(var A: Int128; const B: Int128);   {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int128MultiplyWord8(var A: Int128; const B: Byte);      {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128MultiplyWord16(var A: Int128; const B: Word);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128MultiplyWord32(var A: Int128; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128MultiplyWord64(var A: Int128; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128MultiplyWord128(var A: Int128; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128MultiplyInt8(var A: Int128; const B: ShortInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128MultiplyInt16(var A: Int128; const B: SmallInt);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128MultiplyInt32(var A: Int128; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128MultiplyInt64(var A: Int128; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128MultiplyInt128(var A: Int128; const B: Int128);   {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int128Sqr(var A: Int128); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int128DivideWord8(const A: Int128; const B: Byte; var Q: Int128; var R: Byte);          {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128DivideWord16(const A: Int128; const B: Word; var Q: Int128; var R: Word);         {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128DivideWord32(const A: Int128; const B: LongWord; var Q: Int128; var R: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128DivideWord64(const A: Int128; const B: Word64; var Q: Int128; var R: Word64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128DivideWord128(const A: Int128; const B: Word128; var Q: Int128; var R: Word128);  {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int128DivideInt8(const A: Int128; const B: ShortInt; var Q: Int128; var R: ShortInt);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128DivideInt16(const A: Int128; const B: SmallInt; var Q: Int128; var R: SmallInt); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128DivideInt32(const A: Int128; const B: LongInt; var Q: Int128; var R: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128DivideInt64(const A: Int128; const B: Int64; var Q: Int128; var R: Int64);       {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int128DivideInt128(const A, B: Int128; var Q, R: Int128);                              {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int128ToStr(const A: Int128): String;         {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128ToStrA(const A: Int128): AnsiString;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128ToStrW(const A: Int128): WideString;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128ToStrU(const A: Int128): UnicodeString; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  StrToInt128(const A: String): Int128;         {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  StrToInt128A(const A: AnsiString): Int128;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  StrToInt128W(const A: WideString): Int128;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  StrToInt128U(const A: UnicodeString): Int128; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int128ISqrt(var A: Int128);            {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int128Sqrt(const A: Int128): Extended; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int128Power(var A: Int128; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int128Hash(const A: Int128): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Int256                                                                       }
{                                                                              }
const
  Int256ConstZero : Int256 = (LongWords: (
      $00000000, $00000000, $00000000, $00000000,
      $00000000, $00000000, $00000000, $00000000));
  Int256ConstOne : Int256 = (LongWords: (
      $00000001, $00000000, $00000000, $00000000,
      $00000000, $00000000, $00000000, $00000000));
  Int256ConstMinusOne : Int256 = (LongWords: (
      $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
      $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF));
  Int256ConstMinimum : Int256 = (LongWords: (
      $00000000, $00000000, $00000000, $00000000,
      $00000000, $00000000, $00000000, $80000000));
  Int256ConstMaximum : Int256 = (LongWords: (
      $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
      $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $7FFFFFFF));

procedure Int256InitZero(var A: Int256);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int256InitOne(var A: Int256);      {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int256InitMinusOne(var A: Int256); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int256InitMinimum(var A: Int256);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int256InitMaximum(var A: Int256);  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int256IsNegative(const A: Int256): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsZero(const A: Int256): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsOne(const A: Int256): Boolean;      {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsMinusOne(const A: Int256): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsMinimum(const A: Int256): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsMaximum(const A: Int256): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsOdd(const A: Int256): Boolean;      {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int256Sign(const A: Int256): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int256IsWord8Range(const A: Int256): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsWord16Range(const A: Int256): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsWord32Range(const A: Int256): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsWord64Range(const A: Int256): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsWord128Range(const A: Int256): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsWord256Range(const A: Int256): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsInt8Range(const A: Int256): Boolean;    {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsInt16Range(const A: Int256): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsInt32Range(const A: Int256): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsInt64Range(const A: Int256): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256IsInt128Range(const A: Int256): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int256InitWord32(var A: Int256; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int256InitWord64(var A: Int256; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int256InitWord128(var A: Int256; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int256InitInt32(var A: Int256; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int256InitInt64(var A: Int256; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int256InitInt128(var A: Int256; const B: Int128);   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int256ToWord32(const A: Int256): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256ToWord128(const A: Int256): Word128; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256ToInt32(const A: Int256): LongInt;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256ToInt64(const A: Int256): Int64;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int256ToInt128(const A: Int256): Int128;   {$IFDEF UseStdCall}stdcall;{$ENDIF}


{                                                                              }
{ Int512                                                                       }
{                                                                              }
procedure Int512InitZero(var A: Int512);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int512InitOne(var A: Int512);      {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int512InitMinusOne(var A: Int512); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int512InitMinimum(var A: Int512);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int512InitMaximum(var A: Int512);  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int512IsNegative(const A: Int512): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int512IsZero(const A: Int512): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int512IsOne(const A: Int512): Boolean;      {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int512IsMinusOne(const A: Int512): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int512IsMinimum(const A: Int512): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int512IsMaximum(const A: Int512): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int512IsOdd(const A: Int512): Boolean;      {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int512Sign(const A: Int512): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int512IsWord32Range(const A: Int512): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int512IsWord64Range(const A: Int512): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int512IsWord128Range(const A: Int512): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int512IsWord256Range(const A: Int512): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int512IsInt32Range(const A: Int512): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int512IsInt64Range(const A: Int512): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int512InitWord32(var A: Int512; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int512InitWord64(var A: Int512; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int512InitWord128(var A: Int512; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int512InitInt32(var A: Int512; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int512InitInt64(var A: Int512; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int512InitInt128(var A: Int512; const B: Int128);   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int512ToWord32(const A: Int512): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int512ToInt32(const A: Int512): LongInt;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int512ToInt64(const A: Int512): Int64;     {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Int1024                                                                      }
{                                                                              }
procedure Int1024InitZero(var A: Int1024);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int1024InitOne(var A: Int1024);      {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int1024InitMinusOne(var A: Int1024); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int1024InitMinimum(var A: Int1024);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int1024InitMaximum(var A: Int1024);  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int1024IsNegative(const A: Int1024): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int1024IsZero(const A: Int1024): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int1024IsOne(const A: Int1024): Boolean;      {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int1024IsMinusOne(const A: Int1024): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int1024IsMinimum(const A: Int1024): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int1024IsMaximum(const A: Int1024): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int1024IsOdd(const A: Int1024): Boolean;      {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int1024Sign(const A: Int1024): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int1024IsWord32Range(const A: Int1024): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int1024IsWord64Range(const A: Int1024): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int1024IsWord128Range(const A: Int1024): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int1024IsWord256Range(const A: Int1024): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int1024IsInt32Range(const A: Int1024): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int1024IsInt64Range(const A: Int1024): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int1024InitWord32(var A: Int1024; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int1024InitWord64(var A: Int1024; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int1024InitWord128(var A: Int1024; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int1024InitInt32(var A: Int1024; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int1024InitInt64(var A: Int1024; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int1024InitInt128(var A: Int1024; const B: Int128);   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int1024ToWord32(const A: Int1024): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int1024ToInt32(const A: Int1024): LongInt;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int1024ToInt64(const A: Int1024): Int64;     {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Int2048                                                                      }
{                                                                              }
procedure Int2048InitZero(var A: Int2048);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int2048InitOne(var A: Int2048);      {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int2048InitMinusOne(var A: Int2048); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int2048InitMinimum(var A: Int2048);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int2048InitMaximum(var A: Int2048);  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int2048IsNegative(const A: Int2048): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int2048IsZero(const A: Int2048): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int2048IsOne(const A: Int2048): Boolean;      {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int2048IsMinusOne(const A: Int2048): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int2048IsMinimum(const A: Int2048): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int2048IsMaximum(const A: Int2048): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int2048IsOdd(const A: Int2048): Boolean;      {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int2048Sign(const A: Int2048): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int2048IsWord32Range(const A: Int2048): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int2048IsWord64Range(const A: Int2048): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int2048IsWord128Range(const A: Int2048): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int2048IsWord256Range(const A: Int2048): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int2048IsInt32Range(const A: Int2048): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int2048IsInt64Range(const A: Int2048): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int2048InitWord32(var A: Int2048; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int2048InitWord64(var A: Int2048; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int2048InitWord128(var A: Int2048; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int2048InitInt32(var A: Int2048; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int2048InitInt64(var A: Int2048; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int2048InitInt128(var A: Int2048; const B: Int128);   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int2048ToWord32(const A: Int2048): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int2048ToInt32(const A: Int2048): LongInt;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int2048ToInt64(const A: Int2048): Int64;     {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Int4096                                                                      }
{                                                                              }
procedure Int4096InitZero(var A: Int4096);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int4096InitOne(var A: Int4096);      {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int4096InitMinusOne(var A: Int4096); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int4096InitMinimum(var A: Int4096);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int4096InitMaximum(var A: Int4096);  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int4096IsNegative(const A: Int4096): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int4096IsZero(const A: Int4096): Boolean;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int4096IsOne(const A: Int4096): Boolean;      {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int4096IsMinusOne(const A: Int4096): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int4096IsMinimum(const A: Int4096): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int4096IsMaximum(const A: Int4096): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int4096IsOdd(const A: Int4096): Boolean;      {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int4096Sign(const A: Int4096): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int4096IsWord32Range(const A: Int4096): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int4096IsWord64Range(const A: Int4096): Boolean;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int4096IsWord128Range(const A: Int4096): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int4096IsWord256Range(const A: Int4096): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int4096IsInt32Range(const A: Int4096): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int4096IsInt64Range(const A: Int4096): Boolean;   {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure Int4096InitWord32(var A: Int4096; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int4096InitWord64(var A: Int4096; const B: Word64);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int4096InitWord128(var A: Int4096; const B: Word128); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int4096InitInt32(var A: Int4096; const B: LongInt);   {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int4096InitInt64(var A: Int4096; const B: Int64);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure Int4096InitInt128(var A: Int4096; const B: Int128);   {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  Int4096ToWord32(const A: Int4096): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int4096ToInt32(const A: Int4096): LongInt;   {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  Int4096ToInt64(const A: Int4096): Int64;     {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ INTEGER ENCODINGS                                                            }
{                                                                              }

{                                                                              }
{ VarWord32                                                                    }
{                                                                              }
type
  VarWord32 = packed record
    ControlByte : Byte;
    case Integer of
    0 : (DataWord8  : Byte);
    1 : (DataWord16 : Word);
    2 : (DataWord32 : LongWord);
  end;

procedure VarWord32InitZero(var A: VarWord32);    {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure VarWord32InitMaximum(var A: VarWord32); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  VarWord32IsZero(const A: VarWord32): Boolean;    {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
function  VarWord32IsMaximum(const A: VarWord32): Boolean; {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure VarWord32InitWord8(var A: VarWord32; const B: Byte);      {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure VarWord32InitWord16(var A: VarWord32; const B: Word);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure VarWord32InitWord32(var A: VarWord32; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  VarWord32ToWord8(const A: VarWord32): Byte;      {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  VarWord32ToWord16(const A: VarWord32): Word;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  VarWord32ToWord32(const A: VarWord32): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  VarWord32Size(const A: VarWord32): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ VarInt32                                                                     }
{                                                                              }
type
  VarInt32 = packed record
    ControlByte : Byte;
    case Integer of
    0 : (DataWord8  : Byte);
    1 : (DataWord16 : Word);
    2 : (DataWord32 : LongWord);
    3 : (DataInt8   : ShortInt);
    4 : (DataInt16  : SmallInt);
    5 : (DataInt32  : LongInt);
  end;

procedure VarInt32InitZero(var A: VarInt32); {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}

procedure VarInt32InitWord8(var A: VarInt32; const B: Byte);      {$IFDEF UseStdCall}stdcall;{$ELSE}{$IFDEF UseInline}inline;{$ENDIF}{$ENDIF}
procedure VarInt32InitWord16(var A: VarInt32; const B: Word);     {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure VarInt32InitWord32(var A: VarInt32; const B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}

procedure VarInt32InitInt8(var A: VarInt32; const B: ShortInt);  {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure VarInt32InitInt16(var A: VarInt32; const B: SmallInt); {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure VarInt32InitInt32(var A: VarInt32; const B: LongInt);  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  VarInt32ToWord8(const A: VarWord32): Byte;      {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  VarInt32ToWord16(const A: VarWord32): Word;     {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  VarInt32ToWord32(const A: VarWord32): LongWord; {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  VarInt32ToInt8(const A: VarInt32): ShortInt;  {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  VarInt32ToInt16(const A: VarInt32): SmallInt; {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  VarInt32ToInt32(const A: VarInt32): LongInt;  {$IFDEF UseStdCall}stdcall;{$ENDIF}

function  VarInt32Size(const A: VarInt32): Integer; {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ VarWord32Pair                                                                }
{                                                                              }
type
  VarWord32Pair = packed record
    ControlByte : Byte;
    case Integer of
    0 : (DataWord8s  : array[0..7] of Byte);
    1 : (DataWord16s : array[0..3] of Word);
    2 : (DataWord32s : array[0..1] of LongWord);
  end;

procedure VarWord32PairInitZero(var A: VarWord32Pair);                         {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure VarWord32PairInitWord32(var C: VarWord32Pair; const A, B: LongWord); {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  VarWord32PairToWord32Pair(const A: VarWord32Pair): Word32Pair;       {$IFDEF UseStdCall}stdcall;{$ENDIF}
function  VarWord32PairSize(const A: VarWord32Pair): Integer;                  {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ VarInt32Pair                                                                 }
{                                                                              }
type
  VarInt32Pair = packed record
    ControlByte : Byte;
    case Integer of
    0 : (DataWord8s  : array[0..7] of Byte);
    1 : (DataWord16s : array[0..3] of Word);
    2 : (DataWord32s : array[0..1] of LongWord);
    3 : (DataInt8s   : array[0..7] of ShortInt);
    4 : (DataInt16s  : array[0..3] of SmallInt);
    5 : (DataInt32s  : array[0..1] of LongInt);
  end;

procedure VarInt32PairInitZero(var A: VarInt32Pair);                       {$IFDEF UseStdCall}stdcall;{$ENDIF}
procedure VarInt32PairInitInt32(var C: VarInt32Pair; const A, B: LongInt); {$IFDEF UseStdCall}stdcall;{$ENDIF}



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF INTEGER_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }
  Math;



{                                                                              }
{ Errors                                                                       }
{                                                                              }
{$IFDEF DELPHI2005_UP}
// Raise errors using Error function in System.pas
{$IFOPT Q+}
procedure RaiseOverflowError;
begin
  Error(reIntOverflow);
end;
{$ENDIF}
{$IFOPT R+}
procedure RaiseRangeError;
begin
  Error(reRangeError);
end;
{$ENDIF}
procedure RaiseDivByZeroError;
begin
  Error(reDivByZero);
end;

procedure RaiseInvalidOpError;
begin
  Error(reInvalidOp);
end;

procedure RaiseConvertError;
begin
  Error(reInvalidOp);
end;
{$ELSE}
// Raise range errors using Assert mechanism in System.pas to avoid
// dependancy on SysUtils.pas
{$IFOPT C+}{$DEFINE OPT_C_ON}{$ELSE}{$UNDEF OPT_C_ON}{$C+}{$ENDIF}
{$IFOPT Q+}
resourcestring
  SOverflowError = 'Overflow error';

procedure RaiseOverflowError;
begin
  Assert(False, SOverflowError);
end;
{$ENDIF}
{$IFOPT R+}
resourcestring
  SRangeError = 'Range error';

procedure RaiseRangeError;
begin
  Assert(False, SOverflowError);
end;
{$ENDIF}
resourcestring
  SDivByZeroError = 'Division by zero';
  SInvalidOpError = 'Invalid operation';
  SConvertError   = 'Conversion error';

procedure RaiseDivByZeroError;
begin
  Assert(False, SDivByZeroError);
end;

procedure RaiseInvalidOpError;
begin
  Assert(False, SInvalidOpError);
end;

procedure RaiseConvertError;
begin
  Assert(False, SConvertError);
end;
{$ENDIF}
{$IFNDEF OPT_C_ON}{$C-}{$ENDIF}



{                                                                              }
{ Word8                                                                        }
{                                                                              }
function Word8Min(const A, B: Byte): Byte;
begin
  if A <= B then
    Result := A
  else
    Result := B;
end;

function Word8Max(const A, B: Byte): Byte;
begin
  if A >= B then
    Result := A
  else
    Result := B;
end;

function Word8IsInt8Range(const A: Byte): Boolean;
begin
  Result := A <= Byte(MaxInt8);
end;

function Word8Compare(const A, B: Byte): Integer;
begin
  if A < B then
    Result := -1 else
  if A > B then
    Result := 1
  else
    Result := 0;
end;

function Word8IsBitSet(const A: Byte; const B: Integer): Boolean;
begin
  if (B < 0) or (B > 7) then
    Result := False
  else
    Result := (A and (1 shl B) <> 0);
end;

procedure Word8SetBit(var A: Byte; const B: Integer);
begin
  if (B < 0) or (B > 7) then
    exit;
  A := A or (1 shl B);
end;

procedure Word8ClearBit(var A: Byte; const B: Integer);
begin
  if (B < 0) or (B > 7) then
    exit;
  A := A and not (1 shl B);
end;

procedure Word8ToggleBit(var A: Byte; const B: Integer);
begin
  if (B < 0) or (B > 7) then
    exit;
  A := A xor (1 shl B);
end;

function Word8SetBitScanForward(const A: Byte): Integer;
var B : Byte;
begin
  Result := 0;
  B := A;
  while B > 0 do
    if B and 1 <> 0 then
      exit
    else
      begin
        Inc(Result);
        B := B shr 1;
      end;
  Result := -1;
end;

function Word8SetBitScanReverse(const A: Byte): Integer;
var B : Byte;
begin
  Result := 7;
  B := A;
  while B > 0 do
    if B and $80 <> 0 then
      exit
    else
      begin
        Dec(Result);
        B := B shl 1;
      end;
  Result := -1;
end;

function Word8ClearBitScanForward(const A: Byte): Integer;
var B : Byte;
begin
  Result := 0;
  B := A;
  while B > 0 do
    if B and 1 = 0 then
      exit
    else
      begin
        Inc(Result);
        B := B shr 1;
      end;
  Result := -1;
end;

function Word8ClearBitScanReverse(const A: Byte): Integer;
var B : Byte;
begin
  Result := 7;
  B := A;
  while B > 0 do
    if B and $80 = 0 then
      exit
    else
      begin
        Dec(Result);
        B := B shl 1;
      end;
  Result := -1;
end;

procedure Word8Shl(var A: Byte; const B: Byte);
begin
  A := A shl B;
end;

procedure Word8Shr(var A: Byte; const B: Byte);
begin
  A := A shr B;
end;

procedure Word8Rol(var A: Byte; const B: Byte);
var C, D : Byte;
begin
  C := B mod 8;
  if C = 0 then
    exit;
  D := 8 - C;
  A := Byte(A shl C) or
       Byte(A shr D);
end;

procedure Word8Ror(var A: Byte; const B: Byte);
var C, D : Byte;
begin
  C := B mod 8;
  if C = 0 then
    exit;
  D := 8 - C;
  A := Byte(A shr C) or
       Byte(A shl D);
end;

procedure Word8Shl1(var A: Byte);
begin
  A := A shl 1;
end;

procedure Word8Shr1(var A: Byte);
begin
  A := A shr 1;
end;

procedure Word8Rol1(var A: Byte);
begin
  A := Byte(A shl 1) or
       Byte(A shr 7);
end;

procedure Word8Ror1(var A: Byte);
begin
  A := Byte(A shr 1) or
       Byte(A shl 7);
end;

const
  Word8BitCountTable : array[Byte] of Byte =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
     1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
     1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
     1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
     3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
     4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);

function Word8BitCount(const A: Byte): Integer;
begin
  Result := Word8BitCountTable[A];
end;

// Uses the Euclidean algorithm
function Word8GCD(const A, B: Byte): Byte;
var C, D, T : Byte;
begin
  C := A;
  D := B;
  while D <> 0 do
    begin
      T := D;
      D := C mod D;
      C := T;
    end;
  Result := C;
end;

// Finds GCD(A, B) and X and Y where AX + BY = GCD(A, B)
function Word8ExtendedEuclid(const A, B: Byte; var X, Y: SmallInt): Byte;
var C, D, T, Q : Byte;
    I, J, U    : SmallInt;
begin
  C := A;
  D := B;
  X := 0;
  I := 1;
  Y := 1;
  J := 0;
  while D <> 0 do
    begin
      T := D;
      Q := C div D;
      D := C mod D;
      C := T;
      U := X;
      X := I - Q * X;
      I := U;
      U := Y;
      Y := J - Q * Y;
      J := U;
    end;
  X := I;
  Y := J;
  Result := C;
end;

const
  Word8PrimeSet: set of Byte = [
      2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61,
      67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137,
      139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211,
      223, 227, 229, 233, 239, 241, 251];

  Word8PrimeCount = 54;
  Word8PrimeTable: array[0..Word8PrimeCount - 1] of Byte = (
      2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61,
      67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137,
      139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211,
      223, 227, 229, 233, 239, 241, 251);

function Word8IsPrime(const A: Byte): Boolean;
begin
  Result := A in Word8PrimeSet;
end;

function Word8NextPrime(const A: Byte): Byte;
var B : Byte;
begin
  B := A;
  if B = $FF then
    begin
      Result := 0; // no next Word8 prime
      exit;
    end;
  if B and 1 = 0 then
    Inc(B)
  else
    Inc(B, 2);
  while B < $FF do
    begin
      if B in Word8PrimeSet then
        begin
          Result := B;
          exit;
        end;
      Inc(B, 2);
    end;
  Result := 0; // no next Word8 prime
end;

const
  // Hash table for Word8 values
  // Based on RC2 "Pi" table
  // Each table value appears once only and is in a randomised order
  // No table value is the same as the value of its index in the table
  Word8HashTable: array[Byte] of Byte = (
    $D9, $78, $F9, $C4, $19, $DD, $B5, $ED, $28, $E9, $FD, $79, $4A, $A0, $D8, $9D,
    $C6, $7E, $37, $83, $2B, $76, $53, $8E, $62, $4C, $64, $88, $44, $8B, $FB, $A2,
    $17, $9A, $59, $F5, $87, $B3, $4F, $13, $61, $45, $6D, $8D, $09, $81, $7D, $32,
    $BD, $8F, $40, $EB, $86, $B7, $7B, $0B, $F0, $95, $21, $22, $5C, $6B, $4E, $82,
    $54, $D6, $65, $93, $CE, $60, $B2, $1C, $73, $56, $C0, $14, $A7, $8C, $F1, $DC,
    $12, $75, $CA, $1F, $3B, $BE, $E4, $D1, $42, $3D, $D4, $30, $A3, $3C, $B6, $26,
    $6F, $BF, $0E, $DA, $46, $69, $07, $57, $27, $F2, $1D, $9B, $BC, $94, $43, $03,
    $F8, $11, $C7, $F6, $90, $EF, $3E, $E7, $06, $C3, $D5, $2F, $C8, $66, $1E, $D7,
    $08, $E8, $EA, $DE, $80, $52, $EE, $F7, $84, $AA, $72, $AC, $35, $4D, $6A, $2A,
    $96, $1A, $D2, $71, $5A, $15, $49, $74, $4B, $9F, $D0, $5E, $04, $18, $A4, $EC,
    $C2, $E0, $41, $6E, $0F, $51, $CB, $CC, $24, $91, $AF, $50, $A1, $F4, $70, $39,
    $99, $7C, $3A, $85, $23, $B8, $B4, $7A, $FC, $02, $36, $5B, $25, $55, $97, $31,
    $2D, $5D, $FA, $98, $E3, $8A, $92, $AE, $05, $DF, $29, $10, $67, $6C, $BA, $C9,
    $D3, $00, $E6, $CF, $E1, $9E, $A8, $2C, $63, $16, $01, $3F, $58, $E2, $89, $A9,
    $0D, $38, $34, $1B, $AB, $33, $FF, $B0, $BB, $48, $0C, $5F, $B9, $B1, $CD, $2E,
    $C5, $F3, $DB, $47, $E5, $A5, $9C, $77, $0A, $A6, $20, $68, $FE, $7F, $C1, $AD);

function Word8Hash(const A: Byte): Byte;
begin
  Result := Word8HashTable[A];
end;



{                                                                              }
{ Word16                                                                       }
{                                                                              }
function Word16Lo(const A: Word): Byte;
begin
  Result := Byte(A);
end;

function Word16Hi(const A: Word): Byte;
begin
  Result := Byte(A shr 8);
end;

function Word16Min(const A, B: Word): Word;
begin
  if A <= B then
    Result := A
  else
    Result := B;
end;

function Word16Max(const A, B: Word): Word;
begin
  if A >= B then
    Result := A
  else
    Result := B;
end;

function Word16IsWord8Range(const A: Word): Boolean;
begin
  Result := A <= MaxWord8;
end;

function Word16IsInt8Range(const A: Word): Boolean;
begin
  Result := A <= Word(MaxInt8);
end;

function Word16IsInt16Range(const A: Word): Boolean;
begin
  Result := A <= Word(MaxInt16);
end;

function Word16Compare(const A, B: Word): Integer;
begin
  if A < B then
    Result := -1 else
  if A > B then
    Result := 1
  else
    Result := 0;
end;

function Word16IsBitSet(const A: Word; const B: Integer): Boolean;
begin
  if (B < 0) or (B > 15) then
    Result := False
  else
    Result := (A and (1 shl B) <> 0);
end;

procedure Word16SetBit(var A: Word; const B: Integer);
begin
  if (B < 0) or (B > 15) then
    exit;
  A := A or (1 shl B);
end;

procedure Word16ClearBit(var A: Word; const B: Integer);
begin
  if (B < 0) or (B > 15) then
    exit;
  A := A and not (1 shl B);
end;

procedure Word16ToggleBit(var A: Word; const B: Integer);
begin
  if (B < 0) or (B > 15) then
    exit;
  A := A xor (1 shl B);
end;

function Word16SetBitScanForward(const A: Word): Integer;
var B : Word;
begin
  Result := 0;
  B := A;
  while B > 0 do
    if B and 1 <> 0 then
      exit
    else
      begin
        Inc(Result);
        B := B shr 1;
      end;
  Result := -1;
end;

function Word16SetBitScanReverse(const A: Word): Integer;
var B : Word;
begin
  Result := 15;
  B := A;
  while B > 0 do
    if B and $8000 <> 0 then
      exit
    else
      begin
        Dec(Result);
        B := B shl 1;
      end;
  Result := -1;
end;

function Word16ClearBitScanForward(const A: Word): Integer;
var B : Word;
begin
  Result := 0;
  B := A;
  while B > 0 do
    if B and 1 = 0 then
      exit
    else
      begin
        Inc(Result);
        B := B shr 1;
      end;
  Result := -1;
end;

function Word16ClearBitScanReverse(const A: Word): Integer;
var B : Word;
begin
  Result := 15;
  B := A;
  while B > 0 do
    if B and $8000 = 0 then
      exit
    else
      begin
        Dec(Result);
        B := B shl 1;
      end;
  Result := -1;
end;

procedure Word16Shl(var A: Word; const B: Byte);
begin
  A := A shl B;
end;

procedure Word16Shr(var A: Word; const B: Byte);
begin
  A := A shr B;
end;

procedure Word16Rol(var A: Word; const B: Byte);
var C, D : Byte;
begin
  C := B mod 16;
  if C = 0 then
    exit;
  D := 16 - C;
  A := Word(A shl C) or
       Word(A shr D);
end;

procedure Word16Ror(var A: Word; const B: Byte);
var C, D : Byte;
begin
  C := B mod 16;
  if C = 0 then
    exit;
  D := 16 - C;
  A := Word(A shr C) or
       Word(A shl D);
end;

procedure Word16Shl1(var A: Word);
begin
  A := A shl 1;
end;

procedure Word16Shr1(var A: Word);
begin
  A := A shr 1;
end;

procedure Word16Rol1(var A: Word);
begin
  A := Word(A shl 1) or
       Word(A shr 15);
end;

procedure Word16Ror1(var A: Word);
begin
  A := Word(A shr 1) or
       Word(A shl 15);
end;

function Word16BitCount(const A: Word): Integer;
begin
  Result := Word8BitCountTable[A and $FF] +
            Word8BitCountTable[A shr 8];
end;

function Word16SwapEndian(const A: Word): Word;
begin
  Result := (Byte(A) shl 8) or
            (Byte(A shr 8));
end;

// Uses the Euclidean algorithm
function Word16GCD(const A, B: Word): Word;
var C, D, T : Word;
begin
  C := A;
  D := B;
  while D <> 0 do
    begin
      T := D;
      D := C mod D;
      C := T;
    end;
  Result := C;
end;

// Finds GCD(A, B) and X and Y where AX + BY = GCD(A, B)
function Word16ExtendedEuclid(const A, B: Word; var X, Y: LongInt): Word;
var C, D, T, Q : Word;
    I, J, U    : LongInt;
begin
  C := A;
  D := B;
  X := 0;
  I := 1;
  Y := 1;
  J := 0;
  while D <> 0 do
    begin
      T := D;
      Q := C div D;
      D := C mod D;
      C := T;
      U := X;
      X := I - Q * X;
      I := U;
      U := Y;
      Y := J - Q * Y;
      J := U;
    end;
  X := I;
  Y := J;
  Result := C;
end;

const
  Word16PrimeCount = 432;
  Word16PrimeTable: array[0..Word16PrimeCount - 1] of Word = (
    2   , 3   , 5   , 7   , 11  , 13  , 17  , 19  ,
    23  , 29  , 31  , 37  , 41  , 43  , 47  , 53  ,
    59  , 61  , 67  , 71  , 73  , 79  , 83  , 89  ,
    97  , 101 , 103 , 107 , 109 , 113 , 127 , 131 ,
    137 , 139 , 149 , 151 , 157 , 163 , 167 , 173 ,
    179 , 181 , 191 , 193 , 197 , 199 , 211 , 223 ,
    227 , 229 , 233 , 239 , 241 , 251 , 257 , 263 ,
    269 , 271 , 277 , 281 , 283 , 293 , 307 , 311 ,
    313 , 317 , 331 , 337 , 347 , 349 , 353 , 359 ,
    367 , 373 , 379 , 383 , 389 , 397 , 401 , 409 ,
    419 , 421 , 431 , 433 , 439 , 443 , 449 , 457 ,
    461 , 463 , 467 , 479 , 487 , 491 , 499 , 503 ,
    509 , 521 , 523 , 541 , 547 , 557 , 563 , 569 ,
    571 , 577 , 587 , 593 , 599 , 601 , 607 , 613 ,
    617 , 619 , 631 , 641 , 643 , 647 , 653 , 659 ,
    661 , 673 , 677 , 683 , 691 , 701 , 709 , 719 ,
    727 , 733 , 739 , 743 , 751 , 757 , 761 , 769 ,
    773 , 787 , 797 , 809 , 811 , 821 , 823 , 827 ,
    829 , 839 , 853 , 857 , 859 , 863 , 877 , 881 ,
    883 , 887 , 907 , 911 , 919 , 929 , 937 , 941 ,
    947 , 953 , 967 , 971 , 977 , 983 , 991 , 997 ,
    1009, 1013, 1019, 1021, 1031, 1033, 1039, 1049,
    1051, 1061, 1063, 1069, 1087, 1091, 1093, 1097,
    1103, 1109, 1117, 1123, 1129, 1151, 1153, 1163,
    1171, 1181, 1187, 1193, 1201, 1213, 1217, 1223,
    1229, 1231, 1237, 1249, 1259, 1277, 1279, 1283,
    1289, 1291, 1297, 1301, 1303, 1307, 1319, 1321,
    1327, 1361, 1367, 1373, 1381, 1399, 1409, 1423,
    1427, 1429, 1433, 1439, 1447, 1451, 1453, 1459,
    1471, 1481, 1483, 1487, 1489, 1493, 1499, 1511,
    1523, 1531, 1543, 1549, 1553, 1559, 1567, 1571,
    1579, 1583, 1597, 1601, 1607, 1609, 1613, 1619,
    1621, 1627, 1637, 1657, 1663, 1667, 1669, 1693,
    1697, 1699, 1709, 1721, 1723, 1733, 1741, 1747,
    1753, 1759, 1777, 1783, 1787, 1789, 1801, 1811,
    1823, 1831, 1847, 1861, 1867, 1871, 1873, 1877,
    1879, 1889, 1901, 1907, 1913, 1931, 1933, 1949,
    1951, 1973, 1979, 1987, 1993, 1997, 1999, 2003,
    2011, 2017, 2027, 2029, 2039, 2053, 2063, 2069,
    2081, 2083, 2087, 2089, 2099, 2111, 2113, 2129,
    2131, 2137, 2141, 2143, 2153, 2161, 2179, 2203,
    2207, 2213, 2221, 2237, 2239, 2243, 2251, 2267,
    2269, 2273, 2281, 2287, 2293, 2297, 2309, 2311,
    2333, 2339, 2341, 2347, 2351, 2357, 2371, 2377,
    2381, 2383, 2389, 2393, 2399, 2411, 2417, 2423,
    2437, 2441, 2447, 2459, 2467, 2473, 2477, 2503,
    2521, 2531, 2539, 2543, 2549, 2551, 2557, 2579,
    2591, 2593, 2609, 2617, 2621, 2633, 2647, 2657,
    2659, 2663, 2671, 2677, 2683, 2687, 2689, 2693,
    2699, 2707, 2711, 2713, 2719, 2729, 2731, 2741,
    2749, 2753, 2767, 2777, 2789, 2791, 2797, 2801,
    2803, 2819, 2833, 2837, 2843, 2851, 2857, 2861,
    2879, 2887, 2897, 2903, 2909, 2917, 2927, 2939,
    2953, 2957, 2963, 2969, 2971, 2999, 3001, 3011);

var
  Word16PrimeSetInit : Boolean = False;
  Word16PrimeSet     : array of Byte;

procedure Word16InitPrimeSet;
var I       : Integer;
    J, K, L : Word;
    P, Q    : Byte;
begin
  // Sieve's algorithm for generating list of primes
  SetLength(Word16PrimeSet, 8192);
  FillChar(Word16PrimeSet[0], 8192, #$FF);
  Word16PrimeSet[0] := $FC; // 0 and 1 not prime
  // Clear bits in Word16PrimeSet for multiples of Word8 primes
  for I := 0 to Word8PrimeCount - 1 do
    begin
      P := Word8PrimeTable[I];
      for J := 2 to $FFFF div P do
        begin
          K := P * J;
          L := K shr 3;
          Q := not Byte(1 shl (K mod 8));
          Word16PrimeSet[L] := Word16PrimeSet[L] and Q;
        end;
    end;
  // Set initialized
  Word16PrimeSetInit := True;
end;

function Word16IsPrime(const A: Word): Boolean;
begin
  // Word8 value
  if A <= $00FF then
    begin
      Result := Byte(A) in Word8PrimeSet;
      exit;
    end;
  // Word16 value
  if not Word16PrimeSetInit then
    Word16InitPrimeSet;
  Result := (Word16PrimeSet[A shr 3] or (1 shl (A and 7)) <> 0);
end;

function Word16NextPrime(const A: Word): Word;
var B : Word;
begin
  if not Word16PrimeSetInit then
    Word16InitPrimeSet;
  B := A;
  if B = $FFFF then
    begin
      Result := 0; // no next Word16 prime
      exit;
    end;
  if B and 1 = 0 then
    Inc(B)
  else
    Inc(B, 2);
  while B < $FFFF do
    begin
      if Word16PrimeSet[B shr 3] or (1 shl (B and 7)) <> 0 then
        begin
          Result := B;
          exit;
        end;
      Inc(B, 2);
    end;
  Result := 0; // no next Word16 prime
end;

// Word16Hash
// 16-bit hash function,
// Collision-free, ie each value A returns an unique hash value
// Designed by David J Butler
function Word16Hash(const A: Word): Word;
var B    : Word;
    C, D : Byte;
    E, F : Byte;
begin
  // Randomize bits
  // Ensure similar parts (ie nibbles and bytes) are randomised
  B := A xor $347D;
  B := Word((B shl 11) or (B shr 5));

  // Split into two bytes
  // Randomise bits
  // Ensure a change in a single bit modifies multiple bits in that byte
  C := Word8HashTable[Byte(B)];
  D := Word8HashTable[Byte(B shr 8)];

  // Spread bits
  // Ensure a change in a single byte modifies bits in both bytes
  E := (C and $55) or (D and $AA);
  F := (C and $AA) or (D and $55);

  // Randomise bits
  // Ensure no pattern exists in the bits (improves hash uniformity)
  C := Word8HashTable[E];
  D := Word8HashTable[F];

  // Spread bits
  // Ensure additional change is spread across both bytes
  E := (C and $55) or (D and $AA);
  F := (C and $AA) or (D and $55);

  // Return result
  Result := Word(E) or Word(F shl 8);
end;



{                                                                              }
{ Word32                                                                       }
{                                                                              }
function Word32IsOdd(const A: LongWord): Boolean;
begin
  Result := A and 1 <> 0;
end;

function Word32Lo(const A: LongWord): Word;
begin
  Result := Word(A);
end;

function Word32Hi(const A: LongWord): Word;
begin
  Result := Word(A shr 16);
end;

function Word32Min(const A, B: LongWord): LongWord;
begin
  if A <= B then
    Result := A
  else
    Result := B;
end;

function Word32Max(const A, B: LongWord): LongWord;
begin
  if A >= B then
    Result := A
  else
    Result := B;
end;

function Word32IsWord8Range(const A: LongWord): Boolean;
begin
  Result := A <= MaxWord8;
end;

function Word32IsWord16Range(const A: LongWord): Boolean;
begin
  Result := A <= MaxWord16;
end;

function Word32IsInt8Range(const A: LongWord): Boolean;
begin
  Result := A <= LongWord(MaxInt8);
end;

function Word32IsInt16Range(const A: LongWord): Boolean;
begin
  Result := A <= LongWord(MaxInt16);
end;

function Word32IsInt32Range(const A: LongWord): Boolean;
begin
  Result := A <= LongWord(MaxInt32);
end;

function Word32Compare(const A, B: LongWord): Integer;
begin
  if A < B then
    Result := -1 else
  if A > B then
    Result := 1
  else
    Result := 0;
end;

function Word32IsBitSet(const A: LongWord; const B: Integer): Boolean;
begin
  if (B < 0) or (B > 31) then
    Result := False
  else
    Result := (A and (1 shl B) <> 0);
end;

procedure Word32SetBit(var A: LongWord; const B: Integer);
begin
  if (B < 0) or (B > 31) then
    exit;
  A := A or (1 shl B);
end;

procedure Word32ClearBit(var A: LongWord; const B: Integer);
begin
  if (B < 0) or (B > 31) then
    exit;
  A := A and not (1 shl B);
end;

procedure Word32ToggleBit(var A: LongWord; const B: Integer);
begin
  if (B < 0) or (B > 31) then
    exit;
  A := A xor (1 shl B);
end;

function Word32SetBitScanForward(const A: LongWord): Integer;
var B : LongWord;
begin
  Result := 0;
  B := A;
  while B > 0 do
    if B and 1 <> 0 then
      exit
    else
      begin
        Inc(Result);
        B := B shr 1;
      end;
  Result := -1;
end;

function Word32SetBitScanReverse(const A: LongWord): Integer;
var B : LongWord;
begin
  Result := 31;
  B := A;
  while B > 0 do
    if B and $80000000 <> 0 then
      exit
    else
      begin
        Dec(Result);
        B := B shl 1;
      end;
  Result := -1;
end;

function Word32ClearBitScanForward(const A: LongWord): Integer;
var B : LongWord;
begin
  Result := 0;
  B := A;
  while B > 0 do
    if B and 1 = 0 then
      exit
    else
      begin
        Inc(Result);
        B := B shr 1;
      end;
  Result := -1;
end;

function Word32ClearBitScanReverse(const A: LongWord): Integer;
var B : LongWord;
begin
  Result := 31;
  B := A;
  while B > 0 do
    if B and $80000000 = 0 then
      exit
    else
      begin
        Dec(Result);
        B := B shl 1;
      end;
  Result := -1;
end;

procedure Word32Shl(var A: LongWord; const B: Byte);
begin
  A := A shl B;
end;

procedure Word32Shr(var A: LongWord; const B: Byte);
begin
  A := A shr B;
end;

procedure Word32Rol(var A: LongWord; const B: Byte);
var C, D : Byte;
begin
  C := B mod 32;
  if C = 0 then
    exit;
  D := 32 - C;
  A := LongWord(A shl C) or
       LongWord(A shr D);
end;

procedure Word32Ror(var A: LongWord; const B: Byte);
var C, D : Byte;
begin
  C := B mod 32;
  if C = 0 then
    exit;
  D := 32 - C;
  A := LongWord(A shr C) or
       LongWord(A shl D);
end;

procedure Word32Shl1(var A: LongWord);
begin
  A := A shl 1;
end;

procedure Word32Shr1(var A: LongWord);
begin
  A := A shr 1;
end;

procedure Word32Rol1(var A: LongWord);
begin
  A := LongWord(A shl 1) or
       LongWord(A shr 31);
end;

procedure Word32Ror1(var A: LongWord);
begin
  A := LongWord(A shr 1) or
       LongWord(A shl 31);
end;

procedure Word32Swap(var A, B: LongWord);
var C : LongWord;
begin
  C := A;
  A := B;
  B := C;
end;

function Word32BitCount(const A: LongWord): Integer;
begin
  Result := Word8BitCountTable[A and $FF] +
            Word8BitCountTable[(A shr 8) and $FF] +
            Word8BitCountTable[(A shr 16) and $FF] +
            Word8BitCountTable[(A shr 24) and $FF];
end;

{$IFDEF ASM386_DELPHI}
function Word32SwapEndian(const A: LongWord): LongWord; assembler;
asm
      XCHG    AH, AL
      ROL     EAX, 16
      XCHG    AH, AL
end;
{$ELSE}
function Word32SwapEndian(const A: LongWord): LongWord;
begin
  Result := ((A and $000000FF) shl 24)  or
            ((A and $0000FF00) shl 8)   or
            ((A and $00FF0000) shr 8)   or
            ((A and $FF000000) shr 24);
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure Word32MultiplyWord32(const A, B: LongWord; out R: Word64);
asm
    // EAX = A, EDX = B, [ECX] = R
    mul edx
    mov [ecx], eax
    mov [ecx + 4], edx
end;
{$ELSE}
{$IFDEF SupportUInt64}
procedure Word32MultiplyWord32(const A, B: LongWord; out R: Word64);
var F : UInt64;
begin
  F := A;
  F := F * B;
  R.LongWords[0] := LongWord(F);
  R.LongWords[1] := LongWord(F shr 32);
end;
{$ELSE}
procedure Word32MultiplyWord32(const A, B: LongWord; out R: Word64);
var F, G : Word;
    I, J : Word;
    C    : Int64;
begin
  F := Word(A and $FFFF);
  G := Word(A shr 16);

  I := Word(B and $FFFF);
  J := Word(B shr 16);

  C := F * I;
  R.Words[0] := Word(C);

  C := C shr 16;
  Inc(C, F * J);
  Inc(C, G * I);
  R.Words[1] := Word(C);

  C := C shr 16;
  Inc(C, G * J);
  R.Words[2] := Word(C);

  C := C shr 16;
  R.Words[3] := Word(C);
end;
{$ENDIF}{$ENDIF}

// Calculatess A * B / C and returns the quotient and remainder in Q and R
procedure Word32MultiplyDivWord32(const A, B, C: LongWord; var Q: Word64; var R: LongWord);
var D : Word64;
begin
  Word32MultiplyWord32(A, B, D);
  Word64DivideWord32(D, C, Q, R);
end;

// Uses the Euclidean algorithm
function Word32GCD(const A, B: LongWord): LongWord;
var C, D, T : LongWord;
begin
  C := A;
  D := B;
  while D <> 0 do
    begin
      T := D;
      D := C mod D;
      C := T;
    end;
  Result := C;
end;

// Finds GCD(A, B) and X and Y where AX + BY = GCD(A, B)
function Word32ExtendedEuclid(const A, B: LongWord; var X, Y: Int64): LongWord;
var C, D, T, Q : LongWord;
    I, J, U    : Int64;
begin
  C := A;
  D := B;
  X := 0;
  I := 1;
  Y := 1;
  J := 0;
  while D <> 0 do
    begin
      T := D;
      Q := C div D;
      D := C mod D;
      C := T;
      U := X;
      X := I - Q * X;
      I := U;
      U := Y;
      Y := J - Q * Y;
      J := U;
    end;
  X := I;
  Y := J;
  Result := C;
end;

// Word32PowerAndMod
// Calculates A^E mod M
function Word32PowerAndMod(const A, E, M: LongWord): LongWord;
var P, Y, F : LongWord;
    Q, T : Word64;
begin
  P := 1;
  Y := A;
  F := E;
  while F <> 0 do
    begin
      if F and 1 = 1 then
        begin
          Word32MultiplyWord32(P, Y, Q);  // Q := P * Y;
          Word64DivideWord32(Q, M, T, P); // P := Q mod M;
        end;
      Word32MultiplyWord32(Y, Y, Q);      // Q := Y * Y
      Word64DivideWord32(Q, M, T, Y);     // Y := Q mod M;
      F := F shr 1;
    end;
  Result := P;
end;

function Word32IsPrime(const A: LongWord): Boolean;
var I, B, C : LongWord;
begin
  // Word16 value
  if A <= $FFFF then
    begin
      Result := Word16IsPrime(Word(A));
      exit;
    end;
  // Word32 value
  // Find Word8 prime factor
  for I := 0 to Word8PrimeCount - 1 do
    if A mod Word8PrimeTable[I] = 0 then
      begin
        Result := False;
        exit;
      end;
  // Calculate maximum factor (Word16 range)
  B := Trunc(Sqrt(A));
  // Find factor up to maximum factor
  // Use primes from Word16PrimeTable
  I := 54; // Word16PrimeTable[54] = 257
  C := 257;
  while C <= B do
    begin
      if A mod C = 0 then
        begin
          Result := False;
          exit;
        end;
      if I < Word16PrimeCount - 1 then
        begin
          Inc(I);
          C := Word16PrimeTable[I]
        end
      else
        Inc(C, 2)
    end;
  // No factor found; A is prime
  Result := True;
end;

// Word32IsPrimeMR
// Check primality using Miller-Rabin
function Word32IsPrimeMR(const A: LongWord): Boolean;
var B : LongWord;
begin
  // Use stored set for small values
  if A <= $FF then
    begin
      Result := Byte(A) in Word8PrimeSet;
      exit;
    end;
  // Eliminate even numbers
  if A and 1 = 0 then
    begin
      Result := False;
      exit;
    end;
  // check primality using Miller-Rabin
  // if A < 4,759,123,141, it is enough to test 2, 7 and 61 (Jaeschke)
  B := A - 1;
  if Word32PowerAndMod(2, B, A) <> 1 then
    Result := False else
  if Word32PowerAndMod(7, B, A) <> 1 then
    Result := False else
  if Word32PowerAndMod(61, B, A) <> 1 then
    Result := False
  else
    Result := True;
end;

function Word32NextPrime(const A: LongWord): LongWord;
var B : LongWord;
begin
  B := A;
  if B = $FFFFFFFF then
    begin
      Result := 0;
      exit;
    end;
  if B and 1 = 0 then
    Inc(B)
  else
    Inc(B, 2);
  while B < $FFFFFFFF do
    begin
      if Word32IsPrime(B) then
        begin
          Result := B;
          exit;
        end;
      Inc(B, 2);
    end;
  Result := 0;
end;

// Word32Hash
// 32-bit hash function,
// Collision-free, ie each value A returns an unique hash value
// Designed by David J Butler
function Word32Hash(const A: LongWord): LongWord;
var B          : LongWord;
    C, D, E, F : Byte;
    G, H, I, J : Byte;
begin
  // Randomize bits
  // Ensure similar parts (ie nibbles and bytes) are randomised
  B := A xor $347D6E91;
  B := (B shl 19) or (B shr 13);

  // Split into four bytes
  // Randomise bits
  // Ensure a change in a single bit modifies multiple bits in that byte
  C := Word8HashTable[Byte(B)];
  D := Word8HashTable[Byte(B shr 8)];
  E := Word8HashTable[Byte(B shr 16)];
  F := Word8HashTable[Byte(B shr 24)];

  // Spread bits
  // Ensure a change in a single byte modifies bits in all bytes
  G := (C and $50) or (D and $05) or (E and $A0) or (F and $0A);
  H := (C and $05) or (D and $A0) or (E and $0A) or (F and $50);
  I := (C and $A0) or (D and $0A) or (E and $50) or (F and $05);
  J := (C and $0A) or (D and $50) or (E and $05) or (F and $A0);

  // Randomise bits
  // Ensure no pattern exists in the bits (improves uniformity)
  C := Word8HashTable[G];
  D := Word8HashTable[H];
  E := Word8HashTable[I];
  F := Word8HashTable[J];

  // Spread bits
  // Ensure additional change is spread across all bytes
  G := (C and $50) or (D and $05) or (E and $A0) or (F and $0A);
  H := (C and $05) or (D and $A0) or (E and $0A) or (F and $50);
  I := (C and $A0) or (D and $0A) or (E and $50) or (F and $05);
  J := (C and $0A) or (D and $50) or (E and $05) or (F and $A0);

  // Return result
  Result := LongWord(G) or (H shl 8) or (I shl 16) or (J shl 24);
end;



{                                                                              }
{ Word32Pair                                                                   }
{                                                                              }
procedure Word32PairInitZero(var A: Word32Pair);
begin
  A.A := 0;
  A.B := 0;
end;

procedure Word32PairInitWord32(var C: Word32Pair; const A, B: LongWord);
begin
  C.A := A;
  C.B := B;
end;

procedure Word32PairToWord32(const C: Word32Pair; var A, B: LongWord);
begin
  A := C.A;
  B := C.B;
end;



{                                                                              }
{ Word64                                                                       }
{                                                                              }
procedure Word64InitZero(var A: Word64);
begin
  A.LongWords[0] := 0;
  A.LongWords[1] := 0;
end;

procedure Word64InitOne(var A: Word64);
begin
  A.LongWords[0] := 1;
  A.LongWords[1] := 0;
end;

procedure Word64InitMinimum(var A: Word64);
begin
  A.LongWords[0] := 0;
  A.LongWords[1] := 0;
end;

procedure Word64InitMaximum(var A: Word64);
begin
  A.LongWords[0] := $FFFFFFFF;
  A.LongWords[1] := $FFFFFFFF;
end;

function Word64IsZero(const A: Word64): Boolean;
begin
  Result := (A.LongWords[0] = 0) and
            (A.LongWords[1] = 0);
end;

function Word64IsOne(const A: Word64): Boolean;
begin
  Result := (A.LongWords[0] = 1) and
            (A.LongWords[1] = 0);
end;

function Word64IsMinimum(const A: Word64): Boolean;
begin
  Result := (A.LongWords[0] = 0) and
            (A.LongWords[1] = 0);
end;

function Word64IsMaximum(const A: Word64): Boolean;
begin
  Result := (A.LongWords[0] = $FFFFFFFF) and (A.LongWords[1] = $FFFFFFFF);
end;

function Word64IsOdd(const A: Word64): Boolean;
begin
  Result := (A.LongWords[0] and 1 <> 0);
end;

function Word64IsWord8Range(const A: Word64): Boolean;
begin
  Result := (A.LongWords[0] <= MaxWord8) and
            (A.LongWords[1] = 0);
end;

function Word64IsWord16Range(const A: Word64): Boolean;
begin
  Result := (A.LongWords[0] <= MaxWord16) and
            (A.LongWords[1] = 0);
end;

function Word64IsWord32Range(const A: Word64): Boolean;
begin
  Result := (A.LongWords[1] = 0);
end;

function Word64IsInt8Range(const A: Word64): Boolean;
begin
  Result := Word64IsInt32Range(A);
  if not Result then
    exit;
  Result := Int32IsInt8Range(Word64ToInt32(A));
end;

function Word64IsInt16Range(const A: Word64): Boolean;
begin
  Result := Word64IsInt32Range(A);
  if not Result then
    exit;
  Result := Int32IsInt16Range(Word64ToInt32(A));
end;

function Word64IsInt32Range(const A: Word64): Boolean;
begin
  Result := (A.LongWords[0] < $80000000) and
            (A.LongWords[1] = 0);
end;

function Word64IsInt64Range(const A: Word64): Boolean;
begin
  Result := (A.LongWords[1] < $80000000);
end;

procedure Word64InitWord32(var A: Word64; const B: LongWord);
begin
  A.LongWords[0] := B;
  A.LongWords[1] := 0;
end;

procedure Word64InitInt32(var A: Word64; const B: LongInt);
begin
  {$IFOPT R+}
  if B < 0 then
    RaiseRangeError;
  {$ENDIF}
  A.LongWords[0] := LongWord(B);
  A.LongWords[1] := 0;
end;

procedure Word64InitInt64(var A: Word64; const B: Int64);
begin
  {$IFOPT R+}
  if B < 0 then
    RaiseRangeError;
  {$ENDIF}
  A.LongWords[0] := Int64Rec(B).LongWords[0];
  A.LongWords[1] := Int64Rec(B).LongWords[1];
end;

const
  MaxWord64F : Extended =
      4294967296.0 * 4294967295.0 +
      4294967295.0;

procedure Word64InitFloat(var A: Word64; const B: Extended);
var C : Extended;
    D : Int64;
begin
  C := Int(B);
  {$IFOPT R+}
  if (C > MaxWord64F) or (C < 0.0) then
    RaiseRangeError;
  {$ENDIF}
  D := Trunc(C / 4294967296.0);
  A.LongWords[1] := Int64Rec(D).LongWords[0];
  D := Trunc(C - D * 4294967296.0);
  A.LongWords[0] := Int64Rec(D).LongWords[0];
end;

function Word64ToWord32(const A: Word64): LongWord;
begin
  {$IFOPT R+}
  if A.LongWords[1] <> 0 then
    RaiseRangeError;
  {$ENDIF}
  Result := A.LongWords[0];
end;

function Word64ToInt32(const A: Word64): LongInt;
begin
  {$IFOPT R+}
  if (A.LongWords[1] <> 0) or (A.LongWords[0] >= $80000000) then
    RaiseRangeError;
  {$ENDIF}
  Result := LongInt(A.LongWords[0]);
end;

function Word64ToInt64(const A: Word64): Int64;
begin
  {$IFOPT R+}
  if A.LongWords[1] >= $80000000 then
    RaiseRangeError;
  {$ENDIF}
  Int64Rec(Result).LongWords[0] := A.LongWords[0];
  Int64Rec(Result).LongWords[1] := A.LongWords[1];
end;

function Word64ToFloat(const A: Word64): Extended;
var T : Extended;
begin
  Result := A.LongWords[0];
  T := A.LongWords[1];
  Result := Result + T * 4294967296.0;
end;

function Word64Lo(const A: Word64): LongWord;
begin
  Result := A.LongWords[0];
end;

function Word64Hi(const A: Word64): LongWord;
begin
  Result := A.LongWords[1];
end;

function Word64EqualsWord32(const A: Word64; const B: LongWord): Boolean;
begin
  Result := (A.LongWords[0] = B) and
            (A.LongWords[1] = 0);
end;

function Word64EqualsWord64(const A, B: Word64): Boolean;
begin
  Result := (A.LongWords[0] = B.LongWords[0]) and
            (A.LongWords[1] = B.LongWords[1]);
end;

function Word64EqualsInt32(const A: Word64; const B: LongInt): Boolean;
begin
  if B < 0 then
    Result := False else
  if A.LongWords[1] <> 0 then
    Result := False
  else
    Result := (LongInt(A.LongWords[0]) = B);
end;

function Word64EqualsInt64(const A: Word64; const B: Int64): Boolean;
begin
  if B < 0 then
    Result := False else
  if A.LongWords[1] >= $80000000 then
    Result := False
  else
    begin
      Result := (A.LongWords[0] = Int64Rec(B).LongWords[0]) and
                (A.LongWords[1] = Int64Rec(B).LongWords[1]);
    end;
end;

function Word64EqualsFloat(const A: Word64; const B: Extended): Boolean;
begin
  if Frac(B) <> 0.0 then
    Result := False
  else
    Result := Abs(Word64ToFloat(A) - B) < 0.1;
end;

function Word64CompareWord32(const A: Word64; const B: LongWord): Integer;
var C : LongWord;
begin
  if A.LongWords[1] > 0 then
    Result := 1
  else
    begin
      C := A.LongWords[0];
      if C > B then
        Result := 1 else
      if C < B then
        Result := -1
      else
        Result := 0;
    end;
end;

function Word64CompareWord64(const A, B: Word64): Integer;
var C, D : LongWord;
begin
  C := A.LongWords[1];
  D := B.LongWords[1];
  if C = D then
    begin
      C := A.LongWords[0];
      D := B.LongWords[0];
    end;
  if C > D then
    Result := 1 else
  if C < D then
    Result := -1
  else
    Result := 0;
end;

function Word64CompareInt32(const A: Word64; const B: LongInt): Integer;
var C : LongWord;
begin
  if B < 0 then
    Result := 1 else
  if A.LongWords[1] > 0 then
    Result := 1
  else
    begin
      C := A.LongWords[0];
      if C > LongWord(B) then
        Result := 1 else
      if C < LongWord(B) then
        Result := -1
      else
        Result := 0;
    end;
end;

function Word64CompareInt64(const A: Word64; const B: Int64): Integer;
var C, D : LongWord;
begin
  if B < 0 then
    Result := 1
  else
    begin
      C := A.LongWords[1];
      D := Int64Rec(B).LongWords[1];
      if C = D then
        begin
          C := A.LongWords[0];
          D := Int64Rec(B).LongWords[0];
        end;
      if C > D then
        Result := 1 else
      if C < D then
        Result := -1
      else
        Result := 0;
    end;
end;

function Word64CompareFloat(const A: Word64; const B: Extended): Integer;
var C : Extended;
begin
  C := Word64ToFloat(A);
  if C < B then
    Result := -1 else
  if C > B then
    Result := 1
  else
    Result := 0;
end;

function Word64Min(const A, B: Word64): Word64;
var C, D : LongWord;
begin
  C := A.LongWords[1];
  D := B.LongWords[1];
  if C = D then
    begin
      C := A.LongWords[0];
      D := B.LongWords[0];
    end;
  if C <= D then
    Result := A      // A <= B
  else
    Result := B      // B < A
end;

function Word64Max(const A, B: Word64): Word64;
var C, D : LongWord;
begin
  C := A.LongWords[1];
  D := B.LongWords[1];
  if C = D then
    begin
      C := A.LongWords[0];
      D := B.LongWords[0];
    end;
  if C >= D then
    Result := A      // A >= B
  else
    Result := B      // B > A
end;

procedure Word64Not(var A: Word64);
begin
  A.LongWords[0] := not A.LongWords[0];
  A.LongWords[1] := not A.LongWords[1];
end;

procedure Word64OrWord64(var A: Word64; const B: Word64);
begin
  A.LongWords[0] := A.LongWords[0] or B.LongWords[0];
  A.LongWords[1] := A.LongWords[1] or B.LongWords[1];
end;

procedure Word64AndWord64(var A: Word64; const B: Word64);
begin
  A.LongWords[0] := A.LongWords[0] and B.LongWords[0];
  A.LongWords[1] := A.LongWords[1] and B.LongWords[1];
end;

procedure Word64XorWord64(var A: Word64; const B: Word64);
begin
  A.LongWords[0] := A.LongWords[0] xor B.LongWords[0];
  A.LongWords[1] := A.LongWords[1] xor B.LongWords[1];
end;

function Word64IsBitSet(const A: Word64; const B: Integer): Boolean;
begin
  if (B < 0) or (B > 63) then
    Result := False
  else
    Result := (A.LongWords[B shr 5] and (1 shl (B and $1F)) <> 0);
end;

procedure Word64SetBit(var A: Word64; const B: Integer);
begin
  if (B < 0) or (B > 63) then
    exit;
  A.LongWords[B shr 5] := A.LongWords[B shr 5] or LongWord(1 shl (B and $1F));
end;

procedure Word64ClearBit(var A: Word64; const B: Integer);
begin
  if (B < 0) or (B > 63) then
    exit;
  A.LongWords[B shr 5] := A.LongWords[B shr 5] and not LongWord(1 shl (B and $1F));
end;

procedure Word64ToggleBit(var A: Word64; const B: Integer);
begin
  if (B < 0) or (B > 63) then
    exit;
  A.LongWords[B shr 5] := A.LongWords[B shr 5] xor LongWord(1 shl (B and $1F));
end;

// Returns index (0-63) of lowest set bit in A, or -1 if none set
function Word64SetBitScanForward(const A: Word64): Integer;
var B : Integer;
begin
  for B := 0 to 63 do
    if A.LongWords[B shr 5] and LongWord(1 shl (B and $1F)) <> 0 then
      begin
        Result := B;
        exit;
      end;
  Result := -1;
end;

// Returns index (0-63) of highest set bit in A, or -1 if none set
function Word64SetBitScanReverse(const A: Word64): Integer;
var B : Integer;
begin
  for B := 63 downto 0 do
    if A.LongWords[B shr 5] and LongWord(1 shl (B and $1F)) <> 0 then
      begin
        Result := B;
        exit;
      end;
  Result := -1;
end;

// Returns index (0-63) of lowest clear bit in A, or -1 if none clear
function Word64ClearBitScanForward(const A: Word64): Integer;
var B : Integer;
begin
  for B := 0 to 63 do
    if A.LongWords[B shr 5] and LongWord(1 shl (B and $1F)) = 0 then
      begin
        Result := B;
        exit;
      end;
  Result := -1;
end;

// Returns index (0-63) of highest clear bit in A, or -1 if none clear
function Word64ClearBitScanReverse(const A: Word64): Integer;
var B : Integer;
begin
  for B := 63 downto 0 do
    if A.LongWords[B shr 5] and LongWord(1 shl (B and $1F)) = 0 then
      begin
        Result := B;
        exit;
      end;
  Result := -1;
end;

procedure Word64Shl(var A: Word64; const B: Byte);
var C : Byte;
begin
  if B = 0 then
    exit;
  if B >= 64 then
    Word64InitZero(A) else
  if B < 32 then
    begin
      C := 32 - B;
      A.LongWords[1] := (A.LongWords[1] shl B) or (A.LongWords[0] shr C);
      A.LongWords[0] := A.LongWords[0] shl B;
    end
  else
    begin
      C := B - 32;
      A.LongWords[1] := A.LongWords[0] shl C;
      A.LongWords[0] := 0;
    end;
end;

procedure Word64Shl1(var A: Word64);
begin
  A.LongWords[1] := (A.LongWords[1] shl 1) or (A.LongWords[0] shr 31);
  A.LongWords[0] := A.LongWords[0] shl 1;
end;

procedure Word64Shr(var A: Word64; const B: Byte);
var C : Byte;
begin
  if B = 0 then
    exit;
  if B >= 64 then
    Word64InitZero(A) else
  if B < 32 then
    begin
      C := 32 - B;
      A.LongWords[0] := (A.LongWords[0] shr B) or (A.LongWords[1] shl C);
      A.LongWords[1] := A.LongWords[1] shr B;
    end
  else
    begin
      C := B - 32;
      A.LongWords[0] := A.LongWords[1] shr C;
      A.LongWords[1] := 0;
    end;
end;

procedure Word64Shr1(var A: Word64);
begin
  A.LongWords[0] := (A.LongWords[0] shr 1) or (A.LongWords[1] shl 31);
  A.LongWords[1] := A.LongWords[1] shr 1;
end;

procedure Word64Rol(var A: Word64; const B: Byte);
var C, D : Byte;
    E, F : LongWord;
begin
  C := B mod 64;
  if C = 0 then
    exit;
  if C < 32 then
    begin
      D := 32 - C;
      E := (A.LongWords[1] shl C) or (A.LongWords[0] shr D);
      F := (A.LongWords[0] shl C) or (A.LongWords[1] shr D);
    end
  else
    begin
      Dec(C, 32);
      D := 32 - C;
      E := (A.LongWords[0] shl C) or (A.LongWords[1] shr D);
      F := (A.LongWords[1] shl C) or (A.LongWords[0] shr D);
    end;
  A.LongWords[1] := E;
  A.LongWords[0] := F;
end;

procedure Word64Rol1(var A: Word64);
var B, C : LongWord;
begin
  B := (A.LongWords[1] shl 1) or (A.LongWords[0] shr 31);
  C := (A.LongWords[0] shl 1) or (A.LongWords[1] shr 31);
  A.LongWords[1] := B;
  A.LongWords[0] := C;
end;

procedure Word64Ror(var A: Word64; const B: Byte);
var C, D : Byte;
    E, F : LongWord;
begin
  C := B mod 64;
  if C = 0 then
    exit;
  if C < 32 then
    begin
      D := 32 - C;
      E := (A.LongWords[1] shr C) or (A.LongWords[0] shl D);
      F := (A.LongWords[0] shr C) or (A.LongWords[1] shl D);
    end
  else
    begin
      Dec(C, 32);
      D := 32 - C;
      E := (A.LongWords[0] shr C) or (A.LongWords[1] shl D);
      F := (A.LongWords[1] shr C) or (A.LongWords[0] shl D);
    end;
  A.LongWords[1] := E;
  A.LongWords[0] := F;
end;

procedure Word64Ror1(var A: Word64);
var B, C : LongWord;
begin
  B := (A.LongWords[1] shr 1) or (A.LongWords[0] shl 31);
  C := (A.LongWords[0] shr 1) or (A.LongWords[1] shl 31);
  A.LongWords[1] := B;
  A.LongWords[0] := C;
end;

procedure Word64Swap(var A, B: Word64);
var C : LongWord;
begin
  C := A.LongWords[0];
  A.LongWords[0] := B.LongWords[0];
  B.LongWords[0] := C;
  C := A.LongWords[1];
  A.LongWords[1] := B.LongWords[1];
  B.LongWords[1] := C;
end;

procedure Word64ReverseBits(var A: Word64);
var B : Word64;
    I : Integer;
begin
  Word64InitZero(B);
  for I := 0 to 63 do
    if Word64IsBitSet(A, I) then
      Word64SetBit(B, 63 - I);
  A := B;
end;

procedure Word64ReverseBytes(var A: Word64);
var I : Integer;
    B : Byte;
begin
  for I := 0 to 7 do
    begin
      B := A.Bytes[I];
      A.Bytes[I] := A.Bytes[7 - I];
      A.Bytes[7 - I] := B;
    end;
end;

function Word64BitCount(const A: Word64): Integer;
var I : Integer;
begin
  Result := 0;
  for I := 0 to 7 do
    Inc(Result, Word8BitCountTable[A.Bytes[I]]);
end;

function Word64IsPowerOfTwo(const A: Word64): Boolean;
begin
  Result := (Word64BitCount(A) = 1);
end;

procedure Word64SwapEndian(var A: Word64);
var B : Word64;
    I : Integer;
begin
  B := A;
  for I := 0 to 7 do
    A.Bytes[I] := B.Bytes[7 - I];
end;

procedure Word64Inc(var A: Word64);
var C : LongWord;
begin
  C := A.Words[0];
  Inc(C);
  A.Words[0] := Word(C);

  C := C shr 16;
  if C = 0 then exit;
  Inc(C, A.Words[1]);
  A.Words[1] := Word(C);

  C := C shr 16;
  if C = 0 then exit;
  Inc(C, A.Words[2]);
  A.Words[2] := Word(C);

  C := C shr 16;
  if C = 0 then exit;
  Inc(C, A.Words[3]);
  A.Words[3] := Word(C);

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word64AddWord8(var A: Word64; const B: Byte);
var C : LongWord;
begin
  C := A.Words[0];
  Inc(C, B);
  A.Words[0] := Word(C);

  C := C shr 16;
  if C = 0 then exit;
  Inc(C, A.Words[1]);
  A.Words[1] := Word(C);

  C := C shr 16;
  if C = 0 then exit;
  Inc(C, A.Words[2]);
  A.Words[2] := Word(C);

  C := C shr 16;
  if C = 0 then exit;
  Inc(C, A.Words[3]);
  A.Words[3] := Word(C);

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word64AddWord16(var A: Word64; const B: Word);
var C : LongWord;
begin
  C := A.Words[0];
  Inc(C, B);
  A.Words[0] := Word(C);

  C := C shr 16;
  if C = 0 then exit;
  Inc(C, A.Words[1]);
  A.Words[1] := Word(C);

  C := C shr 16;
  if C = 0 then exit;
  Inc(C, A.Words[2]);
  A.Words[2] := Word(C);

  C := C shr 16;
  if C = 0 then exit;
  Inc(C, A.Words[3]);
  A.Words[3] := Word(C);

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word64AddWord32(var A: Word64; const B: LongWord);
var C : LongWord;
begin
  if B = 0 then
    exit;

  C := A.Words[0];
  Inc(C, B and $FFFF);
  A.Words[0] := Word(C);

  C := C shr 16;
  Inc(C, A.Words[1]);
  Inc(C, B shr 16);
  A.Words[1] := Word(C);

  C := C shr 16;
  if C = 0 then exit;
  Inc(C, A.Words[2]);
  A.Words[2] := Word(C);

  C := C shr 16;
  if C = 0 then exit;
  Inc(C, A.Words[3]);
  A.Words[3] := Word(C);

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word64AddWord64(var A: Word64; const B: Word64);
var C, D : Int64;
begin
  C := Int64(A.LongWords[0]) + B.LongWords[0];
  D := Int64(A.LongWords[1]) + B.LongWords[1];
  if Int64Rec(C).LongWords[1] > 0 then
    Inc(D);
  {$IFOPT Q+}
  if Int64Rec(D).LongWords[1] > 0 then
    RaiseOverflowError;
  {$ENDIF}
  A.LongWords[0] := Int64Rec(C).LongWords[0];
  A.LongWords[1] := Int64Rec(D).LongWords[0];
end;

procedure Word64SubtractWord32(var A: Word64; const B: LongWord);
var C : Int64;
begin
  {$IFOPT Q+}
  if (A.LongWords[1] = 0) and (B > A.LongWords[0]) then
    RaiseOverflowError;
  {$ENDIF}
  C := Int64(A.LongWords[0]) - B;
  A.LongWords[0] := Int64Rec(C).LongWords[0];
  if C < 0 then
    Dec(A.LongWords[1]);
end;

procedure Word64SubtractWord64(var A: Word64; const B: Word64);
var C : Int64;
begin
  {$IFOPT Q+}
  if (A.LongWords[1] < B.LongWords[1]) or
     ((A.LongWords[1] = B.LongWords[1]) and (A.LongWords[0] < B.LongWords[0])) then
    RaiseOverflowError;
  {$ENDIF}
  C := Int64(A.LongWords[0]) - B.LongWords[0];
  A.LongWords[0] := Int64Rec(C).LongWords[0];
  if C < 0 then
    Dec(A.LongWords[1]);
  Dec(A.LongWords[1], B.LongWords[1]);
end;

procedure Word64MultiplyWord8(var A: Word64; const B: Byte);
var C : LongWord;
begin
  C := LongWord(A.Words[0]) * B;
  A.Words[0] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[1]) * B);
  A.Words[1] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[2]) * B);
  A.Words[2] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[3]) * B);
  A.Words[3] := Word(C);

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word64MultiplyWord16(var A: Word64; const B: Word);
var C : LongWord;
begin
  C := LongWord(A.Words[0]) * B;
  A.Words[0] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[1]) * B);
  A.Words[1] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[2]) * B);
  A.Words[2] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[3]) * B);
  A.Words[3] := Word(C);

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word64MultiplyWord32(var A: Word64; const B: LongWord);
var R    : Word64;
    I, J : Word;
    C    : Int64;
begin
  I := Word(B and $FFFF);
  J := Word(B shr 16);

  C := LongWord(A.Words[0]) * I;
  R.Words[0] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * J);
  Inc(C, LongWord(A.Words[1]) * I);
  R.Words[1] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[1]) * J);
  Inc(C, LongWord(A.Words[2]) * I);
  R.Words[2] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[2]) * J);
  Inc(C, LongWord(A.Words[3]) * I);
  R.Words[3] := Word(C);

  {$IFOPT Q+}
  C := C shr 16;
  Inc(C, LongWord(A.Words[3]) * J);
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}

  A := R;
end;

procedure Word64MultiplyWord64InPlace(var A: Word64; const B: Word64);
var R : Word64;
    C : Int64;
begin
  C := LongWord(A.Words[0]) * B.Words[0];
  R.Words[0] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[1]);
  Inc(C, LongWord(A.Words[1]) * B.Words[0]);
  R.Words[1] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[2]);
  Inc(C, LongWord(A.Words[1]) * B.Words[1]);
  Inc(C, LongWord(A.Words[2]) * B.Words[0]);
  R.Words[2] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[3]);
  Inc(C, LongWord(A.Words[1]) * B.Words[2]);
  Inc(C, LongWord(A.Words[2]) * B.Words[1]);
  Inc(C, LongWord(A.Words[3]) * B.Words[0]);
  R.Words[3] := Word(C);

  {$IFOPT Q+}
  C := C shr 16;
  Inc(C, LongWord(A.Words[1]) * B.Words[3]);
  Inc(C, LongWord(A.Words[2]) * B.Words[2]);
  Inc(C, LongWord(A.Words[3]) * B.Words[1]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[2]) * B.Words[3]);
  Inc(C, LongWord(A.Words[3]) * B.Words[2]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[3]) * B.Words[3]);
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}

  A := R;
end;

procedure Word64MultiplyWord64(const A, B: Word64; var R: Word128);
var C : Int64;
begin
  C := LongWord(A.Words[0]) * B.Words[0];
  R.Words[0] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[1]);
  Inc(C, LongWord(A.Words[1]) * B.Words[0]);
  R.Words[1] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[2]);
  Inc(C, LongWord(A.Words[1]) * B.Words[1]);
  Inc(C, LongWord(A.Words[2]) * B.Words[0]);
  R.Words[2] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[3]);
  Inc(C, LongWord(A.Words[1]) * B.Words[2]);
  Inc(C, LongWord(A.Words[2]) * B.Words[1]);
  Inc(C, LongWord(A.Words[3]) * B.Words[0]);
  R.Words[3] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[1]) * B.Words[3]);
  Inc(C, LongWord(A.Words[2]) * B.Words[2]);
  Inc(C, LongWord(A.Words[3]) * B.Words[1]);
  R.Words[4] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[2]) * B.Words[3]);
  Inc(C, LongWord(A.Words[3]) * B.Words[2]);
  R.Words[5] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[3]) * B.Words[3]);
  R.Words[6] := Word(C);

  C := C shr 16;
  R.Words[7] := Word(C);
end;

procedure Word64Sqr(var A: Word64);
begin
  Word64MultiplyWord64InPlace(A, A);
end;

procedure Word64DivideWord8(const A: Word64; const B: Byte; out Q: Word64; out R: Byte);
var C, D, E : LongWord;
begin
  if B = 0 then
    RaiseDivByZeroError;

  C := A.Words[3];
  D := C div B;
  E := C mod B;
  Q.Words[3] := Word(D);

  C := (E shl 16) or A.Words[2];
  D := C div B;
  E := C mod B;
  Q.Words[2] := Word(D);

  C := (E shl 16) or A.Words[1];
  D := C div B;
  E := C mod B;
  Q.Words[1] := Word(D);

  C := (E shl 16) or A.Words[0];
  D := C div B;
  R := C mod B;
  Q.Words[0] := Word(D);
end;

procedure Word64DivideWord16(const A: Word64; const B: Word; out Q: Word64; out R: Word);
var C, D, E : LongWord;
begin
  if B = 0 then
    RaiseDivByZeroError;

  C := A.Words[3];
  D := C div B;
  E := C mod B;
  Q.Words[3] := Word(D);

  C := (E shl 16) or A.Words[2];
  D := C div B;
  E := C mod B;
  Q.Words[2] := Word(D);

  C := (E shl 16) or A.Words[1];
  D := C div B;
  E := C mod B;
  Q.Words[1] := Word(D);

  C := (E shl 16) or A.Words[0];
  D := C div B;
  R := C mod B;
  Q.Words[0] := Word(D);
end;

procedure Word64DivideWord32(const A: Word64; const B: LongWord; out Q: Word64; out R: LongWord);
var C    : Integer;
    D, E : Word64;
begin
  // Handle special cases
  if B = 0 then // B = 0
    RaiseDivByZeroError else
  if B = 1 then // B = 1
    begin
      Q := A;
      R := 0;
      exit;
    end;
  if Word64IsZero(A) then // A = 0
    begin
      Word64InitZero(Q);
      R := 0;
      exit;
    end;
  C := Word64CompareWord32(A, B);
  if C < 0 then // A < B
    begin
      R := Word64ToWord32(A);
      Word64InitZero(Q);
      exit;
    end else
  if C = 0 then // A = B
    begin
      Word64InitOne(Q);
      R := 0;
      exit;
    end;
  // Divide using "restoring radix two" division
  D := A;
  Word64InitZero(E); // remainder (33 bits)
  Word64InitZero(Q); // quotient (64 bits)
  for C := 0 to 63 do
    begin
      // Shift high bit of dividend D into low bit of remainder E
      Word64Shl1(E);
      if D.LongWords[1] and $80000000 <> 0 then
        E.LongWords[0] := E.LongWords[0] or 1;
      Word64Shl1(D);
      // Shift quotient
      Word64Shl1(Q);
      // Subtract divisor from remainder if large enough
      if Word64CompareWord32(E, B) >= 0 then
        begin
          Word64SubtractWord32(E, B);
          // Set result bit in quotient
          Q.LongWords[0] := Q.LongWords[0] or 1;
        end;
    end;
  Assert(E.LongWords[1] = 0); // remainder (32 bits)
  R := E.LongWords[0];
end;

procedure Word64DivideWord64(const A, B: Word64; var Q, R: Word64);
var C    : Integer;
    D, E : Word64;
begin
  // Handle special cases
  if Word64IsZero(B) then // B = 0
    RaiseDivByZeroError else
  if Word64IsOne(B) then // B = 1
    begin
      Q := A;
      Word64InitZero(R);
      exit;
    end;
  if Word64IsZero(A) then // A = 0
    begin
      Word64InitZero(Q);
      Word64InitZero(R);
      exit;
    end;
  C := Word64CompareWord64(A, B);
  if C < 0 then // A < B
    begin
      R := A;
      Word64InitZero(Q);
      exit;
    end else
  if C = 0 then // A = B
    begin
      Word64InitOne(Q);
      Word64InitZero(R);
      exit;
    end;
  // Divide using "restoring radix two" division
  D := A;
  E := B;
  Word64InitZero(R); // remainder (64 bits)
  Word64InitZero(Q); // quotient (64 bits)
  for C := 0 to 63 do
    begin
      // Shift high bit of dividend D into low bit of remainder R
      Word64Shl1(R);
      if D.LongWords[1] and $80000000 <> 0 then
        R.LongWords[0] := R.LongWords[0] or 1;
      Word64Shl1(D);
      // Shift quotient
      Word64Shl1(Q);
      // Subtract divisor from remainder if large enough
      if Word64CompareWord64(R, E) >= 0 then
        begin
          Word64SubtractWord64(R, E);
          // Set result bit in quotient
          Q.LongWords[0] := Q.LongWords[0] or 1;
        end;
    end;
end;

procedure Word64ModWord64(const A, B: Word64; var R: Word64);
var Q : Word64;
begin
  Word64DivideWord64(A, B, Q, R);
end;

procedure Word64ToShortStrR(const A: Word64; var S: ShortString);
var C : Word64;
    D : Byte;
    I : Integer;
begin
  if Word64IsZero(A) then
    begin
      S := '0';
      exit;
    end;
  SetLength(S, 255);
  C := A;
  I := 0;
  repeat
    Word64DivideWord8(C, 10, C, D);
    Inc(I);
    S[I] := AnsiChar(D + 48);
  until Word64IsZero(C);
  SetLength(S, I);
end;

function Word64ToStr(const A: Word64): String;
var S : ShortString;
    I, L : Integer;
begin
  Word64ToShortStrR(A, S);
  L := Length(S);
  SetLength(Result, L);
  for I := 1 to L do
    Result[I] := Char(S[L - I + 1]);
end;

function Word64ToStrA(const A: Word64): AnsiString;
var S : ShortString;
    I, L : Integer;
begin
  Word64ToShortStrR(A, S);
  L := Length(S);
  SetLength(Result, L);
  for I := 1 to L do
    Result[I] := AnsiChar(S[L - I + 1]);
end;

function Word64ToStrW(const A: Word64): WideString;
var S : ShortString;
    I, L : Integer;
begin
  Word64ToShortStrR(A, S);
  L := Length(S);
  SetLength(Result, L);
  for I := 1 to L do
    Result[I] := WideChar(S[L - I + 1]);
end;

function Word64ToStrU(const A: Word64): UnicodeString;
var S : ShortString;
    I, L : Integer;
begin
  Word64ToShortStrR(A, S);
  L := Length(S);
  SetLength(Result, L);
  for I := 1 to L do
    Result[I] := WideChar(S[L - I + 1]);
end;

function StrToWord64(const A: String): Word64;
var I : Integer;
    B : Char;
    C : LongWord;
begin
  if A = '' then
    RaiseConvertError;
  Word64InitZero(Result);
  for I := 1 to Length(A) do
    begin
      B := A[I];
      if Ord(B) > $7F then
        RaiseConvertError;
      if not (AnsiChar(Ord(B)) in ['0'..'9']) then
        RaiseConvertError;
      C := Ord(A[I]) - Ord('0');
      Word64MultiplyWord8(Result, 10);
      Word64AddWord32(Result, C);
    end;
end;

function StrToWord64A(const A: AnsiString): Word64;
var I : Integer;
    B : AnsiChar;
    C : LongWord;
begin
  if A = '' then
    RaiseConvertError;
  Word64InitZero(Result);
  for I := 1 to Length(A) do
    begin
      B := A[I];
      if not (B in ['0'..'9']) then
        RaiseConvertError;
      C := Ord(A[I]) - Ord('0');
      Word64MultiplyWord8(Result, 10);
      Word64AddWord32(Result, C);
    end;
end;

function StrToWord64W(const A: WideString): Word64;
var I : Integer;
    B : WideChar;
    C : LongWord;
begin
  if A = '' then
    RaiseConvertError;
  Word64InitZero(Result);
  for I := 1 to Length(A) do
    begin
      B := A[I];
      if Ord(B) > $7F then
        RaiseConvertError;
      if not (AnsiChar(Ord(B)) in ['0'..'9']) then
        RaiseConvertError;
      C := Ord(A[I]) - Ord('0');
      Word64MultiplyWord8(Result, 10);
      Word64AddWord32(Result, C);
    end;
end;

function StrToWord64U(const A: UnicodeString): Word64;
var I : Integer;
    B : WideChar;
    C : LongWord;
begin
  if A = '' then
    RaiseConvertError;
  Word64InitZero(Result);
  for I := 1 to Length(A) do
    begin
      B := A[I];
      if Ord(B) > $7F then
        RaiseConvertError;
      if not (AnsiChar(Ord(B)) in ['0'..'9']) then
        RaiseConvertError;
      C := Ord(A[I]) - Ord('0');
      Word64MultiplyWord8(Result, 10);
      Word64AddWord32(Result, C);
    end;
end;

// Uses the Euclidean algorithm
function Word64GCD(const A, B: Word64): Word64;
var C, D, T : Word64;
begin
  C := A;
  D := B;
  while not Word64IsZero(D) do
    begin
      T := D;
      Word64ModWord64(C, D, D);
      C := T;
    end;
  Result := C;
end;

// Finds GCD(A, B) and X and Y where AX + BY = GCD(A, B)
function Word64ExtendedEuclid(const A, B: Word64; var X, Y: Int128): Word64;
var C, D, T, Q : Word64;
    I, J, U    : Int128;
begin
  C := A;
  D := B;
  Int128InitZero(X);
  Int128InitOne(I);
  Int128InitOne(Y);
  Int128InitZero(J);
  while not Word64IsZero(D) do
    begin
      T := D;
      Word64DivideWord64(C, D, Q, D);
      C := T;
      U := X;
      Int128MultiplyWord64(X, Q);
      Int128SubtractInt128(I, X);
      X := I;
      I := U;
      U := Y;
      Int128MultiplyWord64(Y, Q);
      Int128SubtractInt128(J, Y);
      Y := J;
      J := U;
    end;
  X := I;
  Y := J;
  Result := C;
end;

// Helper function for Word64Sqrt: Calculates initial estimate for Word64Sqrt
// Initial estimate is 1 shl (HighestSetBit div 2)
procedure Word64SqrtInitialEstimate(var A: Word64);
var I : Integer;
begin
  I := Word64SetBitScanReverse(A);
  I := I div 2;
  Word64InitOne(A);
  Word64Shl(A, I);
end;

// Calculates the integer square root of A
procedure Word64ISqrt(var A: Word64);
var B, C, D : Word64;
    I       : Byte;
begin
  // Handle special cases
  if Word64CompareWord32(A, 1) <= 0 then // A <= 1
    exit;
  // Divide algorithm based on Netwon's method for f(y,x) = y � x^2
  // xest <- (xest + y/xest) / 2
  C := A;
  Word64SqrtInitialEstimate(C);
  // Iterate until estimate converges or until a maximum number of iterations
  I := 0;
  repeat
    B := C;
    Word64DivideWord64(A, B, C, D);
    Word64AddWord64(C, B);
    Word64Shr1(C);
    Inc(I);
  until Word64EqualsWord64(C, B) or (I = 12);
  // Return result
  Assert(C.LongWords[1] = 0);
  A := C;
end;

function Word64Sqrt(const A: Word64): Extended;
var B, C, D : Extended;
    E       : Word64;
    I       : Byte;
begin
  // Handle special cases
  if Word64CompareWord32(A, 1) <= 0 then // A <= 1
    begin
      Result := Word64ToFloat(A);
      exit;
    end;
  // Divide algorithm based on Netwon's method for f(y,x) = y � x^2
  // xest <- (xest + y/xest) / 2
  E := A;
  Word64SqrtInitialEstimate(E);
  C := Word64ToFloat(E);
  // Iterate until estimate converges or until a maximum number of iterations
  D := Word64ToFloat(A);
  I := 0;
  repeat
    B := C;
    C := (B + D / B) / 2.0;
    Inc(I);
  until (C = B) or (I = 14);
  // Return result
  Result := C;
end;

procedure Word64Power(var A: Word64; const B: LongWord);
var R : Word64;
    C : Word64;
    D : LongWord;
begin
  if Word64IsOne(A) then
    exit;
  C := A;
  D := B;
  Word64InitOne(R);
  while D > 0 do
    if D and 1 = 0 then
      begin
        Word64Sqr(C);
        D := D shr 1;
      end
    else
      begin
        Word64MultiplyWord64InPlace(R, C);
        Dec(D);
      end;
  A := R;
end;

// Word64PowerAndMod
// Calculates A^E mod M
function Word64PowerAndMod(const A, E, M: Word64): Word64;
var P, Y, F : Word64;
    Q, T : Word128;
begin
  Word64InitOne(P);
  Y := A;
  F := E;
  while not Word64IsZero(F) do
    begin
      if Word64IsOdd(F) then
        begin
          Word64MultiplyWord64(P, Y, Q);   // Q := P * Y;
          Word128DivideWord64(Q, M, T, P); // P := Q mod M;
        end;
      Word64MultiplyWord64(Y, Y, Q);       // Q := Y * Y
      Word128DivideWord64(Q, M, T, Y);     // Y := Q mod M;
      Word64Shr1(F);
    end;
  Result := P;
end;

function Word64IsPrime(const A: Word64): Boolean;
var B, C : Word64;
    I, L : Integer;
    Q    : Word64;
    R    : Byte;
const MRTestValues64: array[1..11] of Byte = (
      2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31);
begin
  // Check for values in 32 bit range
  if Word64IsWord32Range(A) then
    begin
      Result := Word32IsPrime(Word64ToWord32(A));
      exit;
    end;
  // Eliminate even numbers
  if not Word64IsOdd(A) then
    begin
      Result := False;
      exit;
    end;
  // Trial division using first few primes (3..251)
  for I := 1 to Word8PrimeCount - 1 do
    begin
      Word64DivideWord8(A, Word8PrimeTable[I], Q, R);
      if R = 0 then
        begin
          Result := False;
          exit;
        end;
    end;
  // check primality using Miller-Rabin
  // if A < 341,550,071,728,321, it is enough to test 2, 3, 5, 7, 11, 13, and 17 (Jaeschke)
  // if A < 56,897,193,526,942,024,370,326,972,321, it is enough to test 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 and 31 (TR Nicely)
  B := A;
  Word64SubtractWord32(B, 1); // B := A - 1;
  if A.Words[3] = 0 then
    L := 7
  else
    L := 11;
  for I := 1 to L do
    begin
      Word64InitWord32(C, MRTestValues64[I]);
      if not Word64IsOne(Word64PowerAndMod(C, B, A)) then
        begin
          Result := False;
          exit;
        end;
    end;
  Result := True;
end;

procedure Word64NextPrime(var A: Word64);
begin
  if Word64IsMaximum(A) then
    begin
      Word64InitZero(A);
      exit;
    end;
  if not Word64IsOdd(A) then
    Word64AddWord32(A, 1)
  else
    Word64AddWord32(A, 2);
  while not Word64IsMaximum(A) do
    begin
      if Word64IsPrime(A) then
        exit;
      Word64AddWord32(A, 2);
    end;
  Word64InitZero(A);
end;

function Word64Hash(const A: Word64): LongWord;
begin
  Result := Word32Hash(A.LongWords[0]) xor
            Word32Hash(A.LongWords[1]);
end;



{                                                                              }
{ Word128                                                                      }
{                                                                              }
{$IFDEF ASM386_DELPHI}
procedure Word128InitZero(var A: Word128);
asm
    xor edx, edx
    mov [eax], edx
    mov [eax + 4], edx
    mov [eax + 8], edx
    mov [eax + 12], edx
end;
{$ELSE}
procedure Word128InitZero(var A: Word128);
begin
  A.LongWords[0] := 0;
  A.LongWords[1] := 0;
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
end;
{$ENDIF}

procedure Word128InitOne(var A: Word128);
begin
  A.LongWords[0] := 1;
  A.LongWords[1] := 0;
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
end;

procedure Word128InitMinimum(var A: Word128);
begin
  A.LongWords[0] := 0;
  A.LongWords[1] := 0;
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
end;

procedure Word128InitMaximum(var A: Word128);
begin
  A.LongWords[0] := $FFFFFFFF;
  A.LongWords[1] := $FFFFFFFF;
  A.LongWords[2] := $FFFFFFFF;
  A.LongWords[3] := $FFFFFFFF;
end;

{$IFDEF ASM386_DELPHI}
function Word128IsZero(const A: Word128): Boolean;
asm
    cmp dword ptr [eax], 0
    jne @NotZero
    cmp dword ptr [eax + 4], 0
    jne @NotZero
    cmp dword ptr [eax + 8], 0
    jne @NotZero
    cmp dword ptr [eax + 12], 0
    jne @NotZero
    mov al, 1
    ret
  @NotZero:
    xor eax, eax
end;
{$ELSE}
function Word128IsZero(const A: Word128): Boolean;
begin
  Result := (A.LongWords[0] = 0) and
            (A.LongWords[1] = 0) and
            (A.LongWords[2] = 0) and
            (A.LongWords[3] = 0);
end;
{$ENDIF}

function Word128IsOne(const A: Word128): Boolean;
begin
  Result := (A.LongWords[0] = 1) and
            (A.LongWords[1] = 0) and
            (A.LongWords[2] = 0) and
            (A.LongWords[3] = 0);
end;

function Word128IsMinimum(const A: Word128): Boolean;
begin
  Result := (A.LongWords[0] = 0) and
            (A.LongWords[1] = 0) and
            (A.LongWords[2] = 0) and
            (A.LongWords[3] = 0);
end;

{$IFDEF ASM386_DELPHI}
function Word128IsMaximum(const A: Word128): Boolean;
asm
    mov edx, -1
    cmp [eax], edx
    jne @NotMaximum
    cmp [eax + 4], edx
    jne @NotMaximum
    cmp [eax + 8], edx
    jne @NotMaximum
    cmp [eax + 12], edx
    jne @NotMaximum
    mov al, 1
    ret
  @NotMaximum:
    xor eax, eax
end;
{$ELSE}
function Word128IsMaximum(const A: Word128): Boolean;
begin
  Result := (A.LongWords[0] = $FFFFFFFF) and
            (A.LongWords[1] = $FFFFFFFF) and
            (A.LongWords[2] = $FFFFFFFF) and
            (A.LongWords[3] = $FFFFFFFF);
end;
{$ENDIF}

function Word128IsOdd(const A: Word128): Boolean;
begin
  Result := (A.LongWords[0] and 1 <> 0);
end;

function Word128IsWord8Range(const A: Word128): Boolean;
begin
  Result := (A.LongWords[0] <= MaxWord8) and
            (A.LongWords[1] = 0) and
            (A.LongWords[2] = 0) and
            (A.LongWords[3] = 0);
end;

function Word128IsWord16Range(const A: Word128): Boolean;
begin
  Result := (A.LongWords[0] <= MaxWord16) and
            (A.LongWords[1] = 0) and
            (A.LongWords[2] = 0) and
            (A.LongWords[3] = 0);
end;

function Word128IsWord32Range(const A: Word128): Boolean;
begin
  Result := (A.LongWords[1] = 0) and
            (A.LongWords[2] = 0) and
            (A.LongWords[3] = 0);
end;

function Word128IsWord64Range(const A: Word128): Boolean;
begin
  Result := (A.LongWords[2] = 0) and
            (A.LongWords[3] = 0);
end;

function Word128IsInt8Range(const A: Word128): Boolean;
begin
  Result := Word128IsInt32Range(A);
  if not Result then
    exit;
  Result := Word32IsInt8Range(Word128ToWord32(A));
end;

function Word128IsInt16Range(const A: Word128): Boolean;
begin
  Result := Word128IsInt32Range(A);
  if not Result then
    exit;
  Result := Word32IsInt16Range(Word128ToWord32(A));
end;

function Word128IsInt32Range(const A: Word128): Boolean;
begin
  Result := (A.LongWords[0] < $80000000) and
            (A.LongWords[1] = 0) and
            (A.LongWords[2] = 0) and
            (A.LongWords[3] = 0);
end;

function Word128IsInt64Range(const A: Word128): Boolean;
begin
  Result := (A.LongWords[1] < $80000000) and
            (A.LongWords[2] = 0) and
            (A.LongWords[3] = 0);
end;

function Word128IsInt128Range(const A: Word128): Boolean;
begin
  Result := (A.LongWords[3] < $80000000);
end;

{$IFDEF ASM386_DELPHI}
procedure Word128InitWord32(var A: Word128; const B: LongWord);
asm
    mov [eax], edx
    xor edx, edx
    mov [eax + 4], edx
    mov [eax + 8], edx
    mov [eax + 12], edx
end;
{$ELSE}
procedure Word128InitWord32(var A: Word128; const B: LongWord);
begin
  A.LongWords[0] := B;
  A.LongWords[1] := 0;
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
end;
{$ENDIF}

procedure Word128InitWord64(var A: Word128; const B: Word64);
begin
  A.Word64s[0] := B;
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
end;

procedure Word128InitInt32(var A: Word128; const B: LongInt);
begin
  {$IFOPT R+}
  if B < 0 then
    RaiseRangeError;
  {$ENDIF}
  A.LongWords[0] := LongWord(B);
  A.LongWords[1] := 0;
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
end;

procedure Word128InitInt64(var A: Word128; const B: Int64);
begin
  {$IFOPT R+}
  if B < 0 then
    RaiseRangeError;
  {$ENDIF}
  A.LongWords[0] := Int64Rec(B).LongWords[0];
  A.LongWords[1] := Int64Rec(B).LongWords[1];
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
end;

const
  MaxWord128F : Extended =
      4294967296.0 * 4294967296.0 * 4294967296.0 * 4294967295.0 +
      4294967296.0 * 4294967296.0 * 4294967295.0 +
      4294967296.0 * 4294967295.0 +
      4294967295.0;

procedure Word128InitFloat(var A: Word128; const B: Extended);
var C : Extended;
    D : Extended;
    E : Int64;
begin
  C := Int(B);
  {$IFOPT R+}
  if (C > MaxWord128F) or (C < 0.0) then
    RaiseRangeError;
  {$ENDIF}
  D := 4294967296.0 * 4294967296.0 * 4294967296.0;
  E := Trunc(C / D);
  A.LongWords[3] := Int64Rec(E).LongWords[0];
  C := C - E * D;
  D := 4294967296.0 * 4294967296.0;
  E := Trunc(C / D);
  A.LongWords[2] := Int64Rec(E).LongWords[0];
  C := C - E * D;
  D := 4294967296.0;
  E := Trunc(C / D);
  A.LongWords[1] := Int64Rec(E).LongWords[0];
  C := C - E * D;
  E := Trunc(C);
  A.LongWords[0] := Int64Rec(E).LongWords[0];
end;

{$IFDEF ASM386_DELPHI}
function Word128ToWord32(const A: Word128): LongWord;
asm
    {$IFOPT R+}
    cmp dword ptr [eax + 4], 0
    jne @RangeError
    cmp dword ptr [eax + 8], 0
    jne @RangeError
    cmp dword ptr [eax + 12], 0
    je @NoRangeError
  @RangeError:
    jmp RaiseRangeError
  @NoRangeError:
    {$ENDIF}
    mov eax, [eax]
end;
{$ELSE}
function Word128ToWord32(const A: Word128): LongWord;
begin
  {$IFOPT R+}
  if (A.LongWords[1] > 0) or
     (A.LongWords[2] > 0) or
     (A.LongWords[3] > 0) then
    RaiseRangeError;
  {$ENDIF}
  Result := A.LongWords[0];
end;
{$ENDIF}

function Word128ToWord64(const A: Word128): Word64;
begin
  {$IFOPT R+}
  if (A.LongWords[2] > 0) or
     (A.LongWords[3] > 0) then
    RaiseRangeError;
  {$ENDIF}
  Result := A.Word64s[0];
end;

function Word128ToInt32(const A: Word128): LongInt;
begin
  {$IFOPT R+}
  if (A.LongWords[0] > $7FFFFFFF) or
     (A.LongWords[1] > 0) or
     (A.LongWords[2] > 0) or
     (A.LongWords[3] > 0) then
    RaiseRangeError;
  {$ENDIF}
  Result := LongInt(A.LongWords[0]);
end;

function Word128ToInt64(const A: Word128): Int64;
begin
  {$IFOPT R+}
  if (A.LongWords[1] > $7FFFFFFF) or
     (A.LongWords[2] > 0) or
     (A.LongWords[3] > 0) then
    RaiseRangeError;
  {$ENDIF}
  Int64Rec(Result).LongWords[0] := A.LongWords[0];
  Int64Rec(Result).LongWords[1] := A.LongWords[1];
end;

function Word128ToFloat(const A: Word128): Extended;
var T : Extended;
begin
  Result := A.LongWords[0];
  T := A.LongWords[1];
  Result := Result + T * 4294967296.0;
  T := A.LongWords[2];
  Result := Result + T * 4294967296.0 * 4294967296.0;
  T := A.LongWords[3];
  Result := Result + T * 4294967296.0 * 4294967296.0 * 4294967296.0;
end;

function Word128Lo(const A: Word128): Word64;
begin
  Result := A.Word64s[0];
end;

function Word128Hi(const A: Word128): Word64;
begin
  Result := A.Word64s[1];
end;

{$IFDEF ASM386_DELPHI}
function Word128EqualsWord32(const A: Word128; const B: LongWord): Boolean;
asm
    cmp [eax], edx
    jne @NotEqual
    cmp dword ptr [eax + 4], 0
    jne @NotEqual
    cmp dword ptr [eax + 8], 0
    jne @NotEqual
    cmp dword ptr [eax + 12], 0
    jne @NotEqual
    mov al, 1
    ret
  @NotEqual:
    xor eax, eax
end;
{$ELSE}
function Word128EqualsWord32(const A: Word128; const B: LongWord): Boolean;
begin
  Result := (A.LongWords[0] = B) and
            (A.LongWords[1] = 0) and
            (A.LongWords[2] = 0) and
            (A.LongWords[3] = 0);
end;
{$ENDIF}

function Word128EqualsWord64(const A: Word128; const B: Word64): Boolean;
begin
  Result := (A.LongWords[0] = B.LongWords[0]) and
            (A.LongWords[1] = B.LongWords[1]) and
            (A.LongWords[2] = 0) and
            (A.LongWords[3] = 0);
end;

function Word128EqualsWord128(const A, B: Word128): Boolean;
begin
  Result := (A.LongWords[0] = B.LongWords[0]) and
            (A.LongWords[1] = B.LongWords[1]) and
            (A.LongWords[2] = B.LongWords[2]) and
            (A.LongWords[3] = B.LongWords[3]);
end;

function Word128EqualsInt32(const A: Word128; const B: LongInt): Boolean;
begin
  if B < 0 then
    Result := False
  else
    Result := (A.LongWords[0] = LongWord(B)) and
              (A.LongWords[1] = 0) and
              (A.LongWords[2] = 0) and
              (A.LongWords[3] = 0);
end;

function Word128EqualsInt64(const A: Word128; const B: Int64): Boolean;
begin
  if B < 0 then
    Result := False
  else
    Result := (A.LongWords[0] = Int64Rec(B).LongWords[0]) and
              (A.LongWords[1] = Int64Rec(B).LongWords[1]) and
              (A.LongWords[2] = 0) and
              (A.LongWords[3] = 0);
end;

function Word128CompareWord32(const A: Word128; const B: LongWord): Integer;
var C : LongWord;
begin
  if (A.LongWords[1] <> 0) or
     (A.LongWords[2] <> 0) or
     (A.LongWords[3] <> 0) then
    Result := 1
  else
    begin
      C := A.LongWords[0];
      if C > B then
        Result := 1 else
      if C < B then
        Result := -1
      else
        Result := 0;
    end;
end;

function Word128CompareWord64(const A: Word128; const B: Word64): Integer;
var C, D : LongWord;
begin
  if (A.LongWords[2] <> 0) or
     (A.LongWords[3] <> 0) then
    Result := 1
  else
    begin
      C := A.LongWords[1];
      D := B.LongWords[1];
      if C = D then
        begin
          C := A.LongWords[0];
          D := B.LongWords[0];
        end;
      if C > D then
        Result := 1 else
      if C < D then
        Result := -1
      else
        Result := 0;
    end;
end;

function Word128CompareWord128(const A, B: Word128): Integer;
var C, D : LongWord;
begin
  C := A.LongWords[3];
  D := B.LongWords[3];
  if C = D then
    begin
      C := A.LongWords[2];
      D := B.LongWords[2];
      if C = D then
        begin
          C := A.LongWords[1];
          D := B.LongWords[1];
          if C = D then
            begin
              C := A.LongWords[0];
              D := B.LongWords[0];
            end;
        end;
    end;
  if C > D then
    Result := 1 else
  if C < D then
    Result := -1
  else
    Result := 0;
end;

function Word128CompareInt32(const A: Word128; const B: LongInt): Integer;
var C : LongWord;
begin
  if B < 0 then
    Result := 1 else
  if (A.LongWords[1] <> 0) or
     (A.LongWords[2] <> 0) or
     (A.LongWords[3] <> 0) then
    Result := 1
  else
    begin
      C := A.LongWords[0];
      if C > LongWord(B) then
        Result := 1 else
      if C < LongWord(B) then
        Result := -1
      else
        Result := 0;
    end;
end;

function Word128CompareInt64(const A: Word128; const B: Int64): Integer;
var C, D : LongWord;
begin
  if B < 0 then
    Result := 1 else
  if (A.LongWords[2] <> 0) or
     (A.LongWords[3] <> 0) then
    Result := 1
  else
    begin
      C := A.LongWords[1];
      D := Int64Rec(B).LongWords[1];
      if C = D then
        begin
          C := A.LongWords[0];
          D := Int64Rec(B).LongWords[0];
        end;
      if C > D then
        Result := 1 else
      if C < D then
        Result := -1
      else
        Result := 0;
    end;
end;

function Word128Min(const A, B: Word128): Word128;
begin
  if Word128CompareWord128(A, B) <= 0 then
    Result := A
  else
    Result := B;
end;

function Word128Max(const A, B: Word128): Word128;
begin
  if Word128CompareWord128(A, B) >= 0 then
    Result := A
  else
    Result := B;
end;

procedure Word128Not(var A: Word128);
begin
  A.LongWords[0] := not A.LongWords[0];
  A.LongWords[1] := not A.LongWords[1];
  A.LongWords[2] := not A.LongWords[2];
  A.LongWords[3] := not A.LongWords[3];
end;

procedure Word128OrWord128(var A: Word128; const B: Word128);
begin
  A.LongWords[0] := A.LongWords[0] or B.LongWords[0];
  A.LongWords[1] := A.LongWords[1] or B.LongWords[1];
  A.LongWords[2] := A.LongWords[2] or B.LongWords[2];
  A.LongWords[3] := A.LongWords[3] or B.LongWords[3];
end;

procedure Word128AndWord128(var A: Word128; const B: Word128);
begin
  A.LongWords[0] := A.LongWords[0] and B.LongWords[0];
  A.LongWords[1] := A.LongWords[1] and B.LongWords[1];
  A.LongWords[2] := A.LongWords[2] and B.LongWords[2];
  A.LongWords[3] := A.LongWords[3] and B.LongWords[3];
end;

procedure Word128XorWord128(var A: Word128; const B: Word128);
begin
  A.LongWords[0] := A.LongWords[0] xor B.LongWords[0];
  A.LongWords[1] := A.LongWords[1] xor B.LongWords[1];
  A.LongWords[2] := A.LongWords[2] xor B.LongWords[2];
  A.LongWords[3] := A.LongWords[3] xor B.LongWords[3];
end;

function Word128IsBitSet(const A: Word128; const B: Integer): Boolean;
begin
  if (B < 0) or (B > 127) then
    Result := False
  else
    Result := (A.LongWords[B shr 5] and (1 shl (B and $1F)) <> 0);
end;

procedure Word128SetBit(var A: Word128; const B: Integer);
begin
  if (B < 0) or (B > 127) then
    exit;
  A.LongWords[B shr 5] := A.LongWords[B shr 5] or (1 shl (B and $1F));
end;

procedure Word128ClearBit(var A: Word128; const B: Integer);
begin
  if (B < 0) or (B > 127) then
    exit;
  A.LongWords[B shr 5] := A.LongWords[B shr 5] and not (1 shl (B and $1F));
end;

procedure Word128ToggleBit(var A: Word128; const B: Integer);
begin
  if (B < 0) or (B > 127) then
    exit;
  A.LongWords[B shr 5] := A.LongWords[B shr 5] xor (1 shl (B and $1F));
end;

// Returns index (0-127) of lowest set bit in A, or -1 if none set
function Word128SetBitScanForward(const A: Word128): Integer;
var B : Integer;
begin
  for B := 0 to 127 do
    if A.LongWords[B shr 5] and (1 shl (B and $1F)) <> 0 then
      begin
        Result := B;
        exit;
      end;
  Result := -1;
end;

// Returns index (0-127) of highest set bit in A, or -1 if none set
function Word128SetBitScanReverse(const A: Word128): Integer;
var B : Integer;
begin
  for B := 127 downto 0 do
    if A.LongWords[B shr 5] and (1 shl (B and $1F)) <> 0 then
      begin
        Result := B;
        exit;
      end;
  Result := -1;
end;

// Returns index (0-127) of lowest clear bit in A, or -1 if none clear
function Word128ClearBitScanForward(const A: Word128): Integer;
var B : Integer;
begin
  for B := 0 to 127 do
    if A.LongWords[B shr 5] and (1 shl (B and $1F)) = 0 then
      begin
        Result := B;
        exit;
      end;
  Result := -1;
end;

// Returns index (0-127) of highest clear bit in A, or -1 if none clear
function Word128ClearBitScanReverse(const A: Word128): Integer;
var B : Integer;
begin
  for B := 127 downto 0 do
    if A.LongWords[B shr 5] and (1 shl (B and $1F)) = 0 then
      begin
        Result := B;
        exit;
      end;
  Result := -1;
end;

procedure Word128Shl(var A: Word128; const B: Byte);
var C, D : Byte;
begin
  if B = 0 then
    exit;
  if B >= 128 then
    Word128InitZero(A) else
  if B < 32 then // 1 <= B <= 31
    begin
      C := 32 - B;
      A.LongWords[3] := (A.LongWords[3] shl B) or (A.LongWords[2] shr C);
      A.LongWords[2] := (A.LongWords[2] shl B) or (A.LongWords[1] shr C);
      A.LongWords[1] := (A.LongWords[1] shl B) or (A.LongWords[0] shr C);
      A.LongWords[0] := (A.LongWords[0] shl B);
    end else
  if B < 64 then // 32 <= B <= 63
    begin
      D := B - 32;
      C := 32 - D;
      A.LongWords[3] := (A.LongWords[2] shl D) or (A.LongWords[1] shr C);
      A.LongWords[2] := (A.LongWords[1] shl D) or (A.LongWords[0] shr C);
      A.LongWords[1] := (A.LongWords[0] shl D);
      A.LongWords[0] := 0;
    end else
  if B < 96 then // 64 <= B <= 95
    begin
      D := B - 64;
      C := 32 - D;
      A.LongWords[3] := (A.LongWords[1] shl D) or (A.LongWords[0] shr C);
      A.LongWords[2] := (A.LongWords[0] shl D);
      A.LongWords[1] := 0;
      A.LongWords[0] := 0;
    end
  else           // 96 <= B <= 127
    begin
      D := B - 96;
      A.LongWords[3] := (A.LongWords[0] shl D);
      A.LongWords[2] := 0;
      A.LongWords[1] := 0;
      A.LongWords[0] := 0;
    end;
end;

procedure Word128Shl1(var A: Word128);
begin
  A.LongWords[3] := (A.LongWords[3] shl 1) or (A.LongWords[2] shr 31);
  A.LongWords[2] := (A.LongWords[2] shl 1) or (A.LongWords[1] shr 31);
  A.LongWords[1] := (A.LongWords[1] shl 1) or (A.LongWords[0] shr 31);
  A.LongWords[0] := (A.LongWords[0] shl 1);
end;

procedure Word128Shr(var A: Word128; const B: Byte);
var C, D : Byte;
begin
  if B = 0 then
    exit;
  if B >= 128 then
    Word128InitZero(A) else
  if B < 32 then // 1 <= B <= 31
    begin
      C := 32 - B;
      A.LongWords[0] := (A.LongWords[0] shr B) or (A.LongWords[1] shl C);
      A.LongWords[1] := (A.LongWords[1] shr B) or (A.LongWords[2] shl C);
      A.LongWords[2] := (A.LongWords[2] shr B) or (A.LongWords[3] shl C);
      A.LongWords[3] := (A.LongWords[3] shr B);
    end else
  if B < 64 then // 32 <= B <= 63
    begin
      D := B - 32;
      C := 32 - D;
      A.LongWords[0] := (A.LongWords[1] shr D) or (A.LongWords[2] shl C);
      A.LongWords[1] := (A.LongWords[2] shr D) or (A.LongWords[3] shl C);
      A.LongWords[2] := (A.LongWords[3] shr D);
      A.LongWords[3] := 0;
    end else
  if B < 96 then // 64 <= B <= 95
    begin
      D := B - 64;
      C := 32 - D;
      A.LongWords[0] := (A.LongWords[2] shr D) or (A.LongWords[3] shl C);
      A.LongWords[1] := (A.LongWords[3] shr D);
      A.LongWords[2] := 0;
      A.LongWords[3] := 0;
    end
  else           // 96 <= B <= 127
    begin
      D := B - 96;
      A.LongWords[0] := (A.LongWords[3] shr D);
      A.LongWords[1] := 0;
      A.LongWords[2] := 0;
      A.LongWords[3] := 0;
    end;
end;

procedure Word128Shr1(var A: Word128);
begin
  A.LongWords[0] := (A.LongWords[0] shr 1) or (A.LongWords[1] shl 31);
  A.LongWords[1] := (A.LongWords[1] shr 1) or (A.LongWords[2] shl 31);
  A.LongWords[2] := (A.LongWords[2] shr 1) or (A.LongWords[3] shl 31);
  A.LongWords[3] := (A.LongWords[3] shr 1);
end;

procedure Word128Rol(var A: Word128; const B: Byte);
var C, D : Byte;
    E    : Word128;
begin
  C := B mod 128;
  if C = 0 then
    exit;
  if C < 32 then
    begin
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[3] shl C) or (A.LongWords[2] shr D);
      E.LongWords[2] := (A.LongWords[2] shl C) or (A.LongWords[1] shr D);
      E.LongWords[1] := (A.LongWords[1] shl C) or (A.LongWords[0] shr D);
      E.LongWords[0] := (A.LongWords[0] shl C) or (A.LongWords[3] shr D);
    end else
  if C < 64 then
    begin
      Dec(C, 32);
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[2] shl C) or (A.LongWords[1] shr D);
      E.LongWords[2] := (A.LongWords[1] shl C) or (A.LongWords[0] shr D);
      E.LongWords[1] := (A.LongWords[0] shl C) or (A.LongWords[3] shr D);
      E.LongWords[0] := (A.LongWords[3] shl C) or (A.LongWords[2] shr D);
    end else
  if C < 96 then
    begin
      Dec(C, 64);
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[1] shl C) or (A.LongWords[0] shr D);
      E.LongWords[2] := (A.LongWords[0] shl C) or (A.LongWords[3] shr D);
      E.LongWords[1] := (A.LongWords[3] shl C) or (A.LongWords[2] shr D);
      E.LongWords[0] := (A.LongWords[2] shl C) or (A.LongWords[1] shr D);
    end
  else
    begin
      Dec(C, 96);
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[0] shl C) or (A.LongWords[3] shr D);
      E.LongWords[2] := (A.LongWords[3] shl C) or (A.LongWords[2] shr D);
      E.LongWords[1] := (A.LongWords[2] shl C) or (A.LongWords[1] shr D);
      E.LongWords[0] := (A.LongWords[1] shl C) or (A.LongWords[0] shr D);
    end;
  A := E;
end;

procedure Word128Rol1(var A: Word128);
var B : Word128;
begin
  B.LongWords[3] := (A.LongWords[3] shl 1) or (A.LongWords[2] shr 31);
  B.LongWords[2] := (A.LongWords[2] shl 1) or (A.LongWords[1] shr 31);
  B.LongWords[1] := (A.LongWords[1] shl 1) or (A.LongWords[0] shr 31);
  B.LongWords[0] := (A.LongWords[0] shl 1) or (A.LongWords[3] shr 31);
  A := B;
end;

procedure Word128Ror(var A: Word128; const B: Byte);
var C, D : Byte;
    E    : Word128;
begin
  C := B mod 128;
  if C = 0 then
    exit;
  if C < 32 then
    begin
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[3] shr C) or (A.LongWords[0] shl D);
      E.LongWords[2] := (A.LongWords[2] shr C) or (A.LongWords[3] shl D);
      E.LongWords[1] := (A.LongWords[1] shr C) or (A.LongWords[2] shl D);
      E.LongWords[0] := (A.LongWords[0] shr C) or (A.LongWords[1] shl D);
    end else
  if C < 64 then
    begin
      Dec(C, 32);
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[2] shr C) or (A.LongWords[3] shl D);
      E.LongWords[2] := (A.LongWords[1] shr C) or (A.LongWords[2] shl D);
      E.LongWords[1] := (A.LongWords[0] shr C) or (A.LongWords[1] shl D);
      E.LongWords[0] := (A.LongWords[3] shr C) or (A.LongWords[0] shl D);
    end else
  if C < 96 then
    begin
      Dec(C, 64);
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[1] shr C) or (A.LongWords[2] shl D);
      E.LongWords[2] := (A.LongWords[0] shr C) or (A.LongWords[1] shl D);
      E.LongWords[1] := (A.LongWords[3] shr C) or (A.LongWords[0] shl D);
      E.LongWords[0] := (A.LongWords[2] shr C) or (A.LongWords[3] shl D);
    end
  else
    begin
      Dec(C, 96);
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[0] shr C) or (A.LongWords[1] shl D);
      E.LongWords[2] := (A.LongWords[3] shr C) or (A.LongWords[0] shl D);
      E.LongWords[1] := (A.LongWords[2] shr C) or (A.LongWords[3] shl D);
      E.LongWords[0] := (A.LongWords[1] shr C) or (A.LongWords[2] shl D);
    end;
  A := E;
end;

procedure Word128Ror1(var A: Word128);
var B : Word128;
begin
  B.LongWords[3] := (A.LongWords[3] shr 1) or (A.LongWords[0] shl 31);
  B.LongWords[2] := (A.LongWords[2] shr 1) or (A.LongWords[3] shl 31);
  B.LongWords[1] := (A.LongWords[1] shr 1) or (A.LongWords[2] shl 31);
  B.LongWords[0] := (A.LongWords[0] shr 1) or (A.LongWords[1] shl 31);
  A := B;
end;

procedure Word128Swap(var A, B: Word128);
var D : LongWord;
begin
  D := A.LongWords[0];
  A.LongWords[0] := B.LongWords[0];
  B.LongWords[0] := D;
  D := A.LongWords[1];
  A.LongWords[1] := B.LongWords[1];
  B.LongWords[1] := D;
  D := A.LongWords[2];
  A.LongWords[2] := B.LongWords[2];
  B.LongWords[2] := D;
  D := A.LongWords[3];
  A.LongWords[3] := B.LongWords[3];
  B.LongWords[3] := D;
end;

procedure Word128ReverseBits(var A: Word128);
var B : Word128;
    I : Integer;
begin
  Word128InitZero(B);
  for I := 0 to 127 do
    if Word128IsBitSet(A, I) then
      Word128SetBit(B, 127 - I);
  A := B;
end;

procedure Word128ReverseBytes(var A: Word128);
var I : Integer;
    B : Byte;
begin
  for I := 0 to 15 do
    begin
      B := A.Bytes[I];
      A.Bytes[I] := A.Bytes[15 - I];
      A.Bytes[15 - I] := B;
    end;
end;

function Word128BitCount(const A: Word128): Integer;
var I : Integer;
begin
  Result := 0;
  for I := 0 to 15 do
    Inc(Result, Word8BitCountTable[A.Bytes[I]]);
end;

function Word128IsPowerOfTwo(const A: Word128): Boolean;
begin
  Result := Word128BitCount(A) = 1;
end;

procedure Word128Inc(var A: Word128);
var C : LongWord;
    D : Integer;
begin
  C := A.Words[0];
  Inc(C);
  A.Words[0] := Word(C and $FFFF);

  C := C shr 16;
  if C = 0 then exit;
  Inc(C, A.Words[1]);
  A.Words[1] := Word(C and $FFFF);

  for D := 2 to 7 do
    begin
      C := C shr 16;
      if C = 0 then exit;
      Inc(C, A.Words[D]);
      A.Words[D] := Word(C and $FFFF);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

{$IFDEF ASM386_DELPHI}
procedure Word128AddWord8(var A: Word128; const B: Byte);
asm
    movzx edx, dl
    add [eax], edx
    jnc @Fin
    adc [eax + 4], 0
    adc [eax + 8], 0
    adc [eax + 12], 0
    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
    {$ENDIF}
  @Fin:
end;
{$ELSE}
procedure Word128AddWord8(var A: Word128; const B: Byte);
var C : LongWord;
    D : Integer;
begin
  C := A.Words[0];
  Inc(C, B);
  A.Words[0] := Word(C and $FFFF);

  C := C shr 16;
  if C = 0 then exit;
  Inc(C, A.Words[1]);
  A.Words[1] := Word(C and $FFFF);

  for D := 2 to 7 do
    begin
      C := C shr 16;
      if C = 0 then exit;
      Inc(C, A.Words[D]);
      A.Words[D] := Word(C and $FFFF);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure Word128AddWord16(var A: Word128; const B: Word);
asm
    movzx edx, dx
    add [eax], edx
    jnc @Fin
    adc [eax + 4], 0
    adc [eax + 8], 0
    adc [eax + 12], 0
    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
    {$ENDIF}
  @Fin:
end;
{$ELSE}
procedure Word128AddWord16(var A: Word128; const B: Word);
var C : LongWord;
    D : Integer;
begin
  C := A.Words[0];
  Inc(C, B);
  A.Words[0] := Word(C and $FFFF);

  C := C shr 16;
  if C = 0 then exit;
  Inc(C, A.Words[1]);
  A.Words[1] := Word(C and $FFFF);

  for D := 2 to 7 do
    begin
      C := C shr 16;
      if C = 0 then exit;
      Inc(C, A.Words[D]);
      A.Words[D] := Word(C and $FFFF);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure Word128AddWord32(var A: Word128; const B: LongWord);
asm
    add [eax], edx
    jnc @Fin
    adc [eax + 4], 0
    adc [eax + 8], 0
    adc [eax + 12], 0
    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
    {$ENDIF}
  @Fin:
end;
{$ELSE}
procedure Word128AddWord32(var A: Word128; const B: LongWord);
var C : LongWord;
    D : Integer;
begin
  if B = 0 then
    exit;

  C := A.Words[0];
  Inc(C, B and $FFFF);
  A.Words[0] := Word(C and $FFFF);

  C := C shr 16;
  Inc(C, A.Words[1]);
  Inc(C, B shr 16);
  A.Words[1] := Word(C and $FFFF);

  for D := 2 to 7 do
    begin
      C := C shr 16;
      if C = 0 then exit;
      Inc(C, A.Words[D]);
      A.Words[D] := Word(C and $FFFF);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure Word128AddWord64(var A: Word128; const B: Word64);
asm
    mov ecx, [eax]
    add ecx, [edx]
    mov [eax], ecx

    mov ecx, [eax + 4]
    adc ecx, [edx + 4]
    mov [eax + 4], ecx
    jnc @Fin

    adc dword ptr [eax + 8], 0
    adc dword ptr [eax + 12], 0

    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
    {$ENDIF}
  @Fin:
end;
{$ELSE}
procedure Word128AddWord64(var A: Word128; const B: Word64);
var C : LongWord;
    D : Integer;
begin
  C := LongWord(A.Words[0]) + B.Words[0];
  A.Words[0] := Word(C and $FFFF);

  for D := 1 to 3 do
    begin
      C := C shr 16;
      Inc(C, A.Words[D]);
      Inc(C, B.Words[D]);
      A.Words[D] := Word(C and $FFFF);
    end;

  for D := 4 to 7 do
    begin
      C := C shr 16;
      if C = 0 then exit;
      Inc(C, A.Words[D]);
      A.Words[D] := Word(C and $FFFF);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure Word128AddWord128(var A: Word128; const B: Word128);
asm
    mov ecx, [eax]
    add ecx, [edx]
    mov [eax], ecx

    mov ecx, [eax + 4]
    adc ecx, [edx + 4]
    mov [eax + 4], ecx

    mov ecx, [eax + 8]
    adc ecx, [edx + 8]
    mov [eax + 8], ecx

    mov ecx, [eax + 12]
    adc ecx, [edx + 12]
    mov [eax + 12], ecx

    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
    {$ENDIF}
  @Fin:
end;
{$ELSE}
procedure Word128AddWord128(var A: Word128; const B: Word128);
var C : LongWord;
    D : Integer;
begin
  C := LongWord(A.Words[0]) + B.Words[0];
  A.Words[0] := Word(C and $FFFF);

  for D := 1 to 7 do
    begin
      C := C shr 16;
      Inc(C, A.Words[D]);
      Inc(C, B.Words[D]);
      A.Words[D] := Word(C and $FFFF);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;
{$ENDIF}

procedure Word128SubtractWord32(var A: Word128; const B: LongWord);
var C, D : Integer;
begin
  C := A.Words[0];
  Dec(C, B and $FFFF);
  A.Words[0] := Word(C);

  if C < 0 then C := -1 else C := 0;
  Inc(C, A.Words[1]);
  Dec(C, B shr 16);
  A.Words[1] := Word(C);

  if C < 0 then C := -1 else exit;
  Inc(C, A.Words[2]);
  A.Words[2] := Word(C);

  for D := 3 to 7 do
    begin
      if C >= 0 then exit;
      Inc(C, A.Words[D]);
      A.Words[D] := Word(C);
    end;

  {$IFOPT Q+}
  if C < 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word128SubtractWord64(var A: Word128; const B: Word64);
var C, D : Integer;
begin
  C := A.Words[0];
  Dec(C, B.Words[0]);
  A.Words[0] := Word(C);

  for D := 1 to 3 do
    begin
      if C < 0 then C := -1 else C := 0;
      Inc(C, A.Words[D]);
      Dec(C, B.Words[D]);
      A.Words[D] := Word(C);
    end;

  if C < 0 then C := -1 else exit;
  Inc(C, A.Words[4]);
  A.Words[4] := Word(C);

  for D := 5 to 7 do
    begin
      if C >= 0 then exit;
      Inc(C, A.Words[D]);
      A.Words[D] := Word(C);
    end;

  {$IFOPT Q+}
  if C < 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word128SubtractWord128(var A: Word128; const B: Word128);
var C, D : Integer;
begin
  C := A.Words[0];
  Dec(C, B.Words[0]);
  A.Words[0] := Word(C);

  for D := 1 to 7 do
    begin
      if C < 0 then C := -1 else C := 0;
      Inc(C, A.Words[D]);
      Dec(C, B.Words[D]);
      A.Words[D] := Word(C);
    end;

  {$IFOPT Q+}
  if C < 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

{$IFDEF ASM386_DELPHI}
procedure Word128MultiplyWord8(var A: Word128; const B: Byte);
begin
  Word128MultiplyWord32(A, B);
end;
{$ELSE}
procedure Word128MultiplyWord8(var A: Word128; const B: Byte);
var C : LongWord;
    D : Integer;
begin
  C := LongWord(A.Words[0]) * B;
  A.Words[0] := Word(C);

  for D := 1 to 7 do
    begin
      C := C shr 16;
      Inc(C, LongWord(A.Words[D]) * B);
      A.Words[D] := Word(C);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure Word128MultiplyWord16(var A: Word128; const B: Word);
begin
  Word128MultiplyWord32(A, B);
end;
{$ELSE}
procedure Word128MultiplyWord16(var A: Word128; const B: Word);
var C : LongWord;
    D : Integer;
begin
  C := LongWord(A.Words[0]) * B;
  A.Words[0] := Word(C);

  for D := 1 to 7 do
    begin
      C := C shr 16;
      Inc(C, LongWord(A.Words[D]) * B);
      A.Words[D] := Word(C);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure Word128MultiplyWord32(var A: Word128; const B: LongWord);
asm
    // Init
    push esi
    push edi
    mov esi, eax
    mov ecx, edx
    // A[0]
    mov eax, [esi]
    mul ecx
    mov [esi], eax
    mov edi, edx
    // A[1]
    mov eax, [esi + 4]
    mul ecx
    add eax, edi
    adc edx, 0
    mov [esi + 4], eax
    mov edi, edx
    // A[2]
    mov eax, [esi + 8]
    mul ecx
    add eax, edi
    adc edx, 0
    mov [esi + 8], eax
    mov edi, edx
    // A[3]
    mov eax, [esi + 12]
    mul ecx
    add eax, edi
    adc edx, 0
    mov [esi + 12], eax
    // Fin
    {$IFOPT Q+}
    or edx, edx
    jz @Fin
    jmp RaiseOverflowError
  @Fin:
    {$ENDIF}
    pop edi
    pop esi
end;
{$ELSE}
procedure Word128MultiplyWord32(var A: Word128; const B: LongWord);
var R    : Word128;
    I, J : Word;
    C    : Int64;
    D    : Integer;
begin
  I := Word(B and $FFFF);
  J := Word(B shr 16);

  C := LongWord(A.Words[0]) * I;
  R.Words[0] := Word(C);

  for D := 1 to 7 do
    begin
      C := C shr 16;
      Inc(C, LongWord(A.Words[D - 1]) * J);
      Inc(C, LongWord(A.Words[D]) * I);
      R.Words[D] := Word(C);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  Inc(C, LongWord(A.Words[7]) * J);
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}

  A := R;
end;
{$ENDIF}

procedure Word128MultiplyWord64(var A: Word128; const B: Word64);
var R : Word128;
    C : Int64;
begin
  C := LongWord(A.Words[0]) * B.Words[0];
  R.Words[0] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[1]);
  Inc(C, LongWord(A.Words[1]) * B.Words[0]);
  R.Words[1] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[2]);
  Inc(C, LongWord(A.Words[1]) * B.Words[1]);
  Inc(C, LongWord(A.Words[2]) * B.Words[0]);
  R.Words[2] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[3]);
  Inc(C, LongWord(A.Words[1]) * B.Words[2]);
  Inc(C, LongWord(A.Words[2]) * B.Words[1]);
  Inc(C, LongWord(A.Words[3]) * B.Words[0]);
  R.Words[3] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[1]) * B.Words[3]);
  Inc(C, LongWord(A.Words[2]) * B.Words[2]);
  Inc(C, LongWord(A.Words[3]) * B.Words[1]);
  Inc(C, LongWord(A.Words[4]) * B.Words[0]);
  R.Words[4] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[2]) * B.Words[3]);
  Inc(C, LongWord(A.Words[3]) * B.Words[2]);
  Inc(C, LongWord(A.Words[4]) * B.Words[1]);
  Inc(C, LongWord(A.Words[5]) * B.Words[0]);
  R.Words[5] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[3]) * B.Words[3]);
  Inc(C, LongWord(A.Words[4]) * B.Words[2]);
  Inc(C, LongWord(A.Words[5]) * B.Words[1]);
  Inc(C, LongWord(A.Words[6]) * B.Words[0]);
  R.Words[6] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[4]) * B.Words[3]);
  Inc(C, LongWord(A.Words[5]) * B.Words[2]);
  Inc(C, LongWord(A.Words[6]) * B.Words[1]);
  Inc(C, LongWord(A.Words[7]) * B.Words[0]);
  R.Words[7] := Word(C);

  {$IFOPT Q+}
  C := C shr 16;
  Inc(C, LongWord(A.Words[5]) * B.Words[3]);
  Inc(C, LongWord(A.Words[6]) * B.Words[2]);
  Inc(C, LongWord(A.Words[7]) * B.Words[1]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[6]) * B.Words[3]);
  Inc(C, LongWord(A.Words[7]) * B.Words[2]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[7]) * B.Words[3]);
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}

  A := R;
end;

procedure Word128MultiplyWord128Low(var A: Word128; const B: Word128);
var R : Word128;
    C : Int64;
begin
  C := LongWord(A.Words[0]) * B.Words[0];
  R.Words[0] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[1]);
  Inc(C, LongWord(A.Words[1]) * B.Words[0]);
  R.Words[1] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[2]);
  Inc(C, LongWord(A.Words[1]) * B.Words[1]);
  Inc(C, LongWord(A.Words[2]) * B.Words[0]);
  R.Words[2] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[3]);
  Inc(C, LongWord(A.Words[1]) * B.Words[2]);
  Inc(C, LongWord(A.Words[2]) * B.Words[1]);
  Inc(C, LongWord(A.Words[3]) * B.Words[0]);
  R.Words[3] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[4]);
  Inc(C, LongWord(A.Words[1]) * B.Words[3]);
  Inc(C, LongWord(A.Words[2]) * B.Words[2]);
  Inc(C, LongWord(A.Words[3]) * B.Words[1]);
  Inc(C, LongWord(A.Words[4]) * B.Words[0]);
  R.Words[4] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[5]);
  Inc(C, LongWord(A.Words[1]) * B.Words[4]);
  Inc(C, LongWord(A.Words[2]) * B.Words[3]);
  Inc(C, LongWord(A.Words[3]) * B.Words[2]);
  Inc(C, LongWord(A.Words[4]) * B.Words[1]);
  Inc(C, LongWord(A.Words[5]) * B.Words[0]);
  R.Words[5] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[6]);
  Inc(C, LongWord(A.Words[1]) * B.Words[5]);
  Inc(C, LongWord(A.Words[2]) * B.Words[4]);
  Inc(C, LongWord(A.Words[3]) * B.Words[3]);
  Inc(C, LongWord(A.Words[4]) * B.Words[2]);
  Inc(C, LongWord(A.Words[5]) * B.Words[1]);
  Inc(C, LongWord(A.Words[6]) * B.Words[0]);
  R.Words[6] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[7]);
  Inc(C, LongWord(A.Words[1]) * B.Words[6]);
  Inc(C, LongWord(A.Words[2]) * B.Words[5]);
  Inc(C, LongWord(A.Words[3]) * B.Words[4]);
  Inc(C, LongWord(A.Words[4]) * B.Words[3]);
  Inc(C, LongWord(A.Words[5]) * B.Words[2]);
  Inc(C, LongWord(A.Words[6]) * B.Words[1]);
  Inc(C, LongWord(A.Words[7]) * B.Words[0]);
  R.Words[7] := Word(C);

  {$IFOPT Q+}
  C := C shr 16;
  Inc(C, LongWord(A.Words[1]) * B.Words[7]);
  Inc(C, LongWord(A.Words[2]) * B.Words[6]);
  Inc(C, LongWord(A.Words[3]) * B.Words[5]);
  Inc(C, LongWord(A.Words[4]) * B.Words[4]);
  Inc(C, LongWord(A.Words[5]) * B.Words[3]);
  Inc(C, LongWord(A.Words[6]) * B.Words[2]);
  Inc(C, LongWord(A.Words[7]) * B.Words[1]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[2]) * B.Words[7]);
  Inc(C, LongWord(A.Words[3]) * B.Words[6]);
  Inc(C, LongWord(A.Words[4]) * B.Words[5]);
  Inc(C, LongWord(A.Words[5]) * B.Words[4]);
  Inc(C, LongWord(A.Words[6]) * B.Words[3]);
  Inc(C, LongWord(A.Words[7]) * B.Words[2]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[3]) * B.Words[7]);
  Inc(C, LongWord(A.Words[4]) * B.Words[6]);
  Inc(C, LongWord(A.Words[5]) * B.Words[5]);
  Inc(C, LongWord(A.Words[6]) * B.Words[4]);
  Inc(C, LongWord(A.Words[7]) * B.Words[3]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[4]) * B.Words[7]);
  Inc(C, LongWord(A.Words[5]) * B.Words[6]);
  Inc(C, LongWord(A.Words[6]) * B.Words[5]);
  Inc(C, LongWord(A.Words[7]) * B.Words[4]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[5]) * B.Words[7]);
  Inc(C, LongWord(A.Words[6]) * B.Words[6]);
  Inc(C, LongWord(A.Words[7]) * B.Words[5]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[6]) * B.Words[7]);
  Inc(C, LongWord(A.Words[7]) * B.Words[6]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[7]) * B.Words[7]);
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}

  A := R;
end;

procedure Word128MultiplyWord128(const A, B: Word128; var R: Word256);
var C : Int64;
begin
  C := LongWord(A.Words[0]) * B.Words[0];
  R.Words[0] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[1]);
  Inc(C, LongWord(A.Words[1]) * B.Words[0]);
  R.Words[1] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[2]);
  Inc(C, LongWord(A.Words[1]) * B.Words[1]);
  Inc(C, LongWord(A.Words[2]) * B.Words[0]);
  R.Words[2] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[3]);
  Inc(C, LongWord(A.Words[1]) * B.Words[2]);
  Inc(C, LongWord(A.Words[2]) * B.Words[1]);
  Inc(C, LongWord(A.Words[3]) * B.Words[0]);
  R.Words[3] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[4]);
  Inc(C, LongWord(A.Words[1]) * B.Words[3]);
  Inc(C, LongWord(A.Words[2]) * B.Words[2]);
  Inc(C, LongWord(A.Words[3]) * B.Words[1]);
  Inc(C, LongWord(A.Words[4]) * B.Words[0]);
  R.Words[4] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[5]);
  Inc(C, LongWord(A.Words[1]) * B.Words[4]);
  Inc(C, LongWord(A.Words[2]) * B.Words[3]);
  Inc(C, LongWord(A.Words[3]) * B.Words[2]);
  Inc(C, LongWord(A.Words[4]) * B.Words[1]);
  Inc(C, LongWord(A.Words[5]) * B.Words[0]);
  R.Words[5] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[6]);
  Inc(C, LongWord(A.Words[1]) * B.Words[5]);
  Inc(C, LongWord(A.Words[2]) * B.Words[4]);
  Inc(C, LongWord(A.Words[3]) * B.Words[3]);
  Inc(C, LongWord(A.Words[4]) * B.Words[2]);
  Inc(C, LongWord(A.Words[5]) * B.Words[1]);
  Inc(C, LongWord(A.Words[6]) * B.Words[0]);
  R.Words[6] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[7]);
  Inc(C, LongWord(A.Words[1]) * B.Words[6]);
  Inc(C, LongWord(A.Words[2]) * B.Words[5]);
  Inc(C, LongWord(A.Words[3]) * B.Words[4]);
  Inc(C, LongWord(A.Words[4]) * B.Words[3]);
  Inc(C, LongWord(A.Words[5]) * B.Words[2]);
  Inc(C, LongWord(A.Words[6]) * B.Words[1]);
  Inc(C, LongWord(A.Words[7]) * B.Words[0]);
  R.Words[7] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[1]) * B.Words[7]);
  Inc(C, LongWord(A.Words[2]) * B.Words[6]);
  Inc(C, LongWord(A.Words[3]) * B.Words[5]);
  Inc(C, LongWord(A.Words[4]) * B.Words[4]);
  Inc(C, LongWord(A.Words[5]) * B.Words[3]);
  Inc(C, LongWord(A.Words[6]) * B.Words[2]);
  Inc(C, LongWord(A.Words[7]) * B.Words[1]);
  R.Words[8] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[2]) * B.Words[7]);
  Inc(C, LongWord(A.Words[3]) * B.Words[6]);
  Inc(C, LongWord(A.Words[4]) * B.Words[5]);
  Inc(C, LongWord(A.Words[5]) * B.Words[4]);
  Inc(C, LongWord(A.Words[6]) * B.Words[3]);
  Inc(C, LongWord(A.Words[7]) * B.Words[2]);
  R.Words[9] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[3]) * B.Words[7]);
  Inc(C, LongWord(A.Words[4]) * B.Words[6]);
  Inc(C, LongWord(A.Words[5]) * B.Words[5]);
  Inc(C, LongWord(A.Words[6]) * B.Words[4]);
  Inc(C, LongWord(A.Words[7]) * B.Words[3]);
  R.Words[10] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[4]) * B.Words[7]);
  Inc(C, LongWord(A.Words[5]) * B.Words[6]);
  Inc(C, LongWord(A.Words[6]) * B.Words[5]);
  Inc(C, LongWord(A.Words[7]) * B.Words[4]);
  R.Words[11] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[5]) * B.Words[7]);
  Inc(C, LongWord(A.Words[6]) * B.Words[6]);
  Inc(C, LongWord(A.Words[7]) * B.Words[5]);
  R.Words[12] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[6]) * B.Words[7]);
  Inc(C, LongWord(A.Words[7]) * B.Words[6]);
  R.Words[13] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[7]) * B.Words[7]);
  R.Words[14] := Word(C);

  C := C shr 16;
  R.Words[15] := Word(C);
end;

procedure Word128Sqr(var A: Word128);
begin
  Word128MultiplyWord128Low(A, A);
end;

procedure Word128DivideWord8(const A: Word128; const B: Byte; var Q: Word128; var R: Byte);
var C, D, E : LongWord;
    F       : Integer;
begin
  if B = 0 then
    RaiseDivByZeroError;

  C := A.Words[7];
  D := C div B;
  E := C mod B;
  Q.Words[7] := Word(D);

  for F := 6 downto 0 do
    begin
      C := (E shl 16) or A.Words[F];
      D := C div B;
      E := C mod B;
      Q.Words[F] := Word(D);
    end;

  R := Byte(E);
end;

procedure Word128DivideWord16(const A: Word128; const B: Word; var Q: Word128; var R: Word);
var C, D, E : LongWord;
    F       : Integer;
begin
  if B = 0 then
    RaiseDivByZeroError;

  C := A.Words[7];
  D := C div B;
  E := C mod B;
  Q.Words[7] := Word(D);

  for F := 6 downto 0 do
    begin
      C := (E shl 16) or A.Words[F];
      D := C div B;
      E := C mod B;
      Q.Words[F] := Word(D);
    end;

  R := Word(E);
end;

procedure Word128DivideWord32(const A: Word128; const B: LongWord; var Q: Word128; var R: LongWord);
var C : Integer;
    D : Word128;
    E : Word64;
begin
  // Handle special cases
  if B = 0 then // B = 0
    RaiseDivByZeroError else
  if B = 1 then // B = 1
    begin
      Q := A;
      R := 0;
      exit;
    end;
  if Word128IsZero(A) then // A = 0
    begin
      Word128InitZero(Q);
      R := 0;
      exit;
    end;
  C := Word128CompareWord32(A, B);
  if C < 0 then // A < B
    begin
      R := Word128ToWord32(A);
      Word128InitZero(Q);
      exit;
    end else
  if C = 0 then // A = B
    begin
      Word128InitOne(Q);
      R := 0;
      exit;
    end;
  // Divide using "restoring radix two" division
  D := A;
  Word64InitZero(E); // remainder (33 bits)
  Word128InitZero(Q); // quotient (128 bits)
  for C := 0 to 127 do
    begin
      // Shift high bit of dividend D into low bit of remainder E
      Word64Shl1(E);
      if D.LongWords[3] and $80000000 <> 0 then
        E.LongWords[0] := E.LongWords[0] or 1;
      Word128Shl1(D);
      // Shift quotient
      Word128Shl1(Q);
      // Subtract divisor from remainder if large enough
      if Word64CompareWord32(E, B) >= 0 then
        begin
          Word64SubtractWord32(E, B);
          // Set result bit in quotient
          Q.LongWords[0] := Q.LongWords[0] or 1;
        end;
    end;
  Assert(E.LongWords[1] = 0); // remainder (32 bits)
  R := E.LongWords[0];
end;

procedure Word128DivideWord64(const A: Word128; const B: Word64; var Q: Word128; var R: Word64);
var C : Integer;
    D : Word128;
    E : Word128;
begin
  // Handle special cases
  if Word64IsZero(B) then // B = 0
    RaiseDivByZeroError else
  if Word64IsOne(B) then // B = 1
    begin
      Q := A;
      Word64InitZero(R);
      exit;
    end;
  if Word128IsZero(A) then // A = 0
    begin
      Word128InitZero(Q);
      Word64InitZero(R);
      exit;
    end;
  C := Word128CompareWord64(A, B);
  if C < 0 then // A < B
    begin
      R := Word128ToWord64(A);
      Word128InitZero(Q);
      exit;
    end else
  if C = 0 then // A = B
    begin
      Word128InitOne(Q);
      Word64InitZero(R);
      exit;
    end;
  // Divide using "restoring radix two" division
  D := A;
  Word128InitZero(E); // remainder (65 bits)
  Word128InitZero(Q); // quotient (128 bits)
  for C := 0 to 127 do
    begin
      // Shift high bit of dividend D into low bit of remainder E
      Word128Shl1(E);
      if D.LongWords[3] and $80000000 <> 0 then
        E.LongWords[0] := E.LongWords[0] or 1;
      Word128Shl1(D);
      // Shift quotient
      Word128Shl1(Q);
      // Subtract divisor from remainder if large enough
      if Word128CompareWord64(E, B) >= 0 then
        begin
          Word128SubtractWord64(E, B);
          // Set result bit in quotient
          Q.LongWords[0] := Q.LongWords[0] or 1;
        end;
    end;
  Assert(E.LongWords[2] = 0); // remainder (64 bits)
  R := Word128ToWord64(E);
end;

procedure Word128DivideWord128(const A, B: Word128; var Q, R: Word128);
var C : Integer;
    D : Word128;
begin
  // Handle special cases
  if Word128IsZero(B) then // B = 0
    RaiseDivByZeroError else
  if Word128IsOne(B) then // B = 1
    begin
      Q := A;
      Word128InitZero(R);
      exit;
    end;
  if Word128IsZero(A) then // A = 0
    begin
      Word128InitZero(Q);
      Word128InitZero(R);
      exit;
    end;
  C := Word128CompareWord128(A, B);
  if C < 0 then // A < B
    begin
      R := A;
      Word128InitZero(Q);
      exit;
    end else
  if C = 0 then // A = B
    begin
      Word128InitOne(Q);
      Word128InitZero(R);
      exit;
    end;
  // Divide using "restoring radix two" division
  D := A;
  Word128InitZero(R); // remainder (128 bits)
  Word128InitZero(Q); // quotient (128 bits)
  for C := 0 to 127 do
    begin
      // Shift high bit of dividend D into low bit of remainder R
      Word128Shl1(R);
      if D.LongWords[3] and $80000000 <> 0 then
        R.LongWords[0] := R.LongWords[0] or 1;
      Word128Shl1(D);
      // Shift quotient
      Word128Shl1(Q);
      // Subtract divisor from remainder if large enough
      if Word128CompareWord128(R, B) >= 0 then
        begin
          Word128SubtractWord128(R, B);
          // Set result bit in quotient
          Q.LongWords[0] := Q.LongWords[0] or 1;
        end;
    end;
end;

procedure Word128ModWord128(const A, B: Word128; var R: Word128);
var Q : Word128;
begin
  Word128DivideWord128(A, B, Q, R);
end;

procedure Word128ToShortStrR(const A: Word128; var S: ShortString);
var C : Word128;
    D : Byte;
    I : Integer;
begin
  if Word128IsZero(A) then
    begin
      S := '0';
      exit;
    end;
  SetLength(S, 255);
  C := A;
  I := 0;
  repeat
    Word128DivideWord8(C, 10, C, D);
    Inc(I);
    S[I] := AnsiChar(D + 48);
  until Word128IsZero(C);
  SetLength(S, I);
end;

function Word128ToStr(const A: Word128): String;
var S : ShortString;
    I, L : Integer;
begin
  Word128ToShortStrR(A, S);
  L := Length(S);
  SetLength(Result, L);
  for I := 1 to L do
    Result[I] := Char(S[L - I + 1]);
end;

function Word128ToStrA(const A: Word128): AnsiString;
var S : ShortString;
    I, L : Integer;
begin
  Word128ToShortStrR(A, S);
  L := Length(S);
  SetLength(Result, L);
  for I := 1 to L do
    Result[I] := AnsiChar(S[L - I + 1]);
end;

function Word128ToStrW(const A: Word128): WideString;
var S : ShortString;
    I, L : Integer;
begin
  Word128ToShortStrR(A, S);
  L := Length(S);
  SetLength(Result, L);
  for I := 1 to L do
    Result[I] := WideChar(S[L - I + 1]);
end;

function Word128ToStrU(const A: Word128): UnicodeString;
var S : ShortString;
    I, L : Integer;
begin
  Word128ToShortStrR(A, S);
  L := Length(S);
  SetLength(Result, L);
  for I := 1 to L do
    Result[I] := WideChar(S[L - I + 1]);
end;

function StrToWord128(const A: String): Word128;
var I : Integer;
    B : Char;
    C : LongWord;
begin
  if A = '' then
    RaiseConvertError;
  Word128InitZero(Result);
  for I := 1 to Length(A) do
    begin
      B := A[I];
      if Ord(B) > $7F then
        RaiseConvertError;
      if not (AnsiChar(Ord(B)) in ['0'..'9']) then
        RaiseConvertError;
      C := Ord(A[I]) - Ord('0');
      Word128MultiplyWord8(Result, 10);
      Word128AddWord32(Result, C);
    end;
end;

function StrToWord128A(const A: AnsiString): Word128;
var I : Integer;
    B : AnsiChar;
    C : LongWord;
begin
  if A = '' then
    RaiseConvertError;
  Word128InitZero(Result);
  for I := 1 to Length(A) do
    begin
      B := A[I];
      if not (B in ['0'..'9']) then
        RaiseConvertError;
      C := Ord(A[I]) - Ord('0');
      Word128MultiplyWord8(Result, 10);
      Word128AddWord32(Result, C);
    end;
end;

function StrToWord128W(const A: WideString): Word128;
var I : Integer;
    B : WideChar;
    C : LongWord;
begin
  if A = '' then
    RaiseConvertError;
  Word128InitZero(Result);
  for I := 1 to Length(A) do
    begin
      B := A[I];
      if Ord(B) > $7F then
        RaiseConvertError;
      if not (AnsiChar(Ord(B)) in ['0'..'9']) then
        RaiseConvertError;
      C := Ord(A[I]) - Ord('0');
      Word128MultiplyWord8(Result, 10);
      Word128AddWord32(Result, C);
    end;
end;

function StrToWord128U(const A: UnicodeString): Word128;
var I : Integer;
    B : WideChar;
    C : LongWord;
begin
  if A = '' then
    RaiseConvertError;
  Word128InitZero(Result);
  for I := 1 to Length(A) do
    begin
      B := A[I];
      if Ord(B) > $7F then
        RaiseConvertError;
      if not (AnsiChar(Ord(B)) in ['0'..'9']) then
        RaiseConvertError;
      C := Ord(A[I]) - Ord('0');
      Word128MultiplyWord8(Result, 10);
      Word128AddWord32(Result, C);
    end;
end;

// Uses the Euclidean algorithm
function Word128GCD(const A, B: Word128): Word128;
var C, D, T : Word128;
begin
  C := A;
  D := B;
  while not Word128IsZero(D) do
    begin
      T := D;
      Word128ModWord128(C, D, D);
      C := T;
    end;
  Result := C;
end;

// Helper function for Word128Sqrt: Calculates initial estimate for Word128Sqrt
// Initial estimate is 1 shl (HighestSetBit div 2)
procedure Word128SqrtInitialEstimate(var A: Word128);
var I : Integer;
begin
  I := Word128SetBitScanReverse(A);
  I := I div 2;
  Word128InitOne(A);
  Word128Shl(A, I);
end;

// Calculates the integer square root of A
procedure Word128ISqrt(var A: Word128);
var B, C, D : Word128;
    I       : Byte;
begin
  // Handle special cases
  if Word128CompareWord32(A, 1) <= 0 then // A <= 1
    exit;
  // Divide algorithm based on Netwon's method for f(y,x) = y � x^2
  // xest <- (xest + y/xest) / 2
  C := A;
  Word128SqrtInitialEstimate(C);
  // Iterate until estimate converges or until a maximum number of iterations
  I := 0;
  repeat
    B := C;
    Word128DivideWord128(A, B, C, D);
    Word128AddWord128(C, B);
    Word128Shr1(C);
    Inc(I);
  until Word128EqualsWord128(C, B) or (I = 13);
  // Return result
  Assert(C.LongWords[1] = 0);
  A := C;
end;

function Word128Sqrt(const A: Word128): Extended;
var B, C, D : Extended;
    E       : Word128;
    I       : Byte;
begin
  // Handle special cases
  if Word128CompareWord32(A, 1) <= 0 then // A <= 1
    begin
      Result := Word128ToFloat(A);
      exit;
    end;
  // Divide algorithm based on Netwon's method for f(y,x) = y � x^2
  // xest <- (xest + y/xest) / 2
  E := A;
  Word128SqrtInitialEstimate(E);
  C := Word128ToFloat(E);
  // Iterate until estimate converges or until a maximum number of iterations
  D := Word128ToFloat(A);
  I := 0;
  repeat
    B := C;
    C := (B + D / B) / 2.0;
    Inc(I);
  until (C = B) or (I = 15);
  // Return result
  Result := C;
end;

procedure Word128Power(var A: Word128; const B: LongWord);
var R : Word128;
    C : Word128;
    D : LongWord;
begin
  if Word128IsOne(A) then
    exit;
  C := A;
  D := B;
  Word128InitOne(R);
  while D > 0 do
    if D and 1 = 0 then
      begin
        Word128Sqr(C);
        D := D shr 1;
      end
    else
      begin
        Word128MultiplyWord128Low(R, C);
        Dec(D);
      end;
  A := R;
end;

// Word128PowerAndMod
// Calculates A^E mod M
function Word128PowerAndMod(const A, E, M: Word128): Word128;
var P, Y, F : Word128;
    Q, T : Word256;
begin
  Word128InitOne(P);
  Y := A;
  F := E;
  while not Word128IsZero(F) do
    begin
      if Word128IsOdd(F) then
        begin
          Word128MultiplyWord128(P, Y, Q);  // Q := P * Y;
          Word256DivideWord128(Q, M, T, P); // P := Q mod M;
        end;
      Word128MultiplyWord128(Y, Y, Q);      // Q := Y * Y
      Word256DivideWord128(Q, M, T, Y);     // Y := Q mod M;
      Word128Shr1(F);
    end;
  Result := P;
end;

function Word128IsPrime(const A: Word128): Boolean;
var B, C : Word128;
    I : Integer;
    Q : Word128;
    R : Byte;
const MRTestValues128: array[1..20] of LongWord = (
      2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 61, 73, 350, 1215, 34862, 379215,
      457083754, 574237825, 3958281543);
begin
  // Check for values in 64 bit range
  if Word128IsWord64Range(A) then
    begin
      Result := Word64IsPrime(Word128ToWord64(A));
      exit;
    end;
  // Eliminate even numbers
  if not Word128IsOdd(A) then
    begin
      Result := False;
      exit;
    end;
  // Trial division using first few primes (3..
  for I := 1 to Word8PrimeCount - 1 do
    begin
      Word128DivideWord8(A, Word8PrimeTable[I], Q, R);
      if R = 0 then
        begin
          Result := False;
          exit;
        end;
    end;
  // Check primality using Miller-Rabin
  B := A;
  Word128SubtractWord32(B, 1); // B := A - 1;
  for I := 1 to 20 do
    begin
      Word128InitWord32(C, MRTestValues128[I]);
      if not Word128IsOne(Word128PowerAndMod(C, B, A)) then
        begin
          Result := False;
          exit;
        end;
    end;
  Result := True;
end;

procedure Word128NextPrime(var A: Word128);
begin
  if Word128IsMaximum(A) then
    begin
      Word128InitZero(A);
      exit;
    end;
  if not Word128IsOdd(A) then
    Word128AddWord32(A, 1)
  else
    Word128AddWord32(A, 2);
  while not Word128IsMaximum(A) do
    begin
      if Word128IsPrime(A) then
        exit;
      Word128AddWord32(A, 2);
    end;
  Word128InitZero(A);
end;

function Word128Hash(const A: Word128): LongWord;
begin
  Result := Word32Hash(A.LongWords[0]) xor
            Word32Hash(A.LongWords[1]) xor
            Word32Hash(A.LongWords[2]) xor
            Word32Hash(A.LongWords[3]);
end;



{                                                                              }
{ Word256                                                                      }
{                                                                              }
procedure Word256InitZero(var A: Word256);
begin
  A.LongWords[0] := 0;
  A.LongWords[1] := 0;
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
  A.LongWords[4] := 0;
  A.LongWords[5] := 0;
  A.LongWords[6] := 0;
  A.LongWords[7] := 0;
end;

procedure Word256InitOne(var A: Word256);
begin
  A.LongWords[0] := 1;
  A.LongWords[1] := 0;
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
  A.LongWords[4] := 0;
  A.LongWords[5] := 0;
  A.LongWords[6] := 0;
  A.LongWords[7] := 0;
end;

procedure Word256InitMinimum(var A: Word256);
begin
  A.LongWords[0] := 0;
  A.LongWords[1] := 0;
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
  A.LongWords[4] := 0;
  A.LongWords[5] := 0;
  A.LongWords[6] := 0;
  A.LongWords[7] := 0;
end;

procedure Word256InitMaximum(var A: Word256);
begin
  A.LongWords[0] := $FFFFFFFF;
  A.LongWords[1] := $FFFFFFFF;
  A.LongWords[2] := $FFFFFFFF;
  A.LongWords[3] := $FFFFFFFF;
  A.LongWords[4] := $FFFFFFFF;
  A.LongWords[5] := $FFFFFFFF;
  A.LongWords[6] := $FFFFFFFF;
  A.LongWords[7] := $FFFFFFFF;
end;

function Word256IsZero(const A: Word256): Boolean;
begin
  Result :=
      (A.LongWords[0] = 0) and
      (A.LongWords[1] = 0) and
      (A.LongWords[2] = 0) and
      (A.LongWords[3] = 0) and
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

function Word256IsOne(const A: Word256): Boolean;
begin
  Result :=
      (A.LongWords[0] = 1) and
      (A.LongWords[1] = 0) and
      (A.LongWords[2] = 0) and
      (A.LongWords[3] = 0) and
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

function Word256IsMinimum(const A: Word256): Boolean;
begin
  Result :=
      (A.LongWords[0] = 0) and
      (A.LongWords[1] = 0) and
      (A.LongWords[2] = 0) and
      (A.LongWords[3] = 0) and
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

function Word256IsMaximum(const A: Word256): Boolean;
begin
  Result :=
      (A.LongWords[0] = $FFFFFFFF) and
      (A.LongWords[1] = $FFFFFFFF) and
      (A.LongWords[2] = $FFFFFFFF) and
      (A.LongWords[3] = $FFFFFFFF) and
      (A.LongWords[4] = $FFFFFFFF) and
      (A.LongWords[5] = $FFFFFFFF) and
      (A.LongWords[6] = $FFFFFFFF) and
      (A.LongWords[7] = $FFFFFFFF);
end;

function Word256IsOdd(const A: Word256): Boolean;
begin
  Result := (A.LongWords[0] and 1 <> 0);
end;

function Word256IsWord32Range(const A: Word256): Boolean;
begin
  Result :=
      (A.LongWords[1] = 0) and
      (A.LongWords[2] = 0) and
      (A.LongWords[3] = 0) and
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

function Word256IsWord64Range(const A: Word256): Boolean;
begin
  Result :=
      (A.LongWords[2] = 0) and
      (A.LongWords[3] = 0) and
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

function Word256IsWord128Range(const A: Word256): Boolean;
begin
  Result :=
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

function Word256IsInt32Range(const A: Word256): Boolean;
begin
  Result :=
      (A.LongWords[0] < $80000000) and
      (A.LongWords[1] = 0) and
      (A.LongWords[2] = 0) and
      (A.LongWords[3] = 0) and
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

function Word256IsInt64Range(const A: Word256): Boolean;
begin
  Result :=
      (A.LongWords[1] < $80000000) and
      (A.LongWords[2] = 0) and
      (A.LongWords[3] = 0) and
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

function Word256IsInt128Range(const A: Word256): Boolean;
begin
  Result :=
      (A.LongWords[3] < $80000000) and
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

procedure Word256InitWord32(var A: Word256; const B: LongWord);
begin
  A.LongWords[0] := B;
  A.LongWords[1] := 0;
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
  A.LongWords[4] := 0;
  A.LongWords[5] := 0;
  A.LongWords[6] := 0;
  A.LongWords[7] := 0;
end;

procedure Word256InitWord64(var A: Word256; const B: Word64);
begin
  A.Word64s[0] := B;
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
  A.LongWords[4] := 0;
  A.LongWords[5] := 0;
  A.LongWords[6] := 0;
  A.LongWords[7] := 0;
end;

procedure Word256InitWord128(var A: Word256; const B: Word128);
begin
  A.Word128s[0] := B;
  A.LongWords[4] := 0;
  A.LongWords[5] := 0;
  A.LongWords[6] := 0;
  A.LongWords[7] := 0;
end;

procedure Word256InitInt32(var A: Word256; const B: LongInt);
begin
  {$IFOPT R+}
  if B < 0 then
    RaiseRangeError;
  {$ENDIF}
  A.LongWords[0] := LongWord(B);
  A.LongWords[1] := 0;
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
  A.LongWords[4] := 0;
  A.LongWords[5] := 0;
  A.LongWords[6] := 0;
  A.LongWords[7] := 0;
end;

procedure Word256InitInt64(var A: Word256; const B: Int64);
begin
  {$IFOPT R+}
  if B < 0 then
    RaiseRangeError;
  {$ENDIF}
  A.LongWords[0] := Int64Rec(B).LongWords[0];
  A.LongWords[1] := Int64Rec(B).LongWords[1];
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
  A.LongWords[4] := 0;
  A.LongWords[5] := 0;
  A.LongWords[6] := 0;
  A.LongWords[7] := 0;
end;

procedure Word256InitInt128(var A: Word256; const B: Int128);
begin
  {$IFOPT R+}
  if B.LongWords[3] and $80000000 <> 0 then
    RaiseRangeError;
  {$ENDIF}
  A.LongWords[0] := B.LongWords[0];
  A.LongWords[1] := B.LongWords[1];
  A.LongWords[2] := B.LongWords[2];
  A.LongWords[3] := B.LongWords[3];
  A.LongWords[4] := 0;
  A.LongWords[5] := 0;
  A.LongWords[6] := 0;
  A.LongWords[7] := 0;
end;

function Word256ToWord32(const A: Word256): LongWord;
begin
  {$IFOPT R+}
  if not Word256IsWord32Range(A) then
    RaiseRangeError;
  {$ENDIF}
  Result := A.LongWords[0];
end;

function Word256ToWord64(const A: Word256): Word64;
begin
  {$IFOPT R+}
  if not Word256IsWord64Range(A) then
    RaiseRangeError;
  {$ENDIF}
  Result.LongWords[0] := A.LongWords[0];
  Result.LongWords[1] := A.LongWords[1];
end;

function Word256ToWord128(const A: Word256): Word128;
begin
  {$IFOPT R+}
  if not Word256IsWord128Range(A) then
    RaiseRangeError;
  {$ENDIF}
  Result.LongWords[0] := A.LongWords[0];
  Result.LongWords[1] := A.LongWords[1];
  Result.LongWords[2] := A.LongWords[2];
  Result.LongWords[3] := A.LongWords[3];
end;

function Word256ToInt32(const A: Word256): LongInt;
begin
  {$IFOPT R+}
  if not Word256IsInt32Range(A) then
    RaiseRangeError;
  {$ENDIF}
  Result := LongInt(A.LongWords[0]);
end;

function Word256ToInt64(const A: Word256): Int64;
begin
  {$IFOPT R+}
  if not Word256IsInt64Range(A) then
    RaiseRangeError;
  {$ENDIF}
  Int64Rec(Result).LongWords[0] := A.LongWords[0];
  Int64Rec(Result).LongWords[1] := A.LongWords[1];
end;

function Word256ToInt128(const A: Word256): Int128;
begin
  {$IFOPT R+}
  if not Word256IsInt128Range(A) then
    RaiseRangeError;
  {$ENDIF}
  Result.LongWords[0] := A.LongWords[0];
  Result.LongWords[1] := A.LongWords[1];
  Result.LongWords[2] := A.LongWords[2];
  Result.LongWords[3] := A.LongWords[3];
end;

function Word256ToFloat(const A: Word256): Extended;
const F : Extended = 4294967296.0 * 4294967296.0 * 4294967296.0 * 4294967296.0;
var T : Extended;
begin
  Result := A.LongWords[0];
  T := A.LongWords[1];
  Result := Result + T * 4294967296.0;
  T := A.LongWords[2];
  Result := Result + T * 4294967296.0 * 4294967296.0;
  T := A.LongWords[3];
  Result := Result +  T * 4294967296.0 * 4294967296.0 * 4294967296.0;
  T := A.LongWords[4];
  Result := Result + T * F;
  T := A.LongWords[5];
  Result := Result + T * F * 4294967296.0;
  T := A.LongWords[6];
  Result := Result + T * F * 4294967296.0 * 4294967296.0;
  T := A.LongWords[7];
  Result := Result + T * F * 4294967296.0 * 4294967296.0 * 4294967296.0;
end;

function Word256Lo(const A: Word256): Word128;
begin
  Result := A.Word128s[0];
end;

function Word256Hi(const A: Word256): Word128;
begin
  Result := A.Word128s[1];
end;

function Word256EqualsWord32(const A: Word256; const B: LongWord): Boolean;
begin
  Result := (A.LongWords[0] = B) and
            (A.LongWords[1] = 0) and
            (A.LongWords[2] = 0) and
            (A.LongWords[3] = 0) and
            (A.LongWords[4] = 0) and
            (A.LongWords[5] = 0) and
            (A.LongWords[6] = 0) and
            (A.LongWords[7] = 0);
end;

function Word256EqualsWord64(const A: Word256; const B: Word64): Boolean;
begin
  Result := (A.LongWords[0] = B.LongWords[0]) and
            (A.LongWords[1] = B.LongWords[1]) and
            (A.LongWords[2] = 0) and
            (A.LongWords[3] = 0) and
            (A.LongWords[4] = 0) and
            (A.LongWords[5] = 0) and
            (A.LongWords[6] = 0) and
            (A.LongWords[7] = 0);
end;

function Word256EqualsWord128(const A: Word256; const B: Word128): Boolean;
begin
  Result :=
     (A.LongWords[0] = B.LongWords[0]) and
     (A.LongWords[1] = B.LongWords[1]) and
     (A.LongWords[2] = B.LongWords[2]) and
     (A.LongWords[3] = B.LongWords[3]) and
     (A.LongWords[4] = 0) and
     (A.LongWords[5] = 0) and
     (A.LongWords[6] = 0) and
     (A.LongWords[7] = 0);
end;

function Word256EqualsWord256(const A, B: Word256): Boolean;
begin
  Result :=
     (A.LongWords[0] = B.LongWords[0]) and
     (A.LongWords[1] = B.LongWords[1]) and
     (A.LongWords[2] = B.LongWords[2]) and
     (A.LongWords[3] = B.LongWords[3]) and
     (A.LongWords[4] = B.LongWords[4]) and
     (A.LongWords[5] = B.LongWords[5]) and
     (A.LongWords[6] = B.LongWords[6]) and
     (A.LongWords[7] = B.LongWords[7]);
end;

function Word256EqualsInt32(const A: Word256; const B: LongInt): Boolean;
begin
  if B < 0 then
    Result := False
  else
    Result := (A.LongWords[0] = LongWord(B)) and
              (A.LongWords[1] = 0) and
              (A.LongWords[2] = 0) and
              (A.LongWords[3] = 0) and
              (A.LongWords[4] = 0) and
              (A.LongWords[5] = 0) and
              (A.LongWords[6] = 0) and
              (A.LongWords[7] = 0);
end;

function Word256EqualsInt64(const A: Word256; const B: Int64): Boolean;
begin
  if B < 0 then
    Result := False
  else
    Result := (A.LongWords[0] = Int64Rec(B).LongWords[0]) and
              (A.LongWords[1] = Int64Rec(B).LongWords[1]) and
              (A.LongWords[2] = 0) and
              (A.LongWords[3] = 0) and
              (A.LongWords[4] = 0) and
              (A.LongWords[5] = 0) and
              (A.LongWords[6] = 0) and
              (A.LongWords[7] = 0);
end;

function Word256EqualsInt128(const A: Word256; const B: Int128): Boolean;
begin
  if B.LongWords[3] and $80000000 <> 0 then
    Result := False
  else
    Result := (A.LongWords[0] = B.LongWords[0]) and
              (A.LongWords[1] = B.LongWords[1]) and
              (A.LongWords[2] = B.LongWords[2]) and
              (A.LongWords[3] = B.LongWords[3]) and
              (A.LongWords[4] = 0) and
              (A.LongWords[5] = 0) and
              (A.LongWords[6] = 0) and
              (A.LongWords[7] = 0);
end;

function Word256CompareWord32(const A: Word256; const B: LongWord): Integer;
var C : LongWord;
begin
  if (A.LongWords[1] <> 0) or
     (A.LongWords[2] <> 0) or
     (A.LongWords[3] <> 0) or
     (A.LongWords[4] <> 0) or
     (A.LongWords[5] <> 0) or
     (A.LongWords[6] <> 0) or
     (A.LongWords[7] <> 0) then
    Result := 1
  else
    begin
      C := A.LongWords[0];
      if C > B then
        Result := 1 else
      if C < B then
        Result := -1
      else
        Result := 0;
    end;
end;

function Word256CompareWord64(const A: Word256; const B: Word64): Integer;
var C, D : LongWord;
begin
  if (A.LongWords[2] <> 0) or
     (A.LongWords[3] <> 0) or
     (A.LongWords[4] <> 0) or
     (A.LongWords[5] <> 0) or
     (A.LongWords[6] <> 0) or
     (A.LongWords[7] <> 0) then
    Result := 1
  else
    begin
      C := A.LongWords[1];
      D := B.LongWords[1];
      if C = D then
        begin
          C := A.LongWords[0];
          D := B.LongWords[0];
        end;
      if C > D then
        Result := 1 else
      if C < D then
        Result := -1
      else
        Result := 0;
    end;
end;

function Word256CompareWord128(const A: Word256; const B: Word128): Integer;
var C, D : LongWord;
begin
  if (A.LongWords[4] <> 0) or
     (A.LongWords[5] <> 0) or
     (A.LongWords[6] <> 0) or
     (A.LongWords[7] <> 0) then
    Result := 1
  else
    begin
      C := A.LongWords[3];
      D := B.LongWords[3];
      if C = D then
        begin
          C := A.LongWords[2];
          D := B.LongWords[2];
          if C = D then
            begin
              C := A.LongWords[1];
              D := B.LongWords[1];
              if C = D then
                begin
                  C := A.LongWords[0];
                  D := B.LongWords[0];
                end;
            end;
        end;
      if C > D then
        Result := 1 else
      if C < D then
        Result := -1
      else
        Result := 0;
    end;
end;

function Word256CompareWord256(const A, B: Word256): Integer;
var C, D : LongWord;
begin
  C := A.LongWords[7];
  D := B.LongWords[7];
  if C = D then
    begin
      C := A.LongWords[6];
      D := B.LongWords[6];
      if C = D then
        begin
          C := A.LongWords[5];
          D := B.LongWords[5];
          if C = D then
            begin
              C := A.LongWords[4];
              D := B.LongWords[4];
              if C = D then
                begin
                  C := A.LongWords[3];
                  D := B.LongWords[3];
                  if C = D then
                    begin
                      C := A.LongWords[2];
                      D := B.LongWords[2];
                      if C = D then
                        begin
                          C := A.LongWords[1];
                          D := B.LongWords[1];
                          if C = D then
                            begin
                             C := A.LongWords[0];
                             D := B.LongWords[0];
                            end;
                        end;
                    end;
                end;
            end;
        end;
    end;
  if C > D then
    Result := 1 else
  if C < D then
    Result := -1
  else
    Result := 0;
end;

function Word256CompareInt32(const A: Word256; const B: LongInt): Integer;
var C : LongWord;
begin
  if B < 0 then
    Result := 1 else
  if (A.LongWords[1] <> 0) or
     (A.LongWords[2] <> 0) or
     (A.LongWords[3] <> 0) or
     (A.LongWords[4] <> 0) or
     (A.LongWords[5] <> 0) or
     (A.LongWords[6] <> 0) or
     (A.LongWords[7] <> 0) then
    Result := 1
  else
    begin
      C := A.LongWords[0];
      if C > LongWord(B) then
        Result := 1 else
      if C < LongWord(B) then
        Result := -1
      else
        Result := 0;
    end;
end;

function Word256CompareInt64(const A: Word256; const B: Int64): Integer;
var C, D : LongWord;
begin
  if B < 0 then
    Result := 1 else
  if (A.LongWords[2] <> 0) or
     (A.LongWords[3] <> 0) or
     (A.LongWords[4] <> 0) or
     (A.LongWords[5] <> 0) or
     (A.LongWords[6] <> 0) or
     (A.LongWords[7] <> 0) then
    Result := 1
  else
    begin
      C := A.LongWords[1];
      D := Int64Rec(B).LongWords[1];
      if C = D then
        begin
          C := A.LongWords[0];
          D := Int64Rec(B).LongWords[0];
        end;
      if C > D then
        Result := 1 else
      if C < B then
        Result := -1
      else
        Result := 0;
    end;
end;

function Word256CompareInt128(const A: Word256; const B: Int128): Integer;
var C, D : LongWord;
begin
  if B.LongWords[3] and $80000000 <> 0 then
    Result := 1 else
  if (A.LongWords[4] <> 0) or
     (A.LongWords[5] <> 0) or
     (A.LongWords[6] <> 0) or
     (A.LongWords[7] <> 0) then
    Result := 1
  else
    begin
      C := A.LongWords[3];
      D := B.LongWords[3];
      if C = D then
        begin
          C := A.LongWords[2];
          D := B.LongWords[2];
          if C = D then
            begin
              C := A.LongWords[1];
              D := B.LongWords[1];
              if C = D then
                begin
                  C := A.LongWords[0];
                  D := B.LongWords[0];
                end;
            end;
        end;
      if C > D then
        Result := 1 else
      if C < D then
        Result := -1
      else
        Result := 0;
    end;
end;

function Word256Min(const A, B: Word256): Word256;
begin
  if Word256CompareWord256(A, B) <= 0 then
    Result := A
  else
    Result := B;
end;

function Word256Max(const A, B: Word256): Word256;
begin
  if Word256CompareWord256(A, B) >= 0 then
    Result := A
  else
    Result := B;
end;

procedure Word256Not(var A: Word256);
begin
  A.LongWords[0] := not A.LongWords[0];
  A.LongWords[1] := not A.LongWords[1];
  A.LongWords[2] := not A.LongWords[2];
  A.LongWords[3] := not A.LongWords[3];
  A.LongWords[4] := not A.LongWords[4];
  A.LongWords[5] := not A.LongWords[5];
  A.LongWords[6] := not A.LongWords[6];
  A.LongWords[7] := not A.LongWords[7];
end;

procedure Word256OrWord256(var A: Word256; const B: Word256);
begin
  A.LongWords[0] := A.LongWords[0] or B.LongWords[0];
  A.LongWords[1] := A.LongWords[1] or B.LongWords[1];
  A.LongWords[2] := A.LongWords[2] or B.LongWords[2];
  A.LongWords[3] := A.LongWords[3] or B.LongWords[3];
  A.LongWords[4] := A.LongWords[4] or B.LongWords[4];
  A.LongWords[5] := A.LongWords[5] or B.LongWords[5];
  A.LongWords[6] := A.LongWords[6] or B.LongWords[6];
  A.LongWords[7] := A.LongWords[7] or B.LongWords[7];
end;

procedure Word256AndWord256(var A: Word256; const B: Word256);
begin
  A.LongWords[0] := A.LongWords[0] and B.LongWords[0];
  A.LongWords[1] := A.LongWords[1] and B.LongWords[1];
  A.LongWords[2] := A.LongWords[2] and B.LongWords[2];
  A.LongWords[3] := A.LongWords[3] and B.LongWords[3];
  A.LongWords[4] := A.LongWords[4] and B.LongWords[4];
  A.LongWords[5] := A.LongWords[5] and B.LongWords[5];
  A.LongWords[6] := A.LongWords[6] and B.LongWords[6];
  A.LongWords[7] := A.LongWords[7] and B.LongWords[7];
end;

procedure Word256XorWord256(var A: Word256; const B: Word256);
begin
  A.LongWords[0] := A.LongWords[0] xor B.LongWords[0];
  A.LongWords[1] := A.LongWords[1] xor B.LongWords[1];
  A.LongWords[2] := A.LongWords[2] xor B.LongWords[2];
  A.LongWords[3] := A.LongWords[3] xor B.LongWords[3];
  A.LongWords[4] := A.LongWords[4] xor B.LongWords[4];
  A.LongWords[5] := A.LongWords[5] xor B.LongWords[5];
  A.LongWords[6] := A.LongWords[6] xor B.LongWords[6];
  A.LongWords[7] := A.LongWords[7] xor B.LongWords[7];
end;

function Word256IsBitSet(const A: Word256; const B: Integer): Boolean;
begin
  if (B < 0) or (B > 255) then
    Result := False
  else
    Result := (A.LongWords[B shr 5] and (1 shl (B and $1F)) <> 0);
end;

procedure Word256SetBit(var A: Word256; const B: Integer);
begin
  if (B < 0) or (B > 255) then
    exit;
  A.LongWords[B shr 5] := A.LongWords[B shr 5] or (1 shl (B and $1F));
end;

procedure Word256ClearBit(var A: Word256; const B: Integer);
begin
  if (B < 0) or (B > 255) then
    exit;
  A.LongWords[B shr 5] := A.LongWords[B shr 5] and not (1 shl (B and $1F));
end;

procedure Word256ToggleBit(var A: Word256; const B: Integer);
begin
  if (B < 0) or (B > 127) then
    exit;
  A.LongWords[B shr 5] := A.LongWords[B shr 5] xor (1 shl (B and $1F));
end;

procedure Word256Shl1(var A: Word256);
begin
  A.LongWords[7] := (A.LongWords[7] shl 1) or (A.LongWords[6] shr 31);
  A.LongWords[6] := (A.LongWords[6] shl 1) or (A.LongWords[5] shr 31);
  A.LongWords[5] := (A.LongWords[5] shl 1) or (A.LongWords[4] shr 31);
  A.LongWords[4] := (A.LongWords[4] shl 1) or (A.LongWords[3] shr 31);
  A.LongWords[3] := (A.LongWords[3] shl 1) or (A.LongWords[2] shr 31);
  A.LongWords[2] := (A.LongWords[2] shl 1) or (A.LongWords[1] shr 31);
  A.LongWords[1] := (A.LongWords[1] shl 1) or (A.LongWords[0] shr 31);
  A.LongWords[0] := (A.LongWords[0] shl 1);
end;

procedure Word256Shr1(var A: Word256);
begin
  A.LongWords[0] := (A.LongWords[0] shr 1) or (A.LongWords[1] shl 31);
  A.LongWords[1] := (A.LongWords[1] shr 1) or (A.LongWords[2] shl 31);
  A.LongWords[2] := (A.LongWords[2] shr 1) or (A.LongWords[3] shl 31);
  A.LongWords[3] := (A.LongWords[3] shr 1) or (A.LongWords[4] shl 31);
  A.LongWords[4] := (A.LongWords[4] shr 1) or (A.LongWords[5] shl 31);
  A.LongWords[5] := (A.LongWords[5] shr 1) or (A.LongWords[6] shl 31);
  A.LongWords[6] := (A.LongWords[6] shr 1) or (A.LongWords[7] shl 31);
  A.LongWords[7] := (A.LongWords[7] shr 1);
end;

procedure Word256Rol1(var A: Word256);
var B : Word256;
begin
  B.LongWords[7] := (A.LongWords[7] shl 1) or (A.LongWords[6] shr 31);
  B.LongWords[6] := (A.LongWords[6] shl 1) or (A.LongWords[5] shr 31);
  B.LongWords[5] := (A.LongWords[5] shl 1) or (A.LongWords[4] shr 31);
  B.LongWords[4] := (A.LongWords[4] shl 1) or (A.LongWords[3] shr 31);
  B.LongWords[3] := (A.LongWords[3] shl 1) or (A.LongWords[2] shr 31);
  B.LongWords[2] := (A.LongWords[2] shl 1) or (A.LongWords[1] shr 31);
  B.LongWords[1] := (A.LongWords[1] shl 1) or (A.LongWords[0] shr 31);
  B.LongWords[0] := (A.LongWords[0] shl 1) or (A.LongWords[7] shr 31);
  A := B;
end;

procedure Word256Ror1(var A: Word256);
var B : Word256;
begin
  B.LongWords[7] := (A.LongWords[7] shr 1) or (A.LongWords[0] shl 31);
  B.LongWords[6] := (A.LongWords[6] shr 1) or (A.LongWords[7] shl 31);
  B.LongWords[5] := (A.LongWords[5] shr 1) or (A.LongWords[6] shl 31);
  B.LongWords[4] := (A.LongWords[4] shr 1) or (A.LongWords[5] shl 31);
  B.LongWords[3] := (A.LongWords[3] shr 1) or (A.LongWords[4] shl 31);
  B.LongWords[2] := (A.LongWords[2] shr 1) or (A.LongWords[3] shl 31);
  B.LongWords[1] := (A.LongWords[1] shr 1) or (A.LongWords[2] shl 31);
  B.LongWords[0] := (A.LongWords[0] shr 1) or (A.LongWords[1] shl 31);
  A := B;
end;

procedure Word256Swap(var A, B: Word256);
var D : LongWord;
begin
  D := A.LongWords[0];
  A.LongWords[0] := B.LongWords[0];
  B.LongWords[0] := D;
  D := A.LongWords[1];
  A.LongWords[1] := B.LongWords[1];
  B.LongWords[1] := D;
  D := A.LongWords[2];
  A.LongWords[2] := B.LongWords[2];
  B.LongWords[2] := D;
  D := A.LongWords[3];
  A.LongWords[3] := B.LongWords[3];
  B.LongWords[3] := D;
  D := A.LongWords[4];
  A.LongWords[4] := B.LongWords[4];
  B.LongWords[4] := D;
  D := A.LongWords[5];
  A.LongWords[5] := B.LongWords[5];
  B.LongWords[5] := D;
  D := A.LongWords[6];
  A.LongWords[6] := B.LongWords[6];
  B.LongWords[6] := D;
  D := A.LongWords[7];
  A.LongWords[7] := B.LongWords[7];
  B.LongWords[7] := D;
end;

procedure Word256AddWord32(var A: Word256; const B: LongWord);
var C : LongWord;
    D : Integer;
begin
  if B = 0 then
    exit;

  C := A.Words[0];
  Inc(C, B and $FFFF);
  A.Words[0] := Word(C and $FFFF);

  C := C shr 16;
  Inc(C, A.Words[1]);
  Inc(C, B shr 16);
  A.Words[1] := Word(C and $FFFF);

  for D := 2 to 15 do
    begin
      C := C shr 16;
      if C = 0 then exit;
      Inc(C, A.Words[D]);
      A.Words[D] := Word(C and $FFFF);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word256AddWord64(var A: Word256; const B: Word64);
var C : LongWord;
    D : Integer;
begin
  C := LongWord(A.Words[0]) + B.Words[0];
  A.Words[0] := Word(C and $FFFF);

  for D := 1 to 3 do
    begin
      C := C shr 16;
      Inc(C, A.Words[D]);
      Inc(C, B.Words[D]);
      A.Words[D] := Word(C and $FFFF);
    end;

  for D := 4 to 15 do
    begin
      C := C shr 16;
      if C = 0 then exit;
      Inc(C, A.Words[D]);
      A.Words[D] := Word(C and $FFFF);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word256AddWord128(var A: Word256; const B: Word128);
var C : LongWord;
    D : Integer;
begin
  C := LongWord(A.Words[0]) + B.Words[0];
  A.Words[0] := Word(C and $FFFF);

  for D := 1 to 7 do
    begin
      C := C shr 16;
      Inc(C, A.Words[D]);
      Inc(C, B.Words[D]);
      A.Words[D] := Word(C and $FFFF);
    end;

  for D := 8 to 15 do
    begin
      C := C shr 16;
      if C = 0 then exit;
      Inc(C, A.Words[D]);
      A.Words[D] := Word(C and $FFFF);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word256AddWord256(var A: Word256; const B: Word256);
var C : LongWord;
    D : Integer;
begin
  C := LongWord(A.Words[0]) + B.Words[0];
  A.Words[0] := Word(C and $FFFF);

  for D := 1 to 15 do
    begin
      C := C shr 16;
      Inc(C, A.Words[D]);
      Inc(C, B.Words[D]);
      A.Words[D] := Word(C and $FFFF);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word256SubtractWord32(var A: Word256; const B: LongWord);
var C, D : Integer;
begin
  C := A.Words[0];
  Dec(C, B and $FFFF);
  A.Words[0] := Word(C);

  if C < 0 then C := -1 else C := 0;
  Inc(C, A.Words[1]);
  Dec(C, B shr 16);
  A.Words[1] := Word(C);

  if C < 0 then C := -1 else exit;
  Inc(C, A.Words[2]);
  A.Words[2] := Word(C);

  for D := 3 to 15 do
    begin
      if C >= 0 then exit;
      Inc(C, A.Words[D]);
      A.Words[D] := Word(C);
    end;

  {$IFOPT Q+}
  if C < 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word256SubtractWord64(var A: Word256; const B: Word64);
var C, D : Integer;
begin
  C := A.Words[0];
  Dec(C, B.Words[0]);
  A.Words[0] := Word(C);

  for D := 1 to 3 do
    begin
      if C < 0 then C := -1 else C := 0;
      Inc(C, A.Words[D]);
      Dec(C, B.Words[D]);
      A.Words[D] := Word(C);
    end;

  if C < 0 then C := -1 else exit;
  Inc(C, A.Words[4]);
  A.Words[4] := Word(C);

  for D := 5 to 15 do
    begin
      if C >= 0 then exit;
      Inc(C, A.Words[D]);
      A.Words[D] := Word(C);
    end;

  {$IFOPT Q+}
  if C < 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word256SubtractWord128(var A: Word256; const B: Word128);
var C, D : Integer;
begin
  C := A.Words[0];
  Dec(C, B.Words[0]);
  A.Words[0] := Word(C);

  for D := 1 to 7 do
    begin
      if C < 0 then C := -1 else C := 0;
      Inc(C, A.Words[D]);
      Dec(C, B.Words[D]);
      A.Words[D] := Word(C);
    end;

  if C < 0 then C := -1 else exit;
  Inc(C, A.Words[8]);
  A.Words[8] := Word(C);

  for D := 9 to 15 do
    begin
      if C >= 0 then exit;
      Inc(C, A.Words[D]);
      A.Words[D] := Word(C);
    end;

  {$IFOPT Q+}
  if C < 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word256SubtractWord256(var A: Word256; const B: Word256);
var C, D : Integer;
begin
  C := A.Words[0];
  Dec(C, B.Words[0]);
  A.Words[0] := Word(C);

  for D := 1 to 15 do
    begin
      if C < 0 then C := -1 else C := 0;
      Inc(C, A.Words[D]);
      Dec(C, B.Words[D]);
      A.Words[D] := Word(C);
    end;

  {$IFOPT Q+}
  if C < 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word256MultiplyWord8(var A: Word256; const B: Byte);
var C : LongWord;
    D : Integer;
begin
  C := LongWord(A.Words[0]) * B;
  A.Words[0] := Word(C);

  for D := 1 to 15 do
    begin
      C := C shr 16;
      Inc(C, LongWord(A.Words[D]) * B);
      A.Words[D] := Word(C);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word256MultiplyWord16(var A: Word256; const B: Word);
var C : LongWord;
    D : Integer;
begin
  C := LongWord(A.Words[0]) * B;
  A.Words[0] := Word(C);

  for D := 1 to 15 do
    begin
      C := C shr 16;
      Inc(C, LongWord(A.Words[D]) * B);
      A.Words[D] := Word(C);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Word256MultiplyWord32(var A: Word256; const B: LongWord);
var R    : Word256;
    I, J : Word;
    C    : Int64;
    D    : Integer;
begin
  I := Word(B and $FFFF);
  J := Word(B shr 16);

  C := LongWord(A.Words[0]) * I;
  R.Words[0] := Word(C);

  for D := 1 to 15 do
    begin
      C := C shr 16;
      Inc(C, LongWord(A.Words[D - 1]) * J);
      Inc(C, LongWord(A.Words[D]) * I);
      R.Words[D] := Word(C);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  Inc(C, LongWord(A.Words[7]) * J);
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}

  A := R;
end;

procedure Word256MultiplyWord64(var A: Word256; const B: Word64);
var R : Word256;
    C : Int64;
    I : Integer;
begin
  C := LongWord(A.Words[0]) * B.Words[0];
  R.Words[0] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[1]);
  Inc(C, LongWord(A.Words[1]) * B.Words[0]);
  R.Words[1] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[2]);
  Inc(C, LongWord(A.Words[1]) * B.Words[1]);
  Inc(C, LongWord(A.Words[2]) * B.Words[0]);
  R.Words[2] := Word(C);

  for I := 0 to 12 do
    begin
      C := C shr 16;
      Inc(C, LongWord(A.Words[I]    ) * B.Words[3]);
      Inc(C, LongWord(A.Words[I + 1]) * B.Words[2]);
      Inc(C, LongWord(A.Words[I + 2]) * B.Words[1]);
      Inc(C, LongWord(A.Words[I + 3]) * B.Words[0]);
      R.Words[I + 3] := Word(C);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  Inc(C, LongWord(A.Words[13]) * B.Words[3]);
  Inc(C, LongWord(A.Words[14]) * B.Words[2]);
  Inc(C, LongWord(A.Words[15]) * B.Words[1]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[14]) * B.Words[3]);
  Inc(C, LongWord(A.Words[15]) * B.Words[2]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[15]) * B.Words[3]);
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}

  A := R;
end;

procedure Word256MultiplyWord128(var A: Word256; const B: Word128);
var R : Word256;
    C : Int64;
    I : Integer;
begin
  C := LongWord(A.Words[0]) * B.Words[0];
  R.Words[0] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[1]);
  Inc(C, LongWord(A.Words[1]) * B.Words[0]);
  R.Words[1] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[2]);
  Inc(C, LongWord(A.Words[1]) * B.Words[1]);
  Inc(C, LongWord(A.Words[2]) * B.Words[0]);
  R.Words[2] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[3]);
  Inc(C, LongWord(A.Words[1]) * B.Words[2]);
  Inc(C, LongWord(A.Words[2]) * B.Words[1]);
  Inc(C, LongWord(A.Words[3]) * B.Words[0]);
  R.Words[3] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[4]);
  Inc(C, LongWord(A.Words[1]) * B.Words[3]);
  Inc(C, LongWord(A.Words[2]) * B.Words[2]);
  Inc(C, LongWord(A.Words[3]) * B.Words[1]);
  Inc(C, LongWord(A.Words[4]) * B.Words[0]);
  R.Words[4] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[5]);
  Inc(C, LongWord(A.Words[1]) * B.Words[4]);
  Inc(C, LongWord(A.Words[2]) * B.Words[3]);
  Inc(C, LongWord(A.Words[3]) * B.Words[2]);
  Inc(C, LongWord(A.Words[4]) * B.Words[1]);
  Inc(C, LongWord(A.Words[5]) * B.Words[0]);
  R.Words[5] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[6]);
  Inc(C, LongWord(A.Words[1]) * B.Words[5]);
  Inc(C, LongWord(A.Words[2]) * B.Words[4]);
  Inc(C, LongWord(A.Words[3]) * B.Words[3]);
  Inc(C, LongWord(A.Words[4]) * B.Words[2]);
  Inc(C, LongWord(A.Words[5]) * B.Words[1]);
  Inc(C, LongWord(A.Words[6]) * B.Words[0]);
  R.Words[6] := Word(C);

  for I := 0 to 8 do
    begin
      C := C shr 16;
      Inc(C, LongWord(A.Words[I]    ) * B.Words[7]);
      Inc(C, LongWord(A.Words[I + 1]) * B.Words[6]);
      Inc(C, LongWord(A.Words[I + 2]) * B.Words[5]);
      Inc(C, LongWord(A.Words[I + 3]) * B.Words[4]);
      Inc(C, LongWord(A.Words[I + 4]) * B.Words[3]);
      Inc(C, LongWord(A.Words[I + 5]) * B.Words[2]);
      Inc(C, LongWord(A.Words[I + 6]) * B.Words[1]);
      Inc(C, LongWord(A.Words[I + 7]) * B.Words[0]);
      R.Words[I + 7] := Word(C);
    end;

  {$IFOPT Q+}
  C := C shr 16;
  Inc(C, LongWord(A.Words[9])  * B.Words[7]);
  Inc(C, LongWord(A.Words[10]) * B.Words[6]);
  Inc(C, LongWord(A.Words[11]) * B.Words[5]);
  Inc(C, LongWord(A.Words[12]) * B.Words[4]);
  Inc(C, LongWord(A.Words[13]) * B.Words[3]);
  Inc(C, LongWord(A.Words[14]) * B.Words[2]);
  Inc(C, LongWord(A.Words[15]) * B.Words[1]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[10]) * B.Words[7]);
  Inc(C, LongWord(A.Words[11]) * B.Words[6]);
  Inc(C, LongWord(A.Words[12]) * B.Words[5]);
  Inc(C, LongWord(A.Words[13]) * B.Words[4]);
  Inc(C, LongWord(A.Words[14]) * B.Words[3]);
  Inc(C, LongWord(A.Words[15]) * B.Words[2]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[11]) * B.Words[7]);
  Inc(C, LongWord(A.Words[12]) * B.Words[6]);
  Inc(C, LongWord(A.Words[13]) * B.Words[5]);
  Inc(C, LongWord(A.Words[14]) * B.Words[4]);
  Inc(C, LongWord(A.Words[15]) * B.Words[3]);
  if C > 0 then
    RaiseOverflowError;
    
  C := C shr 16;
  Inc(C, LongWord(A.Words[12]) * B.Words[7]);
  Inc(C, LongWord(A.Words[13]) * B.Words[6]);
  Inc(C, LongWord(A.Words[14]) * B.Words[5]);
  Inc(C, LongWord(A.Words[15]) * B.Words[4]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[13]) * B.Words[7]);
  Inc(C, LongWord(A.Words[14]) * B.Words[6]);
  Inc(C, LongWord(A.Words[15]) * B.Words[5]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[14]) * B.Words[7]);
  Inc(C, LongWord(A.Words[15]) * B.Words[6]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[15]) * B.Words[7]);
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}

  A := R;
end;

procedure Word256MultiplyWord256(const A, B: Word256; var R: Word512);
var C : Int64;
    I, L, N : Integer;
begin
  C := LongWord(A.Words[0]) * B.Words[0];
  R.Words[0] := Word(C);
  for L := 1 to 15 do
    begin
      C := C shr 16;
      for I := 0 to L do
        Inc(C, LongWord(A.Words[I]) * B.Words[L - I]);
      R.Words[L] := Word(C);
    end;
  for L := 16 to 31 do
    begin
      C := C shr 16;
      N := 32 - L;
      for I := 0 to N - 1 do
        Inc(C, LongWord(A.Words[16 - N + I]) * B.Words[15 - I]);
      R.Words[L] := Word(C);
    end;
end;

procedure Word256Sqr(const A: Word256; var R: Word512);
begin
  Word256MultiplyWord256(A, A, R);
end;

procedure Word256DivideWord128(const A: Word256; const B: Word128; var Q: Word256; var R: Word128);
var C : Integer;
    D : Word256;
    E : Word256;
begin
  // Handle special cases
  if Word128IsZero(B) then // B = 0
    RaiseDivByZeroError else
  if Word128IsOne(B) then // B = 1
    begin
      Q := A;
      Word128InitZero(R);
      exit;
    end;
  if Word256IsZero(A) then // A = 0
    begin
      Word256InitZero(Q);
      Word128InitZero(R);
      exit;
    end;
  C := Word256CompareWord128(A, B);
  if C < 0 then // A < B
    begin
      R := Word256ToWord128(A);
      Word256InitZero(Q);
      exit;
    end else
  if C = 0 then // A = B
    begin
      Word256InitOne(Q);
      Word128InitZero(R);
      exit;
    end;
  // Divide using "restoring radix two" division
  D := A;
  Word256InitZero(E); // remainder (129 bits)
  Word256InitZero(Q); // quotient (256 bits)
  for C := 0 to 255 do
    begin
      // Shift high bit of dividend D into low bit of remainder E
      Word256Shl1(E);
      if D.LongWords[7] and $80000000 <> 0 then
        E.LongWords[0] := E.LongWords[0] or 1;
      Word256Shl1(D);
      // Shift quotient
      Word256Shl1(Q);
      // Subtract divisor from remainder if large enough
      if Word256CompareWord128(E, B) >= 0 then
        begin
          Word256SubtractWord128(E, B);
          // Set result bit in quotient
          Q.LongWords[0] := Q.LongWords[0] or 1;
        end;
    end;
  Assert(E.LongWords[4] = 0); // remainder (128 bits)
  R := Word256ToWord128(E);
end;

function Word256Hash(const A: Word256): LongWord;
begin
  Result := Word32Hash(A.LongWords[0]) xor
            Word32Hash(A.LongWords[1]) xor
            Word32Hash(A.LongWords[2]) xor
            Word32Hash(A.LongWords[3]) xor
            Word32Hash(A.LongWords[4]) xor
            Word32Hash(A.LongWords[5]) xor
            Word32Hash(A.LongWords[6]) xor
            Word32Hash(A.LongWords[7]);
end;



{                                                                              }
{ Buf helper functions                                                         }
{                                                                              }
{$IFDEF DELPHI5_DOWN}
type
  PByte     = ^Byte;
  PWord     = ^Word;
  PLongWord = ^LongWord;
  PShortInt = ^ShortInt;
  PSmallInt = ^SmallInt;
  PLongInt  = ^LongInt;
  PInteger  = ^Integer;
  PInt64    = ^Int64;
{$ENDIF}

function BufIsZero(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
    I : LongWord;
begin
  Assert(Size mod 4 = 0);

  if Size = 0 then
    begin
      Result := True;
      exit;
    end;
  P := @Buf;
  for I := 0 to Size div 4 - 1 do
    if P^ <> 0 then
      begin
        Result := False;
        exit;
      end
    else
      Inc(P);
  Result := True;
end;

function BufIsValue(const Buf; const Size: LongWord; const Value: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
    I : LongWord;
begin
  Assert(Size mod 4 = 0);

  if Size = 0 then
    begin
      Result := True;
      exit;
    end;
  P := @Buf;
  for I := 0 to Size div 4 - 1 do
    if P^ <> Value then
      begin
        Result := False;
        exit;
      end
    else
      Inc(P);
  Result := True;
end;

procedure BufInitZero(var Buf; const Size: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
    I : LongWord;
    V : LongWord;
begin
  Assert(Size mod 4 = 0);

  if Size = 0 then
    exit;

  P := @Buf;
  V := 0;
  for I := 0 to Size div 4 - 1 do
    begin
      P^ := V;
      Inc(P);
    end;
end;

procedure BufInitValue(var Buf; const Size: LongWord; const Value: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
    I : LongWord;
begin
  Assert(Size mod 4 = 0);

  if Size = 0 then
    exit;

  P := @Buf;
  for I := 0 to Size div 4 - 1 do
    begin
      P^ := Value;
      Inc(P);
    end;
end;



{                                                                              }
{ WordBuf                                                                      }
{                                                                              }
procedure WordBufInitZero(var Buf; const Size: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
begin
  Assert(Size > 0);

  BufInitZero(Buf, Size);
end;

procedure WordBufInitOne(var Buf; const Size: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  P := @Buf;
  P^ := 1;
  Inc(P);
  BufInitZero(P^, Size - SizeOf(LongWord));
end;

procedure WordBufInitMinimum(var Buf; const Size: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
begin
  WordBufInitZero(Buf, Size);
end;

procedure WordBufInitMaximum(var Buf; const Size: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
begin
  BufInitValue(Buf, Size, $FFFFFFFF);
end;

procedure WordBufInitFloat(var BufA; const SizeA: LongWord; const B: Extended);
var D : Word128;
    I, L : Integer;
    V : LongWord;
    P : PLongWord;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  Word128InitFloat(D, B);
  // find size
  L := 0;
  for I := 3 downto 0 do
    begin
      V := D.LongWords[I];
      if V <> 0 then
        begin
          L := I + 1;
          break;
        end;
    end;
  // check range
  {$IFOPT R+}
  if LongWord(L) * SizeOf(LongWord) > SizeA then
    RaiseRangeError;
  {$ENDIF}
  // set value
  P := @BufA;
  for I := 0 to L - 1 do
    begin
      P^ := D.LongWords[I];
      Inc(P);
    end;
  BufInitZero(P^, SizeA - (LongWord(L) * SizeOf(LongWord)));
end;

function WordBufIsZero(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := BufIsZero(Buf, Size);
end;

function WordBufIsOne(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  P := @Buf;
  if P^ <> 1 then
    begin
      Result := False;
      exit;
    end;
  Inc(P);
  Result := BufIsZero(P^, Size - SizeOf(LongWord));
end;

function WordBufIsMinimum(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := WordBufIsZero(Buf, Size);
end;

function WordBufIsMaximum(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := BufIsValue(Buf, Size, $FFFFFFFF);
end;

function WordBufIsOdd(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  P := @Buf;
  Result := P^ and 1 = 1;
end;

function BufIsWordRange(const Buf; const Size: LongWord; const WordSize: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
    L : LongWord;
begin
  Assert(Size mod 4 = 0);
  Assert(WordSize mod 32 = 0);

  if Size * 8 <= WordSize then
    begin
      Result := True;
      exit;
    end;
  P := @Buf;
  L := WordSize div 32;
  Inc(P, L);
  Result := BufIsZero(P^, Size - L * 4);
end;

function WordBufIsWord32Range(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := BufIsWordRange(Buf, Size, 32);
end;

function WordBufIsWord64Range(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := BufIsWordRange(Buf, Size, 64);
end;

function WordBufIsWord128Range(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := BufIsWordRange(Buf, Size, 128);
end;

function WordBufIsWord256Range(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := BufIsWordRange(Buf, Size, 256);
end;

function BufIsIntRange(const Buf; const Size: LongWord; const WordSize: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
    L : LongWord;
begin
  Assert(Size mod 4 = 0);
  Assert(WordSize mod 32 = 0);

  L := WordSize div 32;
  if Size < L then
    begin
      Result := True;
      exit;
    end;
  P := @Buf;
  Inc(P, L - 1);
  if P^ and $80000000 <> 0 then
    begin
      Result := False;
      exit;
    end;
  Inc(P);
  Result := BufIsZero(P^, Size - L * 4);
end;

function WordBufIsInt32Range(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := BufIsIntRange(Buf, Size, 32);
end;

function WordBufIsInt64Range(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := BufIsIntRange(Buf, Size, 64);
end;

function WordBufIsInt128Range(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := BufIsIntRange(Buf, Size, 128);
end;

procedure WordBufInitWord32(var BufA; const SizeA: LongWord; const B: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  P := @BufA;
  P^ := 1;
  Inc(P);
  BufInitZero(P^, SizeA - SizeOf(LongWord));
end;

procedure WordBufInitWord64(var BufA; const SizeA: LongWord; const B: Word64); {$IFDEF UseInline}inline;{$ENDIF}
var P : PWord64;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  P := @BufA;
  P^ := B;
  Inc(P);
  BufInitZero(P^, SizeA - SizeOf(Word64));
end;

procedure WordBufInitWord128(var BufA; const SizeA: LongWord; const B: Word128); {$IFDEF UseInline}inline;{$ENDIF}
var P : PWord128;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  P := @BufA;
  P^ := B;
  Inc(P);
  BufInitZero(P^, SizeA - SizeOf(Word128));
end;

procedure WordBufInitInt32(var BufA; const SizeA: LongWord; const B: LongInt); {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongInt;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  P := @BufA;
  P^ := B;
  Inc(P);
  BufInitZero(P^, SizeA - SizeOf(LongInt));
end;

procedure WordBufInitInt64(var BufA; const SizeA: LongWord; const B: Int64); {$IFDEF UseInline}inline;{$ENDIF}
var P : PInt64;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  P := @BufA;
  P^ := B;
  Inc(P);
  BufInitZero(P^, SizeA - SizeOf(Int64));
end;

procedure WordBufInitInt128(var BufA; const SizeA: LongWord; const B: Int128); {$IFDEF UseInline}inline;{$ENDIF}
var P : PInt128;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  P := @BufA;
  P^ := B;
  Inc(P);
  BufInitZero(P^, SizeA - SizeOf(Int128));
end;

function WordBufToWord32(const Buf; const Size: LongWord): LongWord; {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  P := @Buf;
  Result := P^;
  {$IFOPT R+}
  Inc(P);
  if not BufIsZero(P^, Size - SizeOf(LongWord)) then
    RaiseRangeError;
  {$ENDIF}
end;

function WordBufToWord64(const Buf; const Size: LongWord): Word64; {$IFDEF UseInline}inline;{$ENDIF}
var P : PWord64;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  P := @Buf;
  Result := P^;
  {$IFOPT R+}
  Inc(P);
  if not BufIsZero(P^, Size - SizeOf(Word64)) then
    RaiseRangeError;
  {$ENDIF}
end;

function WordBufToWord128(const Buf; const Size: LongWord): Word128; {$IFDEF UseInline}inline;{$ENDIF}
var P : PWord128;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  P := @Buf;
  Result := P^;
  {$IFOPT R+}
  Inc(P);
  if not BufIsZero(P^, Size - SizeOf(Word128)) then
    RaiseRangeError;
  {$ENDIF}
end;

function WordBufToInt32(const Buf; const Size: LongWord): LongInt; {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongInt;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  P := @Buf;
  Result := P^;
  {$IFOPT R+}
  Inc(P);
  if not BufIsZero(P^, Size - SizeOf(LongInt)) then
    RaiseRangeError;
  {$ENDIF}
end;

function WordBufToInt64(const Buf; const Size: LongWord): Int64; {$IFDEF UseInline}inline;{$ENDIF}
var P : PInt64;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  P := @Buf;
  Result := P^;
  {$IFOPT R+}
  Inc(P);
  if not BufIsZero(P^, Size - SizeOf(Int64)) then
    RaiseRangeError;
  {$ENDIF}
end;

function WordBufToInt128(const Buf; const Size: LongWord): Int128; {$IFDEF UseInline}inline;{$ENDIF}
var P : PInt128;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  P := @Buf;
  Result := P^;
  {$IFOPT R+}
  Inc(P);
  if not BufIsZero(P^, Size - SizeOf(Int128)) then
    RaiseRangeError;
  {$ENDIF}
end;

function WordBufToFloat(const Buf; const Size: LongWord): Extended; {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
    I : LongWord;
    F : Extended;
const MaxF = 1.1E+4932 / 4294967296;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  P := @Buf;
  Result := 0.0;
  F := 1.0;
  for I := 0 to Size div 4 - 1 do
    begin
      Result := Result + P^ * F;
      {$IFOPT R+}
      if F = MaxF then
        RaiseOverflowError;
     {$ENDIF}
      F := F * 4294967296.0;
      Inc(P);
    end;
end;

function WordBufEqualsWord32(const BufA; const SizeA: LongWord; const B: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  P := @BufA;
  if P^ <> B then
    begin
      Result := False;
      exit;
    end;
  Inc(P);
  Result := BufIsZero(P^, SizeA - SizeOf(LongWord));
end;

function WordBufEqualsWord64(const BufA; const SizeA: LongWord; const B: Word64): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PWord64;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  P := @BufA;
  if not Word64EqualsWord64(P^, B) then
    begin
      Result := False;
      exit;
    end;
  Inc(P);
  Result := BufIsZero(P^, SizeA - SizeOf(Word64));
end;

function WordBufEqualsWord128(const BufA; const SizeA: LongWord; const B: Word128): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PWord128;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  P := @BufA;
  if not Word128EqualsWord128(P^, B) then
    begin
      Result := False;
      exit;
    end;
  Inc(P);
  Result := BufIsZero(P^, SizeA - SizeOf(Word128));
end;

function WordBufEqualsWordBuf(const BufA; const SizeA: LongWord; const BufB; const SizeB: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P, Q : PLongWord;
    L : LongWord;
    I, N : Integer;
begin
  Assert(SizeA > 0);
  Assert(SizeB > 0);
  Assert(SizeA mod 4 = 0);
  Assert(SizeB mod 4 = 0);

  if SizeA < SizeB then
    L := SizeA
  else
    L := SizeB;
  N := L div 4;
  P := @BufA;
  Q := @BufB;
  for I := 0 to N - 1 do
    if P^ <> Q^ then
      begin
        Result := False;
        exit;
      end
    else
      begin
        Inc(P);
        Inc(Q);
      end;
  if SizeA > SizeB then
    Result := BufIsZero(P^, SizeA - SizeB) else
  if SizeB > SizeA then 
    Result := BufIsZero(Q^, SizeB - SizeA)
  else
    Result := True;
end;

function WordBufEqualsInt32(const BufA; const SizeA: LongWord; const B: LongInt): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongInt;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  P := @BufA;
  if P^ <> B then
    begin
      Result := False;
      exit;
    end;
  Inc(P);
  Result := BufIsZero(P^, SizeA - SizeOf(LongWord));
end;

function WordBufEqualsInt64(const BufA; const SizeA: LongWord; const B: Int64): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PInt64;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  P := @BufA;
  if P^ <> B then
    begin
      Result := False;
      exit;
    end;
  Inc(P);
  Result := BufIsZero(P^, SizeA - SizeOf(Word64));
end;

function WordBufEqualsInt128(const BufA; const SizeA: LongWord; const B: Int128): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PInt128;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  P := @BufA;
  if not Int128EqualsInt128(P^, B) then
    begin
      Result := False;
      exit;
    end;
  Inc(P);
  Result := BufIsZero(P^, SizeA - SizeOf(Word128));
end;

function WordBufCompareWord32(const BufA; const SizeA: LongWord; const B: LongWord): Integer; {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
    C : LongWord;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  P := @BufA;
  C := P^;
  if C > B then
    Result := 1 else
  if C < B then
    begin
      Inc(P);
      if BufIsZero(P^, SizeA - SizeOf(LongWord)) then
        Result := -1
      else
        Result := 1;
    end
  else
    begin
      Inc(P);
      if BufIsZero(P^, SizeA - SizeOf(LongWord)) then
        Result := 0
      else
        Result := 1;
    end;
end;

function WordBufCompareWord64(const BufA; const SizeA: LongWord; const B: Word64): Integer; {$IFDEF UseInline}inline;{$ENDIF}
var P : PWord64;
    I : Integer;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  P := @BufA;
  I := Word64CompareWord64(P^, B);
  if I > 0 then
    Result := 1 else
  if I < 0 then
    begin
      Inc(P);
      if BufIsZero(P^, SizeA - SizeOf(Word64)) then
        Result := -1
      else
        Result := 1;
    end
  else
    begin
      Inc(P);
      if BufIsZero(P^, SizeA - SizeOf(Word64)) then
        Result := 0
      else
        Result := 1;
    end;
end;

function WordBufCompareWord128(const BufA; const SizeA: LongWord; const B: Word128): Integer; {$IFDEF UseInline}inline;{$ENDIF}
var P : PWord128;
    I : Integer;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  P := @BufA;
  I := Word128CompareWord128(P^, B);
  if I > 0 then
    Result := 1 else
  if I < 0 then
    begin
      Inc(P);
      if BufIsZero(P^, SizeA - SizeOf(Word128)) then
        Result := -1
      else
        Result := 1;
    end
  else
    begin
      Inc(P);
      if BufIsZero(P^, SizeA - SizeOf(Word128)) then
        Result := 0
      else
        Result := 1;
    end;
end;

function WordBufCompareWordBuf(const BufA; const SizeA: LongWord; const BufB; const SizeB: LongWord): Integer; {$IFDEF UseInline}inline;{$ENDIF}
var P, Q : PLongWord;
    C, D : LongWord;
    L : LongWord;
    I, N : Integer;
begin
  Assert(SizeA > 0);
  Assert(SizeB > 0);
  Assert(SizeA mod 4 = 0);
  Assert(SizeB mod 4 = 0);

  P := @BufA;
  Q := @BufB;
  if SizeA < SizeB then
    L := SizeA
  else
    L := SizeB;
  N := L div 4;
  Inc(P, N - 1);
  Inc(Q, N - 1);
  if SizeA > SizeB then
    begin
      if not BufIsZero(P^, SizeA - L) then
        begin
          Result := 1;
          exit;
        end
    end else
  if SizeA < SizeB then
    begin
      if not BufIsZero(Q^, SizeB - L) then
        begin
          Result := -1;
          exit;
        end;
    end;
  for I := 0 to N - 1 do
    begin
      C := P^;
      D := Q^;
      if C > D then
        begin
          Result := 1;
          exit;
        end else
      if C < D then
        begin
          Result := -1;
          exit;
        end;
      Dec(P);
      Dec(Q);
    end;
  Result := 0;
end;

function WordBufCompareInt32(const BufA; const SizeA: LongWord; const B: LongInt): Integer; {$IFDEF UseInline}inline;{$ENDIF}
begin
  if B < 0 then
    Result := 1
  else
    Result := WordBufCompareWord32(BufA, SizeA, LongWord(B));
end;

function WordBufCompareInt64(const BufA; const SizeA: LongWord; const B: Int64): Integer; {$IFDEF UseInline}inline;{$ENDIF}
begin
  if B < 0 then
    Result := 1
  else
    Result := WordBufCompareWord64(BufA, SizeA, Word64(B));
end;

function WordBufCompareInt128(const BufA; const SizeA: LongWord; const B: Int128): Integer; {$IFDEF UseInline}inline;{$ENDIF}
begin
  if Int128IsNegative(B) then
    Result := 1
  else
    Result := WordBufCompareWord128(BufA, SizeA, Word128(B));
end;

procedure WordBufNot(var Buf; const Size: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
    I : Integer;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  P := @Buf;
  for I := 0 to Size div 4 - 1 do
    begin
      P^ := not P^;
      Inc(P);
    end;
end;

procedure WordBufOrWordBuf(var BufA; const SizeA: LongWord; const BufB; const SizeB: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P, Q : PLongWord;
    L : LongWord;
    I : Integer;
begin
  Assert(SizeA > 0);
  Assert(SizeB > 0);
  Assert(SizeA mod 4 = 0);
  Assert(SizeB mod 4 = 0);

  if SizeA < SizeB then
    L := SizeA
  else
    L := SizeB;
  P := @BufA;
  Q := @BufB;
  for I := 0 to L div 4 - 1 do
    begin
      P^ := P^ or Q^;
      Inc(P);
      Inc(Q);
    end;
end;

procedure WordBufAndWordBuf(var BufA; const SizeA: LongWord; const BufB; const SizeB: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P, Q : PLongWord;
    L : LongWord;
    I : Integer;
begin
  Assert(SizeA > 0);
  Assert(SizeB > 0);
  Assert(SizeA mod 4 = 0);
  Assert(SizeB mod 4 = 0);

  if SizeA < SizeB then
    L := SizeA
  else
    L := SizeB;
  P := @BufA;
  Q := @BufB;
  for I := 0 to L div 4 - 1 do
    begin
      P^ := P^ and Q^;
      Inc(P);
      Inc(Q);
    end;
  if SizeA > SizeB then
    BufInitZero(P^, SizeA - SizeB);
end;

procedure WordBufXorWordBuf(var BufA; const SizeA: LongWord; const BufB; const SizeB: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P, Q : PLongWord;
    L : LongWord;
    I : Integer;
begin
  Assert(SizeA > 0);
  Assert(SizeB > 0);
  Assert(SizeA mod 4 = 0);
  Assert(SizeB mod 4 = 0);

  if SizeA < SizeB then
    L := SizeA
  else
    L := SizeB;
  P := @BufA;
  Q := @BufB;
  for I := 0 to L div 4 - 1 do
    begin
      P^ := P^ xor Q^;
      Inc(P);
      Inc(Q);
    end;
end;

function WordBufIsBitSet(const BufA; const SizeA: LongWord; const B: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  if B >= SizeA * 32 then
    Result := False
  else
    begin
      P := @BufA;
      Inc(P, B shr 5);
      Result := (P^ and (1 shl (B and $1F)) <> 0);
    end;
end;

procedure WordBufSetBit(var BufA; const SizeA: LongWord; const B: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  {$IFOPT Q+}
  if B >= SizeA * 32 then
    RaiseOverflowError
  else
  {$ENDIF}
    begin
      P := @BufA;
      Inc(P, B shr 5);
      P^ := P^ or (1 shl (B and $1F));
    end;
end;

procedure WordBufClearBit(var BufA; const SizeA: LongWord; const B: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  {$IFOPT Q+}
  if B >= SizeA * 32 then
    RaiseOverflowError
  else
  {$ENDIF}
    begin
      P := @BufA;
      Inc(P, B shr 5);
      P^ := P^ and not LongWord(1 shl (B and $1F));
    end;
end;

procedure WordBufToggleBit(var BufA; const SizeA: LongWord; const B: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  {$IFOPT Q+}
  if B >= SizeA * 32 then
    RaiseOverflowError
  else
  {$ENDIF}
    begin
      P := @BufA;
      Inc(P, B shr 5);
      P^ := P^ xor LongWord(1 shl (B and $1F));
    end;
end;

procedure WordBufShl1(var Buf; const Size: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P, Q : PLongWord;
    I, L : Integer;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  P := @Buf;
  L := Size div 4;
  Inc(P, L);
  {$IFOPT Q+}
  if P^ and $80000000 <> 0 then
    RaiseOverflowError;
  {$ENDIF}
  Q := P;
  Dec(Q);
  for I := L - 1 downto 1 do
    begin
      P^ := (P^ shl 1) or (Q^ shr 31);
      P := Q;
      Dec(Q);
    end;
  P^ := P^ shl 1;
end;

procedure WordBufShr1(var Buf; const Size: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P, Q : PLongWord;
    I, L : Integer;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  P := @Buf;
  L := Size div 4;
  Q := P;
  Inc(Q);
  for I := 0 to L - 2 do
    begin
      P^ := (P^ shr 1) or (Q^ shl 31);
      P := Q;
      Inc(Q);
    end;
  P^ := P^ shr 1;
end;

procedure WordBufRol1(const Buf; const Size: LongWord; var BufR); {$IFDEF UseInline}inline;{$ENDIF}
var P, Q, V : PLongWord;
    I, L : Integer;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);
  Assert(@Buf <> @BufR);

  P := @Buf;
  Q := @BufR;
  L := Size div 4;
  Inc(P, L - 1);
  Inc(Q, L - 1);
  for I := L - 1 downto 1 do
    begin
      V := P;
      Dec(V);
      Q^ := (P^ shl 1) or (V^ shr 31);
      Dec(P);
      Dec(Q);
    end;
  V := @Buf;
  Inc(P, L - 1);
  Q^ := (P^ shl 1) or (V^ shr 31);
end;

procedure WordBufRor1(const Buf; const Size: LongWord; var BufR); {$IFDEF UseInline}inline;{$ENDIF}
var P, Q, V : PLongWord;
    I, L : Integer;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);
  Assert(@Buf <> @BufR);

  P := @Buf;
  V := P;
  Q := @BufR;
  L := Size div 4;
  Inc(P, L - 1);
  Inc(Q, L - 1);

  Q^ := (P^ shr 1) or (V^ shl 31);
  for I := L - 2 downto 0 do
    begin
      V := P;
      Dec(P);
      Dec(Q);
      Q^ := (P^ shr 1) or (V^ shl 31);
    end;
end;

function WordBufAddWord32(var Buf; const Size: LongWord; const B: LongWord): LongWord; {$IFDEF UseInline}inline;{$ENDIF}
var L, I : Integer;
    R    : Int64;
    C    : LongWord;
    P    : PLongWord;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  if B = 0 then
    begin
      Result := 0;
      exit;
    end;
  P := @Buf;
  L := Size div 4;
  C := B;
  for I := 0 to L - 1 do
    begin
      R := C;
      Inc(R, P^);
      P^ := Int64Rec(R).LongWords[0];
      C := Int64Rec(R).LongWords[1];
      if C = 0 then
        begin
          Result := 0;
          exit;
        end;
      Inc(P);
    end;
  Result := C;
end;

function WordBufAddWord64(var Buf; const Size: LongWord; const B: Word64): LongWord; {$IFDEF UseInline}inline;{$ENDIF}
var P    : PLongWord;
    L, I : Integer;
    R    : Word128;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  if (B.LongWords[0] = 0) and
     (B.LongWords[1] = 0) then
    begin
      Result := 0;
      exit;
    end;
  P := @Buf;
  L := Size div 4;
  Word128InitWord64(R, B);
  for I := 0 to L - 1 do
    begin
      Word128AddWord32(R, P^);
      P^ := R.LongWords[0];
      R.LongWords[0] := R.LongWords[1];
      R.LongWords[1] := R.LongWords[2];
      R.LongWords[2] := R.LongWords[3];
      if (R.LongWords[0] = 0) and
         (R.LongWords[1] = 0) then
        begin
          Result := 0;
          exit;
        end;
      Inc(P);
    end;
  Result := R.LongWords[0];
end;

function WordBufAddWord128(var Buf; const Size: LongWord; const B: Word128): LongWord; {$IFDEF UseInline}inline;{$ENDIF}
var P    : PLongWord;
    L, I : Integer;
    R    : Word256;
begin
  Assert(Size > 0);
  Assert(Size mod 4 = 0);

  if (B.LongWords[0] = 0) and
     (B.LongWords[1] = 0) and
     (B.LongWords[2] = 0) and
     (B.LongWords[3] = 0) then
    begin
      Result := 0;
      exit;
    end;
  P := @Buf;
  L := Size div 4;
  Word256InitWord128(R, B);
  for I := 0 to L - 1 do
    begin
      Word256AddWord32(R, P^);
      P^ := R.LongWords[0];
      R.LongWords[0] := R.LongWords[1];
      R.LongWords[1] := R.LongWords[2];
      R.LongWords[2] := R.LongWords[3];
      R.LongWords[3] := R.LongWords[4];
      R.LongWords[4] := R.LongWords[5];
      if (R.LongWords[0] = 0) and
         (R.LongWords[1] = 0) and
         (R.LongWords[2] = 0) and
         (R.LongWords[3] = 0) then
        begin
          Result := 0;
          exit;
        end;
      Inc(P);
    end;
  Result := R.LongWords[0];
end;

function WordBufAddWordBuf(const BufA; const SizeA: LongWord; const BufB; const SizeB: LongWord; var BufR): LongWord; {$IFDEF UseInline}inline;{$ENDIF}
var P, Q : PLongWord;
    L, M : Integer;
    I    : Integer;
    R    : Int64;
    C    : LongWord;
begin
  Assert(SizeA > 0);
  Assert(SizeB > 0);
  Assert(SizeA mod 4 = 0);
  Assert(SizeB mod 4 = 0);

  L := SizeA div 4;
  M := SizeB div 4;
  P := @BufA;
  Q := @BufB;
  C := 0;
  for I := 0 to M - 1 do
    begin
      R := C;
      Inc(R, P^);
      Inc(R, Q^);
      P^ := Int64Rec(R).LongWords[0];
      C := Int64Rec(R).LongWords[1];
      Inc(P);
      Inc(Q);
    end;
  if C = 0 then
    begin
      Result := 0;
      exit;
    end;
  for I := M to L - 1 do
    begin
      R := C;
      Inc(R, P^);
      P^ := Int64Rec(R).LongWords[0];
      C := Int64Rec(R).LongWords[1];
      if C = 0 then
        begin
          Result := 0;
          exit;
        end;
      Inc(P);
    end;
  Result := C;
end;

function WordBufSubtractWord32(var BufA; const SizeA: LongWord; const B: LongWord): Integer; {$IFDEF UseInline}inline;{$ENDIF}
var P    : PLongWord;
    C    : Integer;
    L, I : Integer;
    R    : Int64;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  // Handle B = 0
  if B = 0 then
    begin
      Result := 1;
      exit;
    end;
  // Handle A = B
  C := WordBufCompareWord32(BufA, SizeA, B);
  if C = 0 then
    begin
      BufInitZero(BufA, SizeA);
      Result := 0;
      exit;
    end;
  // Handle A < B
  if C < 0 then
    begin
      P := @BufA;
      P^ := B - P^;
      Inc(P);
      BufInitZero(P^, SizeA - 4);
      Result := -1;
      exit;
    end;
  // Handle A > B
  // Subtract
  P := @BufA;
  L := SizeA div 4;
  for I := 0 to L - 1 do
    begin
      if I = 0 then
        begin
          R := $100000000;
          Dec(R, B);
        end
      else
        R := $FFFFFFFF;
      Inc(R, P^);
      P^ := Int64Rec(R).LongWords[0];
      if Int64Rec(R).LongWords[1] > 0 then
        break;
      Inc(P);
    end;
  // Return sign
  Result := 1;
end;

function WordBufSubtractWord64(var BufA; const SizeA: LongWord; const B: Word64): Integer; {$IFDEF UseInline}inline;{$ENDIF}
var P    : PLongWord;
    C    : Integer;
    L, I : Integer;
    R    : Word128;
    D, E : Word64;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  // Handle B = 0
  if (B.LongWords[0] = 0) and
     (B.LongWords[1] = 0) then
    begin
      Result := 1;
      exit;
    end;
  // Handle A = B
  C := WordBufCompareWord64(BufA, SizeA, B);
  if C = 0 then
    begin
      BufInitZero(BufA, SizeA);
      Result := 0;
      exit;
    end;
  // Handle A < B
  if C < 0 then
    begin
      D := B;
      E := WordBufToWord64(BufA, SizeA);
      Word64SubtractWord64(D, E);
      WordBufInitWord64(BufA, SizeA, D);
      Result := -1;
      exit;
    end;
  // Handle A > B
  // Subtract
  P := @BufA;
  L := SizeA div 4;
  for I := 0 to L - 1 do
    begin
      if I = 0 then
        begin
          Word128InitZero(R);
          R.LongWords[2] := 1;
          Word128SubtractWord64(R, B);
        end
      else
        begin
          R.LongWords[0] := R.LongWords[1];
          R.LongWords[1] := $FFFFFFFF;
        end;
      Word128AddWord32(R, P^);
      P^ := R.LongWords[0];
      if R.LongWords[2] > 0 then
        break;
      Inc(P);
    end;
  // Return sign
  Result := 1;
end;

function WordBufSubtractWord128(var BufA; const SizeA: LongWord; const B: Word128): Integer; {$IFDEF UseInline}inline;{$ENDIF}
var P    : PLongWord;
    C    : Integer;
    L, I : Integer;
    R    : Word256;
    D, E : Word128;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  // Handle B = 0
  if Word128IsZero(B) then
    begin
      Result := 1;
      exit;
    end;
  // Handle A = B
  C := WordBufCompareWord128(BufA, SizeA, B);
  if C = 0 then
    begin
      BufInitZero(BufA, SizeA);
      Result := 0;
      exit;
    end;
  // Handle A < B
  if C < 0 then
    begin
      D := B;
      E := WordBufToWord128(BufA, SizeA);
      Word128SubtractWord128(D, E);
      WordBufInitWord128(BufA, SizeA, D);
      Result := -1;
      exit;
    end;
  // Handle A > B
  // Subtract
  P := @BufA;
  L := SizeA div 4;
  for I := 0 to L - 1 do
    begin
      if I = 0 then
        begin
          Word256InitZero(R);
          R.LongWords[4] := 1;
          Word256SubtractWord128(R, B);
        end
      else
        begin
          R.LongWords[0] := R.LongWords[1];
          R.LongWords[1] := R.LongWords[2];
          R.LongWords[2] := R.LongWords[3];
          R.LongWords[3] := $FFFFFFFF;
        end;
      Word256AddWord32(R, P^);
      P^ := R.LongWords[0];
      if R.LongWords[4] > 0 then
        break;
      Inc(P);
    end;
  // Return sign
  Result := 1;
end;

function WordBufSubtractWordBuf(var BufA; const SizeA: LongWord; const BufB; const SizeB: LongWord): Integer; {$IFDEF UseInline}inline;{$ENDIF}
var P       : PLongWord;
    C       : Integer;
    D, E    : PLongWord;
    L, M, N : Integer;
    R       : Int64;
    T       : LongWord;
    I       : Integer;
    F       : Boolean;
begin
  Assert(SizeA > 0);
  Assert(SizeB > 0);
  Assert(SizeA mod 4 = 0);
  Assert(SizeB mod 4 = 0);

  // Handle A = B
  C := WordBufCompareWordBuf(BufA, SizeA, BufB, SizeB);
  if C = 0 then
    begin
      BufInitZero(BufA, SizeA);
      Result := 0;
      exit;
    end;
  // Swap around if A < B
  if C > 0 then
    begin
      D := @BufA;
      E := @BufB;
      L := SizeA div 4;
      M := SizeB div 4;
    end
  else
    begin
      D := @BufB;
      E := @BufA;
      L := SizeB div 4;
      M := SizeA div 4;
    end;
  // Subtract
  P := @BufA;
  F := False;
  for I := 0 to M - 1 do
    begin
      if F then
        R := $FFFFFFFF
      else
        R := $100000000;
      Inc(R, D^);
      Dec(R, E^);
      P^ := Int64Rec(R).LongWords[0];
      F := Int64Rec(R).LongWords[1] = 0;
      Inc(P);
      Inc(D);
      Inc(E);
    end;
  if F then
    begin
      N := SizeA div 4;
      for I := M to L - 1 do
        begin
          R := $FFFFFFFF;
          Inc(R, D^);
          T := Int64Rec(R).LongWords[0];
          if I < N then
            P^ := T
          {$IFOPT Q+}
          else
            if T <> 0 then
              RaiseOverflowError
          {$ENDIF};
          if Int64Rec(R).LongWords[1] > 0 then
            break;
          Inc(P);
          Inc(D);
        end;
    end;
  if C > 0 then
    Result := 1
  else
    Result := -1;
end;

function WordBufMultiplyWord8(var BufA; const SizeA: LongWord; const B: Byte): LongWord; {$IFDEF UseInline}inline;{$ENDIF}
var P    : PLongWord;
    I, L : Integer;
    C    : Int64;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  if B = 0 then
    begin
      BufInitZero(BufA, SizeA);
      Result := 0;
      exit;
    end;
  if B = 1 then
    begin
      Result := 0;
      exit;
    end;

  P := @BufA;
  C := P^ * B;
  P^ := Int64Rec(C).LongWords[0];
  L := SizeA div 4;
  for I := 1 to L - 1 do
    begin
      Inc(P);
      C := C shr 32;
      C := C + (P^ * B);
      P^ := Int64Rec(C).LongWords[0];
    end;
  C := C shr 32;
  Result := Int64Rec(C).LongWords[0];
end;

function WordBufMultiplyWord16(var BufA; const SizeA: LongWord; const B: Word): LongWord; {$IFDEF UseInline}inline;{$ENDIF}
var P    : PLongWord;
    I, L : Integer;
    C    : Int64;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  if B = 0 then
    begin
      Result := 0;
      BufInitZero(BufA, SizeA);
      exit;
    end;
  if B = 1 then
    begin
      Result := 0;
      exit;
    end;

  P := @BufA;
  C := P^ * B;
  P^ := Int64Rec(C).LongWords[0];
  L := SizeA div 4;
  for I := 1 to L - 1 do
    begin
      Inc(P);
      C := C shr 32;
      C := C + (P^ * B);
      P^ := Int64Rec(C).LongWords[0];
    end;
  C := C shr 32;
  Result := Int64Rec(C).LongWords[0];
end;

function WordBufMultiplyWord32(var BufA; const SizeA: LongWord; const B: LongWord): LongWord; {$IFDEF UseInline}inline;{$ENDIF}
var P    : PLongWord;
    I, L : Integer;
    C, D : Word64;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  if B = 0 then
    begin
      Result := 0;
      BufInitZero(BufA, SizeA);
      exit;
    end;
  if B = 1 then
    begin
      Result := 0;
      exit;
    end;

  P := @BufA;
  Word64InitWord32(C, P^);
  Word64MultiplyWord32(C, B);
  P^ := C.LongWords[0];
  L := SizeA div 4;
  for I := 1 to L - 1 do
    begin
      Inc(P);
      C.LongWords[0] := C.LongWords[1];
      C.LongWords[1] := 0;
      Word64InitWord32(D, P^);
      Word64MultiplyWord32(D, B);
      Word64AddWord64(C, D);
      P^ := Int64Rec(C).LongWords[0];
    end;
  C.LongWords[0] := C.LongWords[1];
  C.LongWords[1] := 0;
  Result := C.LongWords[0];
end;

function WordBufMultiplyWord64(var BufA; const SizeA: LongWord; const B: Word64): Word64; {$IFDEF UseInline}inline;{$ENDIF}
var P    : PLongWord;
    I, L : Integer;
    C, D : Word128;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  if Word64IsZero(B) then
    begin
      Word64InitZero(Result);
      BufInitZero(BufA, SizeA);
      exit;
    end;
  if Word64IsOne(B) then
    begin
      Word64InitZero(Result);
      exit;
    end;

  P := @BufA;
  Word128InitWord32(C, P^);
  Word128MultiplyWord64(C, B);
  P^ := C.LongWords[0];
  L := SizeA div 4;
  for I := 1 to L - 1 do
    begin
      Inc(P);
      C.LongWords[0] := C.LongWords[1];
      C.LongWords[1] := C.LongWords[2];
      C.LongWords[2] := 0;
      Word128InitWord32(D, P^);
      Word128MultiplyWord64(D, B);
      Word128AddWord128(C, D);
      P^ := C.LongWords[0];
    end;
  C.LongWords[0] := C.LongWords[1];
  C.LongWords[1] := C.LongWords[2];
  C.LongWords[2] := 0;
  Result.LongWords[0] := C.LongWords[0];
  Result.LongWords[1] := C.LongWords[1];
end;

function WordBufMultiplyWord128(var BufA; const SizeA: LongWord; const B: Word128): Word128; {$IFDEF UseInline}inline;{$ENDIF}
var P    : PLongWord;
    I, L : Integer;
    C, D : Word256;
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  if Word128IsZero(B) then
    begin
      Word128InitZero(Result);
      BufInitZero(BufA, SizeA);
      exit;
    end;
  if Word128IsOne(B) then
    begin
      Word128InitZero(Result);
      exit;
    end;

  P := @BufA;
  Word256InitWord32(C, P^);
  Word256MultiplyWord128(C, B);
  P^ := C.LongWords[0];
  L := SizeA div 4;
  for I := 1 to L - 1 do
    begin
      Inc(P);
      C.LongWords[0] := C.LongWords[1];
      C.LongWords[1] := C.LongWords[2];
      C.LongWords[2] := C.LongWords[3];
      C.LongWords[3] := C.LongWords[4];
      C.LongWords[4] := 0;
      Word256InitWord32(D, P^);
      Word256MultiplyWord128(D, B);
      Word256AddWord256(C, D);
      P^ := C.LongWords[0];
    end;
  C.LongWords[0] := C.LongWords[1];
  C.LongWords[1] := C.LongWords[2];
  C.LongWords[2] := C.LongWords[3];
  C.LongWords[3] := C.LongWords[4];
  C.LongWords[4] := 0;
  Result.LongWords[0] := C.LongWords[0];
  Result.LongWords[1] := C.LongWords[1];
  Result.LongWords[2] := C.LongWords[2];
  Result.LongWords[3] := C.LongWords[3];
end;

{ SizeR = SizeA + SizeB }
procedure WordBufMultiplyWordBufLong(const BufA; const SizeA: LongWord; const BufB; const SizeB: LongWord; var BufR); {$IFDEF UseInline}inline;{$ENDIF}
var P, Q : PLongWord;
    T    : PLongWord;
    L, M : Integer;
    I, J : Integer;
    C    : LongWord;
    R, V : Int64;
begin
  Assert(SizeA > 0);
  Assert(SizeB > 0);
  Assert(SizeA mod 4 = 0);
  Assert(SizeB mod 4 = 0);

  L := SizeA div 4;
  M := SizeB div 4;
  Q := @BufB;
  for I := 0 to M - 1 do
    begin
      V := Q^;
      C := 0;
      P := @BufA;
      for J := 0 to L - 1 do
        begin
          R := C;
          T := @BufR;
          Inc(T, I + J);
          Inc(R, T^);
          Inc(R, V * P^);
          T^ := Int64Rec(R).LongWords[0];
          C := Int64Rec(R).LongWords[1];
          Inc(P);
        end;
      T := @BufR;
      Inc(T, I + L);
      T^ := C;
      Inc(Q);
    end;
end;

procedure WordBufMultiplyWordBuf(const BufA; const SizeA: LongWord; const BufB; const SizeB: LongWord; var BufR);
begin
  Assert(SizeA > 0);
  Assert(SizeB > 0);
  Assert(SizeA mod 4 = 0);
  Assert(SizeB mod 4 = 0);

  if WordBufIsZero(BufA, SizeA) or
     WordBufIsZero(BufB, SizeB) then
    begin
      WordBufInitZero(BufR, SizeA + SizeB);
      exit;
    end;
  if WordBufIsOne(BufA, SizeA) then
    begin
      WordBufInitZero(BufR, SizeA + SizeB);
      Move(BufB, BufR, SizeB);
      exit;
    end;
  if WordBufIsOne(BufB, SizeB) then
    begin
      WordBufInitZero(BufR, SizeA + SizeB);
      Move(BufA, BufR, SizeA);
      exit;
    end;

  WordBufMultiplyWordBufLong(BufA, SizeA, BufB, SizeB, BufR);
end;

{ SizeR = SizeA * 2 }
procedure WordBufSqr(const BufA; const SizeA: LongWord; var BufR);
begin
  Assert(SizeA > 0);
  Assert(SizeA mod 4 = 0);

  WordBufMultiplyWordBuf(BufA, SizeA, BufA, SizeA, BufR);
end;


{                                                                              }
{ Word512                                                                      }
{                                                                              }
procedure Word512InitZero(var A: Word512);
begin
  WordBufInitZero(A, Word512Size);
end;

procedure Word512InitOne(var A: Word512);
begin
  WordBufInitOne(A, Word512Size);
end;

procedure Word512InitMinimum(var A: Word512);
begin
  WordBufInitMinimum(A, Word512Size);
end;

procedure Word512InitMaximum(var A: Word512);
begin
  WordBufInitMaximum(A, Word512Size);
end;

function Word512IsZero(const A: Word512): Boolean;
begin
  Result := WordBufIsZero(A, Word512Size);
end;

function Word512IsOne(const A: Word512): Boolean;
begin
  Result := WordBufIsOne(A, Word512Size);
end;

function Word512IsMinimum(const A: Word512): Boolean;
begin
  Result := WordBufIsMinimum(A, Word512Size);
end;

function Word512IsMaximum(const A: Word512): Boolean;
begin
  Result := WordBufIsMaximum(A, Word512Size);
end;

function Word512IsOdd(const A: Word512): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := WordBufIsOdd(A, Word512Size);
end;

function Word512IsWord32Range(const A: Word512): Boolean;
begin
  Result := WordBufIsWord32Range(A, Word512Size);
end;

function Word512IsWord64Range(const A: Word512): Boolean;
begin
  Result := WordBufIsWord64Range(A, Word512Size);
end;

function Word512IsWord128Range(const A: Word512): Boolean;
begin
  Result := WordBufIsWord128Range(A, Word512Size);
end;

function Word512IsInt32Range(const A: Word512): Boolean;
begin
  Result := WordBufIsInt32Range(A, Word512Size);
end;

function Word512IsInt64Range(const A: Word512): Boolean;
begin
  Result := WordBufIsInt64Range(A, Word512Size);
end;

function Word512IsInt128Range(const A: Word512): Boolean;
begin
  Result := WordBufIsInt128Range(A, Word512Size);
end;

procedure Word512InitWord32(var A: Word512; const B: LongWord);
begin
  WordBufInitWord32(A, Word512Size, B);
end;

procedure Word512InitWord64(var A: Word512; const B: Word64);
begin
  WordBufInitWord64(A, Word512Size, B);
end;

procedure Word512InitWord128(var A: Word512; const B: Word128);
begin
  WordBufInitWord128(A, Word512Size, B);
end;

procedure Word512InitInt32(var A: Word512; const B: LongInt);
begin
  WordBufInitInt32(A, Word512Size, B);
end;

procedure Word512InitInt64(var A: Word512; const B: Int64);
begin
  WordBufInitInt64(A, Word512Size, B);
end;

procedure Word512InitInt128(var A: Word512; const B: Int128);
begin
  WordBufInitInt128(A, Word512Size, B);
end;

function Word512ToWord32(const A: Word512): LongWord;
begin
  Result := WordBufToWord32(A, Word512Size);
end;

function Word512ToWord64(const A: Word512): Word64;
begin
  Result := WordBufToWord64(A, Word512Size);
end;

function Word512ToWord128(const A: Word512): Word128;
begin
  Result := WordBufToWord128(A, Word512Size);
end;

function Word512ToInt32(const A: Word512): LongInt;
begin
  Result := WordBufToInt32(A, Word512Size);
end;

function Word512ToInt64(const A: Word512): Int64;
begin
  Result := WordBufToInt64(A, Word512Size);
end;

function Word512ToInt128(const A: Word512): Int128;
begin
  Result := WordBufToInt128(A, Word512Size);
end;

function Word512ToFloat(const A: Word512): Extended;
begin
  Result := WordBufToFloat(A, Word512Size);
end;

function Word512Lo(const A: Word512): Word256;
begin
  Result := A.Word256s[0];
end;

function Word512Hi(const A: Word512): Word256;
begin
  Result := A.Word256s[1];
end;

function Word512EqualsWord32(const A: Word512; const B: LongWord): Boolean;
begin
  Result := WordBufEqualsWord32(A, Word512Size, B);
end;

function  Word512EqualsWord64(const A: Word512; const B: Word64): Boolean;
begin
  Result := WordBufEqualsWord64(A, Word512Size, B);
end;

function  Word512EqualsWord128(const A: Word512; const B: Word128): Boolean;
begin
  Result := WordBufEqualsWord128(A, Word512Size, B);
end;

function  Word512EqualsWord512(const A, B: Word512): Boolean;
begin
  Result := WordBufEqualsWordBuf(A, Word512Size, B, Word512Size);
end;

function Word512EqualsInt32(const A: Word512; const B: LongInt): Boolean;
begin
  Result := WordBufEqualsInt32(A, Word512Size, B);
end;

function Word512EqualsInt64(const A: Word512; const B: Int64): Boolean;
begin
  Result := WordBufEqualsInt64(A, Word512Size, B);
end;

function Word512EqualsInt128(const A: Word512; const B: Int128): Boolean;
begin
  Result := WordBufEqualsInt128(A, Word512Size, B);
end;

function Word512CompareWord32(const A: Word512; const B: LongWord): Integer;
begin
  Result := WordBufCompareWord32(A, Word512Size, B);
end;

function Word512CompareWord64(const A: Word512; const B: Word64): Integer;
begin
  Result := WordBufCompareWord64(A, Word512Size, B);
end;

function Word512CompareWord128(const A: Word512; const B: Word128): Integer;
begin
  Result := WordBufCompareWord128(A, Word512Size, B);
end;

function Word512CompareWord512(const A, B: Word512): Integer;
begin
  Result := WordBufCompareWordBuf(A, Word512Size, B, Word512Size);
end;

function Word512CompareInt32(const A: Word512; const B: LongInt): Integer;
begin
  Result := WordBufCompareInt32(A, Word512Size, B);
end;

function Word512CompareInt64(const A: Word512; const B: Int64): Integer;
begin
  Result := WordBufCompareInt64(A, Word512Size, B);
end;

function Word512CompareInt128(const A: Word512; const B: Int128): Integer;
begin
  Result := WordBufCompareInt128(A, Word512Size, B);
end;

function Word512Min(const A, B: Word512): Word512;
begin
  if WordBufCompareWordBuf(A, Word512Size, B, Word512Size) <= 0 then
    Result := A
  else
    Result := B;
end;

function Word512Max(const A, B: Word512): Word512;
begin
  if WordBufCompareWordBuf(A, Word512Size, B, Word512Size) >= 0 then
    Result := A
  else
    Result := B;
end;

procedure Word512Not(var A: Word512);
begin
  WordBufNot(A, Word512Size);
end;

procedure Word512OrWord512(var A: Word512; const B: Word512);
begin
  WordBufOrWordBuf(A, Word512Size, B, Word512Size);
end;

procedure Word512AndWord512(var A: Word512; const B: Word512);
begin
  WordBufAndWordBuf(A, Word512Size, B, Word512Size);
end;

procedure Word512XorWord512(var A: Word512; const B: Word512);
begin
  WordBufXorWordBuf(A, Word512Size, B, Word512Size);
end;

function Word512IsBitSet(const A: Word512; const B: Integer): Boolean;
begin
  Result := WordBufIsBitSet(A, Word512Size, B);
end;

procedure Word512SetBit(var A: Word512; const B: Integer);
begin
  WordBufSetBit(A, Word512Size, B);
end;

procedure Word512ClearBit(var A: Word512; const B: Integer);
begin
  WordBufClearBit(A, Word512Size, B);
end;

procedure Word512ToggleBit(var A: Word512; const B: Integer);
begin
  WordBufToggleBit(A, Word512Size, B);
end;

procedure Word512Shl1(var A: Word512);
begin
  WordBufShl1(A, Word512Size);
end;

procedure Word512Shr1(var A: Word512);
begin
  WordBufShr1(A, Word512Size);
end;

procedure Word512Rol1(var A: Word512);
var R : Word512;
begin
  WordBufRol1(A, Word512Size, R);
  A := R;
end;

procedure Word512Ror1(var A: Word512);
var R : Word512;
begin
  WordBufRor1(A, Word512Size, R);
  A := R;
end;

procedure Word512Swap(var A, B: Word512);
var C : Word512;
begin
  C := A;
  A := B;
  B := C;
end;

procedure Word512AddWord32(var A: Word512; const B: LongWord);
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWord32(A, Word512Size, B)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
end;

procedure Word512AddWord64(var A: Word512; const B: Word64);
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWord64(A, Word512Size, B)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
end;

procedure Word512AddWord128(var A: Word512; const B: Word128);
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWord128(A, Word512Size, B)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
end;

procedure Word512AddWord256(var A: Word512; const B: Word256);
var R : Word512;
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWordBuf(A, Word512Size, B, Word256Size, R)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
  A := R;
end;

procedure Word512AddWord512(var A: Word512; const B: Word512);
var R : Word512;
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWordBuf(A, Word512Size, B, Word512Size, R)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
  A := R;
end;



{                                                                              }
{ Word1024                                                                     }
{                                                                              }
procedure Word1024InitZero(var A: Word1024);
begin
  WordBufInitZero(A, Word1024Size);
end;

procedure Word1024InitOne(var A: Word1024);
begin
  WordBufInitOne(A, Word1024Size);
end;

procedure Word1024InitMinimum(var A: Word1024);
begin
  WordBufInitMinimum(A, Word1024Size);
end;

procedure Word1024InitMaximum(var A: Word1024);
begin
  WordBufInitMaximum(A, Word1024Size);
end;

function Word1024IsZero(const A: Word1024): Boolean;
begin
  Result := WordBufIsZero(A, Word1024Size);
end;

function Word1024IsOne(const A: Word1024): Boolean;
begin
  Result := WordBufIsOne(A, Word1024Size);
end;

function Word1024IsMinimum(const A: Word1024): Boolean;
begin
  Result := WordBufIsMinimum(A, Word1024Size);
end;

function Word1024IsMaximum(const A: Word1024): Boolean;
begin
  Result := WordBufIsMaximum(A, Word1024Size);
end;

function Word1024IsOdd(const A: Word1024): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := WordBufIsOdd(A, Word1024Size);
end;

function Word1024IsWord32Range(const A: Word1024): Boolean;
begin
  Result := WordBufIsWord32Range(A, Word1024Size);
end;

function Word1024IsWord64Range(const A: Word1024): Boolean;
begin
  Result := WordBufIsWord64Range(A, Word1024Size);
end;

function Word1024IsWord128Range(const A: Word1024): Boolean;
begin
  Result := WordBufIsWord128Range(A, Word1024Size);
end;

function Word1024IsInt32Range(const A: Word1024): Boolean;
begin
  Result := WordBufIsInt32Range(A, Word1024Size);
end;

function Word1024IsInt64Range(const A: Word1024): Boolean;
begin
  Result := WordBufIsInt64Range(A, Word1024Size);
end;

function Word1024IsInt128Range(const A: Word1024): Boolean;
begin
  Result := WordBufIsInt128Range(A, Word1024Size);
end;

procedure Word1024InitWord32(var A: Word1024; const B: LongWord);
begin
  WordBufInitWord32(A, Word1024Size, B);
end;

procedure Word1024InitWord64(var A: Word1024; const B: Word64);
begin
  WordBufInitWord64(A, Word1024Size, B);
end;

procedure Word1024InitWord128(var A: Word1024; const B: Word128);
begin
  WordBufInitWord128(A, Word1024Size, B);
end;

procedure Word1024InitInt32(var A: Word1024; const B: LongInt);
begin
  WordBufInitInt32(A, Word1024Size, B);
end;

procedure Word1024InitInt64(var A: Word1024; const B: Int64);
begin
  WordBufInitInt64(A, Word1024Size, B);
end;

procedure Word1024InitInt128(var A: Word1024; const B: Int128);
begin
  WordBufInitInt128(A, Word1024Size, B);
end;

function Word1024ToWord32(const A: Word1024): LongWord;
begin
  Result := WordBufToWord32(A, Word1024Size);
end;

function Word1024ToWord64(const A: Word1024): Word64;
begin
  Result := WordBufToWord64(A, Word1024Size);
end;

function Word1024ToWord128(const A: Word1024): Word128;
begin
  Result := WordBufToWord128(A, Word1024Size);
end;

function Word1024ToInt32(const A: Word1024): LongInt;
begin
  Result := WordBufToInt32(A, Word1024Size);
end;

function Word1024ToInt64(const A: Word1024): Int64;
begin
  Result := WordBufToInt64(A, Word1024Size);
end;

function Word1024ToInt128(const A: Word1024): Int128;
begin
  Result := WordBufToInt128(A, Word1024Size);
end;

function Word1024ToFloat(const A: Word1024): Extended;
begin
  Result := WordBufToFloat(A, Word1024Size);
end;

function Word1024Lo(const A: Word1024): Word256;
begin
  Result := A.Word256s[0];
end;

function Word1024Hi(const A: Word1024): Word256;
begin
  Result := A.Word256s[1];
end;

function Word1024EqualsWord32(const A: Word1024; const B: LongWord): Boolean;
begin
  Result := WordBufEqualsWord32(A, Word1024Size, B);
end;

function  Word1024EqualsWord64(const A: Word1024; const B: Word64): Boolean;
begin
  Result := WordBufEqualsWord64(A, Word1024Size, B);
end;

function  Word1024EqualsWord128(const A: Word1024; const B: Word128): Boolean;
begin
  Result := WordBufEqualsWord128(A, Word1024Size, B);
end;

function  Word1024EqualsWord1024(const A, B: Word1024): Boolean;
begin
  Result := WordBufEqualsWordBuf(A, Word1024Size, B, Word1024Size);
end;

function Word1024EqualsInt32(const A: Word1024; const B: LongInt): Boolean;
begin
  Result := WordBufEqualsInt32(A, Word1024Size, B);
end;

function Word1024EqualsInt64(const A: Word1024; const B: Int64): Boolean;
begin
  Result := WordBufEqualsInt64(A, Word1024Size, B);
end;

function Word1024EqualsInt128(const A: Word1024; const B: Int128): Boolean;
begin
  Result := WordBufEqualsInt128(A, Word1024Size, B);
end;

function Word1024CompareWord32(const A: Word1024; const B: LongWord): Integer;
begin
  Result := WordBufCompareWord32(A, Word1024Size, B);
end;

function Word1024CompareWord64(const A: Word1024; const B: Word64): Integer;
begin
  Result := WordBufCompareWord64(A, Word1024Size, B);
end;

function Word1024CompareWord128(const A: Word1024; const B: Word128): Integer;
begin
  Result := WordBufCompareWord128(A, Word1024Size, B);
end;

function Word1024CompareWord1024(const A, B: Word1024): Integer;
begin
  Result := WordBufCompareWordBuf(A, Word1024Size, B, Word1024Size);
end;

function Word1024CompareInt32(const A: Word1024; const B: LongInt): Integer;
begin
  Result := WordBufCompareInt32(A, Word1024Size, B);
end;

function Word1024CompareInt64(const A: Word1024; const B: Int64): Integer;
begin
  Result := WordBufCompareInt64(A, Word1024Size, B);
end;

function Word1024CompareInt128(const A: Word1024; const B: Int128): Integer;
begin
  Result := WordBufCompareInt128(A, Word1024Size, B);
end;

function Word1024Min(const A, B: Word1024): Word1024;
begin
  if WordBufCompareWordBuf(A, Word1024Size, B, Word1024Size) <= 0 then
    Result := A
  else
    Result := B;
end;

function Word1024Max(const A, B: Word1024): Word1024;
begin
  if WordBufCompareWordBuf(A, Word1024Size, B, Word1024Size) >= 0 then
    Result := A
  else
    Result := B;
end;

procedure Word1024Not(var A: Word1024);
begin
  WordBufNot(A, Word1024Size);
end;

procedure Word1024OrWord1024(var A: Word1024; const B: Word1024);
begin
  WordBufOrWordBuf(A, Word1024Size, B, Word1024Size);
end;

procedure Word1024AndWord1024(var A: Word1024; const B: Word1024);
begin
  WordBufAndWordBuf(A, Word1024Size, B, Word1024Size);
end;

procedure Word1024XorWord1024(var A: Word1024; const B: Word1024);
begin
  WordBufXorWordBuf(A, Word1024Size, B, Word1024Size);
end;

function Word1024IsBitSet(const A: Word1024; const B: Integer): Boolean;
begin
  Result := WordBufIsBitSet(A, Word1024Size, B);
end;

procedure Word1024SetBit(var A: Word1024; const B: Integer);
begin
  WordBufSetBit(A, Word1024Size, B);
end;

procedure Word1024ClearBit(var A: Word1024; const B: Integer);
begin
  WordBufClearBit(A, Word1024Size, B);
end;

procedure Word1024ToggleBit(var A: Word1024; const B: Integer);
begin
  WordBufToggleBit(A, Word1024Size, B);
end;

procedure Word1024Shl1(var A: Word1024);
begin
  WordBufShl1(A, Word1024Size);
end;

procedure Word1024Shr1(var A: Word1024);
begin
  WordBufShr1(A, Word1024Size);
end;

procedure Word1024Rol1(var A: Word1024);
var R : Word1024;
begin
  WordBufRol1(A, Word1024Size, R);
  A := R;
end;

procedure Word1024Ror1(var A: Word1024);
var R : Word1024;
begin
  WordBufRor1(A, Word1024Size, R);
  A := R;
end;

procedure Word1024Swap(var A, B: Word1024);
var C : Word1024;
begin
  C := A;
  A := B;
  B := C;
end;

procedure Word1024AddWord32(var A: Word1024; const B: LongWord);
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWord32(A, Word1024Size, B)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
end;

procedure Word1024AddWord64(var A: Word1024; const B: Word64);
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWord64(A, Word1024Size, B)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
end;

procedure Word1024AddWord128(var A: Word1024; const B: Word128);
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWord128(A, Word1024Size, B)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
end;

procedure Word1024AddWord256(var A: Word1024; const B: Word256);
var R : Word1024;
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWordBuf(A, Word1024Size, B, Word256Size, R)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
  A := R;
end;

procedure Word1024AddWord1024(var A: Word1024; const B: Word1024);
var R : Word1024;
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWordBuf(A, Word1024Size, B, Word1024Size, R)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
  A := R;
end;



{                                                                              }
{ Word2048                                                                     }
{                                                                              }
procedure Word2048InitZero(var A: Word2048);
begin
  WordBufInitZero(A, Word2048Size);
end;

procedure Word2048InitOne(var A: Word2048);
begin
  WordBufInitOne(A, Word2048Size);
end;

procedure Word2048InitMinimum(var A: Word2048);
begin
  WordBufInitMinimum(A, Word2048Size);
end;

procedure Word2048InitMaximum(var A: Word2048);
begin
  WordBufInitMaximum(A, Word2048Size);
end;

function Word2048IsZero(const A: Word2048): Boolean;
begin
  Result := WordBufIsZero(A, Word2048Size);
end;

function Word2048IsOne(const A: Word2048): Boolean;
begin
  Result := WordBufIsOne(A, Word2048Size);
end;

function Word2048IsMinimum(const A: Word2048): Boolean;
begin
  Result := WordBufIsMinimum(A, Word2048Size);
end;

function Word2048IsMaximum(const A: Word2048): Boolean;
begin
  Result := WordBufIsMaximum(A, Word2048Size);
end;

function Word2048IsOdd(const A: Word2048): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := WordBufIsOdd(A, Word2048Size);
end;

function Word2048IsWord32Range(const A: Word2048): Boolean;
begin
  Result := WordBufIsWord32Range(A, Word2048Size);
end;

function Word2048IsWord64Range(const A: Word2048): Boolean;
begin
  Result := WordBufIsWord64Range(A, Word2048Size);
end;

function Word2048IsWord128Range(const A: Word2048): Boolean;
begin
  Result := WordBufIsWord128Range(A, Word2048Size);
end;

function Word2048IsInt32Range(const A: Word2048): Boolean;
begin
  Result := WordBufIsInt32Range(A, Word2048Size);
end;

function Word2048IsInt64Range(const A: Word2048): Boolean;
begin
  Result := WordBufIsInt64Range(A, Word2048Size);
end;

function Word2048IsInt128Range(const A: Word2048): Boolean;
begin
  Result := WordBufIsInt128Range(A, Word2048Size);
end;

procedure Word2048InitWord32(var A: Word2048; const B: LongWord);
begin
  WordBufInitWord32(A, Word2048Size, B);
end;

procedure Word2048InitWord64(var A: Word2048; const B: Word64);
begin
  WordBufInitWord64(A, Word2048Size, B);
end;

procedure Word2048InitWord128(var A: Word2048; const B: Word128);
begin
  WordBufInitWord128(A, Word2048Size, B);
end;

procedure Word2048InitInt32(var A: Word2048; const B: LongInt);
begin
  WordBufInitInt32(A, Word2048Size, B);
end;

procedure Word2048InitInt64(var A: Word2048; const B: Int64);
begin
  WordBufInitInt64(A, Word2048Size, B);
end;

procedure Word2048InitInt128(var A: Word2048; const B: Int128);
begin
  WordBufInitInt128(A, Word2048Size, B);
end;

function Word2048ToWord32(const A: Word2048): LongWord;
begin
  Result := WordBufToWord32(A, Word2048Size);
end;

function Word2048ToWord64(const A: Word2048): Word64;
begin
  Result := WordBufToWord64(A, Word2048Size);
end;

function Word2048ToWord128(const A: Word2048): Word128;
begin
  Result := WordBufToWord128(A, Word2048Size);
end;

function Word2048ToInt32(const A: Word2048): LongInt;
begin
  Result := WordBufToInt32(A, Word2048Size);
end;

function Word2048ToInt64(const A: Word2048): Int64;
begin
  Result := WordBufToInt64(A, Word2048Size);
end;

function Word2048ToInt128(const A: Word2048): Int128;
begin
  Result := WordBufToInt128(A, Word2048Size);
end;

function Word2048ToFloat(const A: Word2048): Extended;
begin
  Result := WordBufToFloat(A, Word2048Size);
end;

function Word2048Lo(const A: Word2048): Word256;
begin
  Result := A.Word256s[0];
end;

function Word2048Hi(const A: Word2048): Word256;
begin
  Result := A.Word256s[1];
end;

function Word2048EqualsWord32(const A: Word2048; const B: LongWord): Boolean;
begin
  Result := WordBufEqualsWord32(A, Word2048Size, B);
end;

function  Word2048EqualsWord64(const A: Word2048; const B: Word64): Boolean;
begin
  Result := WordBufEqualsWord64(A, Word2048Size, B);
end;

function  Word2048EqualsWord128(const A: Word2048; const B: Word128): Boolean;
begin
  Result := WordBufEqualsWord128(A, Word2048Size, B);
end;

function  Word2048EqualsWord2048(const A, B: Word2048): Boolean;
begin
  Result := WordBufEqualsWordBuf(A, Word2048Size, B, Word2048Size);
end;

function Word2048EqualsInt32(const A: Word2048; const B: LongInt): Boolean;
begin
  Result := WordBufEqualsInt32(A, Word2048Size, B);
end;

function Word2048EqualsInt64(const A: Word2048; const B: Int64): Boolean;
begin
  Result := WordBufEqualsInt64(A, Word2048Size, B);
end;

function Word2048EqualsInt128(const A: Word2048; const B: Int128): Boolean;
begin
  Result := WordBufEqualsInt128(A, Word2048Size, B);
end;

function Word2048CompareWord32(const A: Word2048; const B: LongWord): Integer;
begin
  Result := WordBufCompareWord32(A, Word2048Size, B);
end;

function Word2048CompareWord64(const A: Word2048; const B: Word64): Integer;
begin
  Result := WordBufCompareWord64(A, Word2048Size, B);
end;

function Word2048CompareWord128(const A: Word2048; const B: Word128): Integer;
begin
  Result := WordBufCompareWord128(A, Word2048Size, B);
end;

function Word2048CompareWord2048(const A, B: Word2048): Integer;
begin
  Result := WordBufCompareWordBuf(A, Word2048Size, B, Word2048Size);
end;

function Word2048CompareInt32(const A: Word2048; const B: LongInt): Integer;
begin
  Result := WordBufCompareInt32(A, Word2048Size, B);
end;

function Word2048CompareInt64(const A: Word2048; const B: Int64): Integer;
begin
  Result := WordBufCompareInt64(A, Word2048Size, B);
end;

function Word2048CompareInt128(const A: Word2048; const B: Int128): Integer;
begin
  Result := WordBufCompareInt128(A, Word2048Size, B);
end;

function Word2048Min(const A, B: Word2048): Word2048;
begin
  if WordBufCompareWordBuf(A, Word2048Size, B, Word2048Size) <= 0 then
    Result := A
  else
    Result := B;
end;

function Word2048Max(const A, B: Word2048): Word2048;
begin
  if WordBufCompareWordBuf(A, Word2048Size, B, Word2048Size) >= 0 then
    Result := A
  else
    Result := B;
end;

procedure Word2048Not(var A: Word2048);
begin
  WordBufNot(A, Word2048Size);
end;

procedure Word2048OrWord2048(var A: Word2048; const B: Word2048);
begin
  WordBufOrWordBuf(A, Word2048Size, B, Word2048Size);
end;

procedure Word2048AndWord2048(var A: Word2048; const B: Word2048);
begin
  WordBufAndWordBuf(A, Word2048Size, B, Word2048Size);
end;

procedure Word2048XorWord2048(var A: Word2048; const B: Word2048);
begin
  WordBufXorWordBuf(A, Word2048Size, B, Word2048Size);
end;

function Word2048IsBitSet(const A: Word2048; const B: Integer): Boolean;
begin
  Result := WordBufIsBitSet(A, Word2048Size, B);
end;

procedure Word2048SetBit(var A: Word2048; const B: Integer);
begin
  WordBufSetBit(A, Word2048Size, B);
end;

procedure Word2048ClearBit(var A: Word2048; const B: Integer);
begin
  WordBufClearBit(A, Word2048Size, B);
end;

procedure Word2048ToggleBit(var A: Word2048; const B: Integer);
begin
  WordBufToggleBit(A, Word2048Size, B);
end;

procedure Word2048Shl1(var A: Word2048);
begin
  WordBufShl1(A, Word2048Size);
end;

procedure Word2048Shr1(var A: Word2048);
begin
  WordBufShr1(A, Word2048Size);
end;

procedure Word2048Rol1(var A: Word2048);
var R : Word2048;
begin
  WordBufRol1(A, Word2048Size, R);
  A := R;
end;

procedure Word2048Ror1(var A: Word2048);
var R : Word2048;
begin
  WordBufRor1(A, Word2048Size, R);
  A := R;
end;

procedure Word2048Swap(var A, B: Word2048);
var C : Word2048;
begin
  C := A;
  A := B;
  B := C;
end;

procedure Word2048AddWord32(var A: Word2048; const B: LongWord);
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWord32(A, Word2048Size, B)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
end;

procedure Word2048AddWord64(var A: Word2048; const B: Word64);
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWord64(A, Word2048Size, B)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
end;

procedure Word2048AddWord128(var A: Word2048; const B: Word128);
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWord128(A, Word2048Size, B)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
end;

procedure Word2048AddWord256(var A: Word2048; const B: Word256);
var R : Word2048;
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWordBuf(A, Word2048Size, B, Word256Size, R)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
  A := R;
end;

procedure Word2048AddWord2048(var A: Word2048; const B: Word2048);
var R : Word2048;
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWordBuf(A, Word2048Size, B, Word2048Size, R)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
  A := R;
end;



{                                                                              }
{ Word4096                                                                     }
{                                                                              }
procedure Word4096InitZero(var A: Word4096);
begin
  WordBufInitZero(A, Word4096Size);
end;

procedure Word4096InitOne(var A: Word4096);
begin
  WordBufInitOne(A, Word4096Size);
end;

procedure Word4096InitMinimum(var A: Word4096);
begin
  WordBufInitMinimum(A, Word4096Size);
end;

procedure Word4096InitMaximum(var A: Word4096);
begin
  WordBufInitMaximum(A, Word4096Size);
end;

function Word4096IsZero(const A: Word4096): Boolean;
begin
  Result := WordBufIsZero(A, Word4096Size);
end;

function Word4096IsOne(const A: Word4096): Boolean;
begin
  Result := WordBufIsOne(A, Word4096Size);
end;

function Word4096IsMinimum(const A: Word4096): Boolean;
begin
  Result := WordBufIsMinimum(A, Word4096Size);
end;

function Word4096IsMaximum(const A: Word4096): Boolean;
begin
  Result := WordBufIsMaximum(A, Word4096Size);
end;

function Word4096IsOdd(const A: Word4096): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := WordBufIsOdd(A, Word4096Size);
end;

function Word4096IsWord32Range(const A: Word4096): Boolean;
begin
  Result := WordBufIsWord32Range(A, Word4096Size);
end;

function Word4096IsWord64Range(const A: Word4096): Boolean;
begin
  Result := WordBufIsWord64Range(A, Word4096Size);
end;

function Word4096IsWord128Range(const A: Word4096): Boolean;
begin
  Result := WordBufIsWord128Range(A, Word4096Size);
end;

function Word4096IsInt32Range(const A: Word4096): Boolean;
begin
  Result := WordBufIsInt32Range(A, Word4096Size);
end;

function Word4096IsInt64Range(const A: Word4096): Boolean;
begin
  Result := WordBufIsInt64Range(A, Word4096Size);
end;

function Word4096IsInt128Range(const A: Word4096): Boolean;
begin
  Result := WordBufIsInt128Range(A, Word4096Size);
end;

procedure Word4096InitWord32(var A: Word4096; const B: LongWord);
begin
  WordBufInitWord32(A, Word4096Size, B);
end;

procedure Word4096InitWord64(var A: Word4096; const B: Word64);
begin
  WordBufInitWord64(A, Word4096Size, B);
end;

procedure Word4096InitWord128(var A: Word4096; const B: Word128);
begin
  WordBufInitWord128(A, Word4096Size, B);
end;

procedure Word4096InitInt32(var A: Word4096; const B: LongInt);
begin
  WordBufInitInt32(A, Word4096Size, B);
end;

procedure Word4096InitInt64(var A: Word4096; const B: Int64);
begin
  WordBufInitInt64(A, Word4096Size, B);
end;

procedure Word4096InitInt128(var A: Word4096; const B: Int128);
begin
  WordBufInitInt128(A, Word4096Size, B);
end;

function Word4096ToWord32(const A: Word4096): LongWord;
begin
  Result := WordBufToWord32(A, Word4096Size);
end;

function Word4096ToWord64(const A: Word4096): Word64;
begin
  Result := WordBufToWord64(A, Word4096Size);
end;

function Word4096ToWord128(const A: Word4096): Word128;
begin
  Result := WordBufToWord128(A, Word4096Size);
end;

function Word4096ToInt32(const A: Word4096): LongInt;
begin
  Result := WordBufToInt32(A, Word4096Size);
end;

function Word4096ToInt64(const A: Word4096): Int64;
begin
  Result := WordBufToInt64(A, Word4096Size);
end;

function Word4096ToInt128(const A: Word4096): Int128;
begin
  Result := WordBufToInt128(A, Word4096Size);
end;

function Word4096ToFloat(const A: Word4096): Extended;
begin
  Result := WordBufToFloat(A, Word4096Size);
end;

function Word4096Lo(const A: Word4096): Word256;
begin
  Result := A.Word256s[0];
end;

function Word4096Hi(const A: Word4096): Word256;
begin
  Result := A.Word256s[1];
end;

function Word4096EqualsWord32(const A: Word4096; const B: LongWord): Boolean;
begin
  Result := WordBufEqualsWord32(A, Word4096Size, B);
end;

function  Word4096EqualsWord64(const A: Word4096; const B: Word64): Boolean;
begin
  Result := WordBufEqualsWord64(A, Word4096Size, B);
end;

function  Word4096EqualsWord128(const A: Word4096; const B: Word128): Boolean;
begin
  Result := WordBufEqualsWord128(A, Word4096Size, B);
end;

function  Word4096EqualsWord4096(const A, B: Word4096): Boolean;
begin
  Result := WordBufEqualsWordBuf(A, Word4096Size, B, Word4096Size);
end;

function Word4096EqualsInt32(const A: Word4096; const B: LongInt): Boolean;
begin
  Result := WordBufEqualsInt32(A, Word4096Size, B);
end;

function Word4096EqualsInt64(const A: Word4096; const B: Int64): Boolean;
begin
  Result := WordBufEqualsInt64(A, Word4096Size, B);
end;

function Word4096EqualsInt128(const A: Word4096; const B: Int128): Boolean;
begin
  Result := WordBufEqualsInt128(A, Word4096Size, B);
end;

function Word4096CompareWord32(const A: Word4096; const B: LongWord): Integer;
begin
  Result := WordBufCompareWord32(A, Word4096Size, B);
end;

function Word4096CompareWord64(const A: Word4096; const B: Word64): Integer;
begin
  Result := WordBufCompareWord64(A, Word4096Size, B);
end;

function Word4096CompareWord128(const A: Word4096; const B: Word128): Integer;
begin
  Result := WordBufCompareWord128(A, Word4096Size, B);
end;

function Word4096CompareWord4096(const A, B: Word4096): Integer;
begin
  Result := WordBufCompareWordBuf(A, Word4096Size, B, Word4096Size);
end;

function Word4096CompareInt32(const A: Word4096; const B: LongInt): Integer;
begin
  Result := WordBufCompareInt32(A, Word4096Size, B);
end;

function Word4096CompareInt64(const A: Word4096; const B: Int64): Integer;
begin
  Result := WordBufCompareInt64(A, Word4096Size, B);
end;

function Word4096CompareInt128(const A: Word4096; const B: Int128): Integer;
begin
  Result := WordBufCompareInt128(A, Word4096Size, B);
end;

function Word4096Min(const A, B: Word4096): Word4096;
begin
  if WordBufCompareWordBuf(A, Word4096Size, B, Word4096Size) <= 0 then
    Result := A
  else
    Result := B;
end;

function Word4096Max(const A, B: Word4096): Word4096;
begin
  if WordBufCompareWordBuf(A, Word4096Size, B, Word4096Size) >= 0 then
    Result := A
  else
    Result := B;
end;

procedure Word4096Not(var A: Word4096);
begin
  WordBufNot(A, Word4096Size);
end;

procedure Word4096OrWord4096(var A: Word4096; const B: Word4096);
begin
  WordBufOrWordBuf(A, Word4096Size, B, Word4096Size);
end;

procedure Word4096AndWord4096(var A: Word4096; const B: Word4096);
begin
  WordBufAndWordBuf(A, Word4096Size, B, Word4096Size);
end;

procedure Word4096XorWord4096(var A: Word4096; const B: Word4096);
begin
  WordBufXorWordBuf(A, Word4096Size, B, Word4096Size);
end;

function Word4096IsBitSet(const A: Word4096; const B: Integer): Boolean;
begin
  Result := WordBufIsBitSet(A, Word4096Size, B);
end;

procedure Word4096SetBit(var A: Word4096; const B: Integer);
begin
  WordBufSetBit(A, Word4096Size, B);
end;

procedure Word4096ClearBit(var A: Word4096; const B: Integer);
begin
  WordBufClearBit(A, Word4096Size, B);
end;

procedure Word4096ToggleBit(var A: Word4096; const B: Integer);
begin
  WordBufToggleBit(A, Word4096Size, B);
end;

procedure Word4096Shl1(var A: Word4096);
begin
  WordBufShl1(A, Word4096Size);
end;

procedure Word4096Shr1(var A: Word4096);
begin
  WordBufShr1(A, Word4096Size);
end;

procedure Word4096Rol1(var A: Word4096);
var R : Word4096;
begin
  WordBufRol1(A, Word4096Size, R);
  A := R;
end;

procedure Word4096Ror1(var A: Word4096);
var R : Word4096;
begin
  WordBufRor1(A, Word4096Size, R);
  A := R;
end;

procedure Word4096Swap(var A, B: Word4096);
var C : Word4096;
begin
  C := A;
  A := B;
  B := C;
end;

procedure Word4096AddWord32(var A: Word4096; const B: LongWord);
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWord32(A, Word4096Size, B)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
end;

procedure Word4096AddWord64(var A: Word4096; const B: Word64);
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWord64(A, Word4096Size, B)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
end;

procedure Word4096AddWord128(var A: Word4096; const B: Word128);
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWord128(A, Word4096Size, B)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
end;

procedure Word4096AddWord256(var A: Word4096; const B: Word256);
var R : Word4096;
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWordBuf(A, Word4096Size, B, Word256Size, R)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
  A := R;
end;

procedure Word4096AddWord4096(var A: Word4096; const B: Word4096);
var R : Word4096;
begin
  {$IFOPT Q+}if{$ENDIF}
  WordBufAddWordBuf(A, Word4096Size, B, Word4096Size, R)
  {$IFOPT Q+}> 0 then RaiseOverflowError{$ENDIF};
  A := R;
end;




{                                                                              }
{ Int8                                                                         }
{                                                                              }
{$IFDEF ASM386_DELPHI}
function Int8Sign(const A: ShortInt): Integer;
asm
    TEST AL, AL
    JL   @Negative
    JG   @Positive
    RET
  @Negative:
    MOV  EAX, -1
    RET
  @Positive:
    MOV  EAX, 1
end;
{$ELSE}
function Int8Sign(const A: ShortInt): Integer;
begin
  if A < 0 then
    Result := -1 else
  if A = 0 then
    Result := 0
  else
    Result := 1;
end;
{$ENDIF}

function Int8Abs(const A: ShortInt; var B: Byte): Boolean;
begin
  if A >= 0 then
    begin
      B := Byte(A);
      Result := False;
    end
  else
    begin
      B := Byte(-SmallInt(A));
      Result := True;
    end;
end;

function Int8IsWord8Range(const A: ShortInt): Boolean;
begin
  Result := (A >= 0);
end;

function Int8Compare(const A, B: ShortInt): Integer;
begin
  if A < B then
    Result := -1 else
  if A > B then
    Result := 1
  else
    Result := 0;
end;

function Int8Hash(const A: ShortInt): Byte;
begin
  Result := Word8HashTable[Byte(A)];
end;



{                                                                              }
{ Int16                                                                        }
{                                                                              }
{$IFDEF ASM386_DELPHI}
function Int16Sign(const A: SmallInt): Integer;
asm
    TEST AX, AX
    JL   @Negative
    JG   @Positive
    RET
  @Negative:
    MOV  EAX, -1
    RET
  @Positive:
    MOV  EAX, 1
end;
{$ELSE}
function Int16Sign(const A: SmallInt): Integer;
begin
  if A < 0 then
    Result := -1 else
  if A = 0 then
    Result := 0
  else
    Result := 1;
end;
{$ENDIF}

function Int16Abs(const A: SmallInt; var B: Word): Boolean;
begin
  if A >= 0 then
    begin
      B := Word(A);
      Result := False;
    end
  else
    begin
      B := Word(-LongInt(A));
      Result := True;
    end;
end;

function Int16IsInt8Range(const A: SmallInt): Boolean;
begin
  Result := (A >= MinInt8) and (A <= MaxInt8);
end;

function Int16IsWord8Range(const A: SmallInt): Boolean;
begin
  Result := (A >= 0) and (A <= MaxWord8);
end;

function Int16IsWord16Range(const A: SmallInt): Boolean;
begin
  Result := (A >= 0);
end;

function Int16Compare(const A, B: SmallInt): Integer;
begin
  if A < B then
    Result := -1 else
  if A > B then
    Result := 1
  else
    Result := 0;
end;

function Int16Hash(const A: SmallInt): Word;
begin
  Result := Word16Hash(Word(A));
end;



{                                                                              }
{ Int32                                                                        }
{                                                                              }
{$IFDEF ASM386_DELPHI}
function Int32Sign(const A: LongInt): Integer;
asm
    TEST EAX, EAX
    JL   @Negative
    JG   @Positive
    RET
  @Negative:
    MOV  EAX, -1
    RET
  @Positive:
    MOV  EAX, 1
end;
{$ELSE}
function Int32Sign(const A: LongInt): Integer;
begin
  if A < 0 then
    Result := -1 else
  if A = 0 then
    Result := 0
  else
    Result := 1;
end;
{$ENDIF}

function Int32Abs(const A: LongInt; var B: LongWord): Boolean;
begin
  if A >= 0 then
    begin
      B := LongWord(A);
      Result := False;
    end else
  if A = Low(LongInt) then
    begin
      B := $80000000;
      Result := True;
    end
  else
    begin
      B := LongWord(-A);
      Result := True;
    end;
end;

function Int32IsInt8Range(const A: LongInt): Boolean;
begin
  Result := (A >= MinInt8) and (A <= MaxInt8);
end;

function Int32IsInt16Range(const A: LongInt): Boolean;
begin
  Result := (A >= MinInt16) and (A <= MaxInt16);
end;

function Int32IsWord8Range(const A: LongInt): Boolean;
begin
  Result := (A >= 0) and (A <= MaxWord8);
end;

function Int32IsWord16Range(const A: LongInt): Boolean;
begin
  Result := (A >= 0) and (A <= MaxWord16);
end;

function Int32IsWord32Range(const A: LongInt): Boolean;
begin
  Result := (A >= 0);
end;

function Int32Compare(const A, B: LongInt): Integer;
begin
  if A < B then
    Result := -1 else
  if A > B then
    Result := 1
  else
    Result := 0;
end;

function Int32Hash(const A: LongInt): LongWord;
begin
  Result := Word32Hash(LongWord(A));
end;



{                                                                              }
{ Int64                                                                        }
{                                                                              }
function Int64Sign(const A: Int64): Integer;
begin
  if A < 0 then
    Result := -1 else
  if A = 0 then
    Result := 0
  else
    Result := 1;
end;

function Int64Abs(const A: Int64; var B: Word64): Boolean;
begin
  if A >= 0 then
    begin
      Word64InitInt64(B, A);
      Result := False;
    end
  else
    begin
      if A = MinInt64 then
        begin
          B.LongWords[0] := $00000000;
          B.LongWords[1] := $80000000;
        end
      else
        Word64InitInt64(B, -A);
      Result := True;
    end;
end;

function Int64IsInt8Range(const A: Int64): Boolean;
begin
  Result := (A >= MinInt8) and (A <= MaxInt8);
end;

function Int64IsInt16Range(const A: Int64): Boolean;
begin
  Result := (A >= MinInt16) and (A <= MaxInt16);
end;

function Int64IsInt32Range(const A: Int64): Boolean;
begin
  Result := (A >= MinInt32) and (A <= MaxInt32);
end;

function Int64IsWord8Range(const A: Int64): Boolean;
begin
  Result := (A >= 0) and (A <= MaxWord8);
end;

function Int64IsWord16Range(const A: Int64): Boolean;
begin
  Result := (A >= 0) and (A <= MaxWord16);
end;

function Int64IsWord32Range(const A: Int64): Boolean;
begin
  Result := (A >= 0) and (A <= MaxWord32);
end;

function Int64IsWord64Range(const A: Int64): Boolean;
begin
  Result := (A >= 0);
end;

function Int64Compare(const A, B: Int64): Integer;
begin
  if A < B then
    Result := -1
  else
  if A > B then
    Result := 1
  else
    Result := 0;
end;

function Int64Hash(const A: Int64): LongWord;
begin
  Result := Word32Hash(Int64Rec(A).LongWords[0]) xor
            Word32Hash(Int64Rec(A).LongWords[1]);
end;



{                                                                              }
{ Int128                                                                       }
{                                                                              }
{$IFDEF ASM386_DELPHI}
procedure Int128InitZero(var A: Int128);
asm
    xor edx, edx
    mov [eax], edx
    mov [eax + 4], edx
    mov [eax + 8], edx
    mov [eax + 12], edx
end;
{$ELSE}
procedure Int128InitZero(var A: Int128);
begin
  A.LongWords[0] := $00000000;
  A.LongWords[1] := $00000000;
  A.LongWords[2] := $00000000;
  A.LongWords[3] := $00000000;
end;
{$ENDIF}

procedure Int128InitOne(var A: Int128);
begin
  A.LongWords[0] := $00000001;
  A.LongWords[1] := $00000000;
  A.LongWords[2] := $00000000;
  A.LongWords[3] := $00000000;
end;

procedure Int128InitMinusOne(var A: Int128);
begin
  A.LongWords[0] := $FFFFFFFF;
  A.LongWords[1] := $FFFFFFFF;
  A.LongWords[2] := $FFFFFFFF;
  A.LongWords[3] := $FFFFFFFF;
end;

procedure Int128InitMinimum(var A: Int128);
begin
  A.LongWords[0] := $00000000;
  A.LongWords[1] := $00000000;
  A.LongWords[2] := $00000000;
  A.LongWords[3] := $80000000;
end;

procedure Int128InitMaximum(var A: Int128);
begin
  A.LongWords[0] := $FFFFFFFF;
  A.LongWords[1] := $FFFFFFFF;
  A.LongWords[2] := $FFFFFFFF;
  A.LongWords[3] := $7FFFFFFF;
end;

function Int128IsNegative(const A: Int128): Boolean;
begin
  Result := A.LongWords[3] and $80000000 <> 0;
end;

function Int128IsZero(const A: Int128): Boolean;
begin
  Result := (A.LongWords[0] = $00000000) and
            (A.LongWords[1] = $00000000) and
            (A.LongWords[2] = $00000000) and
            (A.LongWords[3] = $00000000);
end;

function Int128IsOne(const A: Int128): Boolean;
begin
  Result := (A.LongWords[0] = $00000001) and
            (A.LongWords[1] = $00000000) and
            (A.LongWords[2] = $00000000) and
            (A.LongWords[3] = $00000000);
end;

function Int128IsMinusOne(const A: Int128): Boolean;
begin
  Result := (A.LongWords[0] = $FFFFFFFF) and
            (A.LongWords[1] = $FFFFFFFF) and
            (A.LongWords[2] = $FFFFFFFF) and
            (A.LongWords[3] = $FFFFFFFF);
end;

function Int128IsMinimum(const A: Int128): Boolean;
begin
  Result := (A.LongWords[0] = $00000000) and
            (A.LongWords[1] = $00000000) and
            (A.LongWords[2] = $00000000) and
            (A.LongWords[3] = $80000000);
end;

function Int128IsMaximum(const A: Int128): Boolean;
begin
  Result := (A.LongWords[0] = $FFFFFFFF) and
            (A.LongWords[1] = $FFFFFFFF) and
            (A.LongWords[2] = $FFFFFFFF) and
            (A.LongWords[3] = $7FFFFFFF);
end;

function Int128IsOdd(const A: Int128): Boolean;
begin
  Result := (A.LongWords[0] and 1 = 1);
end;

function Int128Sign(const A: Int128): Integer;
begin
  if A.LongWords[3] and $80000000 <> 0 then
    Result := -1 else
  if (A.LongWords[0] = 0) and
     (A.LongWords[1] = 0) and
     (A.LongWords[2] = 0) and
     (A.LongWords[3] = 0) then
    Result := 0
  else
    Result := 1;
end;

{$IFDEF ASM386_DELPHI}
procedure Int128Negate(var A: Int128);
asm
    not dword ptr [eax]
    not dword ptr [eax + 4]
    not dword ptr [eax + 8]
    not dword ptr [eax + 12]
    add dword ptr [eax], 1
    adc dword ptr [eax + 4], 0
    adc dword ptr [eax + 8], 0
    adc dword ptr [eax + 12], 0
    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
  @Fin:
    {$ENDIF}
end;
{$ELSE}
procedure Int128Negate(var A: Int128);
begin
  {$IFOPT Q+}
  if Int128IsMinimum(A) then
    RaiseOverflowError;
  {$ENDIF}
  A.LongWords[0] := not A.LongWords[0];
  A.LongWords[1] := not A.LongWords[1];
  A.LongWords[2] := not A.LongWords[2];
  A.LongWords[3] := not A.LongWords[3];
  if A.LongWords[0] < $FFFFFFFF then
    begin
      Inc(A.LongWords[0]);
      exit;
    end;
  A.LongWords[0] := 0;
  if A.LongWords[1] < $FFFFFFFF then
    begin
      Inc(A.LongWords[1]);
      exit;
    end;
  A.LongWords[1] := 0;
  if A.LongWords[2] < $FFFFFFFF then
    begin
      Inc(A.LongWords[2]);
      exit;
    end;
  A.LongWords[2] := 0;
  if A.LongWords[3] < $FFFFFFFF then
    Inc(A.LongWords[3])
  else
    A.LongWords[3] := 0;
end;
{$ENDIF}

procedure Int128InitNegWord128(var A: Int128; const B: Word128);
begin
  if B.LongWords[3] >= $80000000 then
    begin
      if (B.LongWords[3] = $80000000) and (B.LongWords[2] = $00000000) and
         (B.LongWords[1] = $00000000) and (B.LongWords[0] = $00000000) then
        begin
          A.LongWords[0] := $00000000;
          A.LongWords[1] := $00000000;
          A.LongWords[2] := $00000000;
          A.LongWords[3] := $80000000;
        end
      {$IFOPT R+}
      else
        RaiseRangeError
      {$ENDIF};
    end
  else
    begin
      A.LongWords[0] := B.LongWords[0];
      A.LongWords[1] := B.LongWords[1];
      A.LongWords[2] := B.LongWords[2];
      A.LongWords[3] := B.LongWords[3];
      Int128Negate(A);
    end;
end;

function Int128AbsInPlace(var A: Int128): Boolean;
begin
  if A.LongWords[3] and $80000000 <> 0 then
    begin
      Int128Negate(A);
      Result := True;
    end
  else
    Result := False;
end;

function Int128Abs(const A: Int128; var B: Word128): Boolean;
begin
  if A.LongWords[3] and $80000000 <> 0 then
    begin
      if (A.LongWords[3] = $80000000) and (A.LongWords[2] = $00000000) and
         (A.LongWords[1] = $00000000) and (A.LongWords[0] = $00000000) then
        begin
          // Minimum
          B.LongWords[0] := $00000000;
          B.LongWords[1] := $00000000;
          B.LongWords[2] := $00000000;
          B.LongWords[3] := $80000000;
        end
      else
        begin
          // Twos complement
          B.LongWords[0] := not A.LongWords[0];
          B.LongWords[1] := not A.LongWords[1];
          B.LongWords[2] := not A.LongWords[2];
          B.LongWords[3] := not A.LongWords[3];
          if B.LongWords[0] < $FFFFFFFF then
            Inc(B.LongWords[0])
          else
            begin
              B.LongWords[0] := 0;
              if B.LongWords[1] < $FFFFFFFF then
                Inc(B.LongWords[1])
              else
                begin
                  B.LongWords[1] := 0;
                  if B.LongWords[2] < $FFFFFFFF then
                    Inc(B.LongWords[2])
                  else
                    begin
                      B.LongWords[2] := 0;
                      if B.LongWords[3] < $FFFFFFFF then
                        Inc(B.LongWords[3])
                      else
                        B.LongWords[3] := 0;
                    end;
                end;
            end;
        end;
      Result := True;
    end
  else
    begin
      B.LongWords[0] := A.LongWords[0];
      B.LongWords[1] := A.LongWords[1];
      B.LongWords[2] := A.LongWords[2];
      B.LongWords[3] := A.LongWords[3];
      Result := False;
    end;
end;

function Int128IsWord8Range(const A: Int128): Boolean;
begin
  if A.LongWords[3] and $80000000 <> 0 then
    Result := False
  else
    Result := (A.LongWords[3] = 0) and (A.LongWords[2] = 0) and (A.LongWords[1] = 0)
          and (A.LongWords[0] <= MaxWord8);
end;

function Int128IsWord16Range(const A: Int128): Boolean;
begin
  if A.LongWords[3] and $80000000 <> 0 then
    Result := False
  else
    Result := (A.LongWords[3] = 0) and (A.LongWords[2] = 0) and (A.LongWords[1] = 0)
          and (A.LongWords[0] <= MaxWord16);
end;

function Int128IsWord32Range(const A: Int128): Boolean;
begin
  if A.LongWords[3] and $80000000 <> 0 then
    Result := False
  else
    Result := (A.LongWords[3] = 0) and (A.LongWords[2] = 0) and (A.LongWords[1] = 0);
end;

function Int128IsWord64Range(const A: Int128): Boolean;
begin
  if A.LongWords[3] and $80000000 <> 0 then
    Result := False
  else
    Result := (A.LongWords[3] = 0) and (A.LongWords[2] = 0);
end;

function Int128IsWord128Range(const A: Int128): Boolean;
begin
  Result := (A.LongWords[3] and $80000000 = 0);
end;

function Int128IsInt8Range(const A: Int128): Boolean;
begin
  Result := Int128IsInt32Range(A);
  if not Result then
    exit;
  Result := Int32IsInt8Range(Int128ToInt32(A));
end;

function Int128IsInt16Range(const A: Int128): Boolean;
begin
  Result := Int128IsInt32Range(A);
  if not Result then
    exit;
  Result := Int32IsInt16Range(Int128ToInt32(A));
end;

function Int128IsInt32Range(const A: Int128): Boolean;
begin
  if A.LongWords[3] and $80000000 <> 0 then
    Result := (A.LongWords[0] >= $80000000) and
              (A.LongWords[1] = $FFFFFFFF) and
              (A.LongWords[2] = $FFFFFFFF) and
              (A.LongWords[3] = $FFFFFFFF)
  else
    Result := (A.LongWords[0] < $80000000) and
              (A.LongWords[1] = 0) and
              (A.LongWords[2] = 0) and
              (A.LongWords[3] = 0);
end;

function Int128IsInt64Range(const A: Int128): Boolean;
begin
  if A.LongWords[3] and $80000000 <> 0 then
    Result := (A.LongWords[1] >= $80000000) and
              (A.LongWords[2] = $FFFFFFFF) and
              (A.LongWords[3] = $FFFFFFFF)
  else
    Result := (A.LongWords[1] < $80000000) and
              (A.LongWords[2] = 0) and
              (A.LongWords[3] = 0);
end;

procedure Int128InitWord32(var A: Int128; const B: LongWord);
begin
  A.LongWords[0] := B;
  A.LongWords[1] := 0;
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
end;

procedure Int128InitWord64(var A: Int128; const B: Word64);
begin
  A.LongWords[0] := B.LongWords[0];
  A.LongWords[1] := B.LongWords[1];
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
end;

procedure Int128InitWord128(var A: Int128; const B: Word128);
begin
  {$IFOPT R+}
  if B.LongWords[3] and $80000000 <> 0 then
    RaiseRangeError;
  {$ENDIF}
  A.LongWords[0] := B.LongWords[0];
  A.LongWords[1] := B.LongWords[1];
  A.LongWords[2] := B.LongWords[2];
  A.LongWords[3] := B.LongWords[3];
end;

procedure Int128InitInt32(var A: Int128; const B: LongInt);
var C : LongInt;
begin
  A.LongInts[0] := B;
  if B >= 0 then
    C := 0
  else
    C := -1;
  A.LongInts[1] := C;
  A.LongInts[2] := C;
  A.LongInts[3] := C;
end;

procedure Int128InitInt64(var A: Int128; const B: Int64);
begin
  A.Int64s[0] := B;
  if B >= 0 then
    A.Int64s[1] := 0
  else
    A.Int64s[1] := -1;
end;

const
  MinInt128F : Extended =
     -4294967296.0 * 4294967296.0 * 4294967296.0 * 2147483648.0;   // -80000000 00000000 00000000 00000000
  MaxInt128F : Extended =
      4294967296.0 * 4294967296.0 * 4294967296.0 * 2147483647.0 +  //  7FFFFFFF 00000000 00000000 00000000
      4294967296.0 * 4294967296.0 * 4294967295.0 +                 //  00000000 FFFFFFFF 00000000 00000000
      4294967296.0 * 4294967295.0 +                                //  00000000 00000000 FFFFFFFF 00000000
      4294967295.0;                                                //  00000000 00000000 00000000 FFFFFFFF

procedure Int128InitFloat(var A: Int128; const B: Extended);
var C : Extended;
    D : Extended;
    E : Int64;
    F : Boolean;
begin
  C := Int(B);
  {$IFOPT R+}
  if (C > MaxInt128F) or (C < MinInt128F) then
    RaiseRangeError;
  {$ENDIF}
  F := (C < 0);
  if F then
    if C = MinInt128F then
      begin
        Int128InitMinimum(A);
        exit;
      end
    else
      C := -C;
  D := 4294967296.0 * 4294967296.0 * 4294967296.0;
  E := Trunc(C / D);
  A.LongWords[3] := Int64Rec(E).LongWords[0];
  C := C - E * D;
  D := 4294967296.0 * 4294967296.0;
  E := Trunc(C / D);
  A.LongWords[2] := Int64Rec(E).LongWords[0];
  C := C - E * D;
  D := 4294967296.0;
  E := Trunc(C / D);
  A.LongWords[1] := Int64Rec(E).LongWords[0];
  C := C - E * D;
  E := Trunc(C);
  A.LongWords[0] := Int64Rec(E).LongWords[0];
  if F then
    Int128Negate(A);
end;

function Int128ToWord32(const A: Int128): LongWord;
begin
  {$IFOPT R+}
  if (A.LongWords[3] <> 0) or
     (A.LongWords[2] <> 0) or
     (A.LongWords[1] <> 0) then
    RaiseRangeError;
  {$ENDIF}
  Result := A.LongWords[0];
end;

function Int128ToWord64(const A: Int128): Word64;
begin
  {$IFOPT R+}
  if (A.LongWords[3] <> 0) or
     (A.LongWords[2] <> 0) then
    RaiseRangeError;
  {$ENDIF}
  Result := A.Word64s[0];
end;

function Int128ToWord128(const A: Int128): Word128;
begin
  {$IFOPT R+}
  if A.LongWords[3] and $80000000 <> 0 then
    RaiseRangeError;
  {$ENDIF}
  Result.Word64s[0] := A.Word64s[0];
  Result.Word64s[1] := A.Word64s[1];
end;

function Int128ToInt32(const A: Int128): LongInt;
begin
  {$IFOPT R+}
  if not Int128IsInt32Range(A) then
    RaiseRangeError;
  {$ENDIF}
  Result := A.LongInts[0];
end;

function Int128ToInt64(const A: Int128): Int64;
begin
  {$IFOPT R+}
  if not Int128IsInt64Range(A) then
    RaiseRangeError;
  {$ENDIF}
  Result := A.Int64s[0];
end;

function Int128ToFloat(const A: Int128): Extended;
var B : Word128;
    N : Boolean;
    T : Extended;
begin
  N := Int128Abs(A, B);
  Result := B.LongWords[0];
  T := B.LongWords[1];
  Result := Result + T * 4294967296.0;
  T := B.LongWords[2];
  Result := Result + T * 4294967296.0 * 4294967296.0;
  T := B.LongWords[3];
  Result := Result + T * 4294967296.0 * 4294967296.0 * 4294967296.0;
  if N then
    Result := -Result;
end;

function Int128EqualsWord32(const A: Int128; const B: LongWord): Boolean;
begin
  Result := (A.LongWords[0] = B) and
            (A.LongWords[1] = 0) and
            (A.LongWords[2] = 0) and
            (A.LongWords[3] = 0);
end;

function Int128EqualsWord64(const A: Int128; const B: Word64): Boolean;
begin
  Result := (A.LongWords[0] = B.LongWords[0]) and
            (A.LongWords[1] = B.LongWords[1]) and
            (A.LongWords[2] = 0) and
            (A.LongWords[3] = 0);
end;

function Int128EqualsWord128(const A: Int128; const B: Word128): Boolean;
begin
  if A.LongWords[3] and $80000000 <> 0 then
    Result := False
  else
    Result := (A.LongWords[0] = B.LongWords[0]) and
              (A.LongWords[1] = B.LongWords[1]) and
              (A.LongWords[2] = B.LongWords[2]) and
              (A.LongWords[3] = B.LongWords[3]);
end;

function Int128EqualsInt32(const A: Int128; const B: LongInt): Boolean;
begin
  if A.LongInts[0] <> B then
    Result := False
  else
    if B < 0 then
      Result := (A.LongWords[1] = $FFFFFFFF) and
                (A.LongWords[2] = $FFFFFFFF) and
                (A.LongWords[3] = $FFFFFFFF)
    else
      Result := (A.LongWords[1] = 0) and
                (A.LongWords[2] = 0) and
                (A.LongWords[3] = 0);
end;

function Int128EqualsInt64(const A: Int128; const B: Int64): Boolean;
begin
  if A.Int64s[0] <> B then
    Result := False
  else
    if B < 0 then
      Result := (A.LongWords[2] = $FFFFFFFF) and
                (A.LongWords[3] = $FFFFFFFF)
    else
      Result := (A.LongWords[2] = 0) and
                (A.LongWords[3] = 0);
end;

function Int128EqualsInt128(const A, B: Int128): Boolean;
begin
  Result := (A.LongWords[0] = B.LongWords[0]) and
            (A.LongWords[1] = B.LongWords[1]) and
            (A.LongWords[2] = B.LongWords[2]) and
            (A.LongWords[3] = B.LongWords[3]);
end;

function Int128CompareWord32(const A: Int128; const B: LongWord): Integer;
begin
  if A.LongWords[3] and $80000000 <> 0 then
    Result := -1 else
  if (A.LongWords[3] > 0) or
     (A.LongWords[2] > 0) or
     (A.LongWords[1] > 0) then
    Result := 1 else
  if A.LongWords[0] < B then
    Result := -1 else
  if A.LongWords[0] > B then
    Result := 1
  else
    Result := 0;
end;

function Int128CompareWord64(const A: Int128; const B: Word64): Integer;
begin
  if A.LongWords[3] and $80000000 <> 0 then
    Result := -1 else
  if (A.LongWords[3] > 0) or
     (A.LongWords[2] > 0) then
    Result := 1 else
  if A.LongWords[1] > B.LongWords[1] then
    Result := 1 else
  if A.LongWords[1] < B.LongWords[1] then
    Result := -1 else
  if A.LongWords[0] > B.LongWords[0] then
    Result := 1 else
  if A.LongWords[0] < B.LongWords[0] then
    Result := -1
  else
    Result := 0;
end;

function Int128CompareWord128(const A: Int128; const B: Word128): Integer;
begin
  if A.LongWords[3] and $80000000 <> 0 then
    Result := -1 else
  if A.LongWords[3] > B.LongWords[3] then
    Result := 1 else
  if A.LongWords[3] < B.LongWords[3] then
    Result := -1 else
  if A.LongWords[2] > B.LongWords[2] then
    Result := 1 else
  if A.LongWords[2] < B.LongWords[2] then
    Result := -1 else
  if A.LongWords[1] > B.LongWords[1] then
    Result := 1 else
  if A.LongWords[1] < B.LongWords[1] then
    Result := -1 else
  if A.LongWords[0] > B.LongWords[0] then
    Result := 1 else
  if A.LongWords[0] < B.LongWords[0] then
    Result := -1
  else
    Result := 0;
end;

function Int128CompareInt32(const A: Int128; const B: LongInt): Integer;
var P, Q : Boolean;
begin
  P := A.LongWords[3] and $80000000 <> 0;
  Q := B < 0;
  if P <> Q then
    if P then
      Result := -1
    else
      Result := 1
  else
    if P then
      if (A.LongWords[3] <> $FFFFFFFF) or
         (A.LongWords[2] <> $FFFFFFFF) or
         (A.LongWords[1] <> $FFFFFFFF) then
        Result := -1 else
      if A.LongInts[0] < B then
        Result := -1 else
      if A.LongInts[0] > B then
        Result := 1
      else
        Result := 0
    else
      if (A.LongWords[3] <> 0) or
         (A.LongWords[2] <> 0) or
         (A.LongWords[1] <> 0) or
         (A.LongWords[0] >= $80000000) then
        Result := 1 else
      if A.LongInts[0] < B then
        Result := -1 else
      if A.LongInts[0] > B then
        Result := 1
      else
        Result := 0;
end;

function Int128CompareInt64(const A: Int128; const B: Int64): Integer;
var P, Q : Boolean;
begin
  P := A.LongWords[3] and $80000000 <> 0;
  Q := B < 0;
  if P <> Q then
    if P then
      Result := -1
    else
      Result := 1
  else
    if P then
      if (A.LongWords[3] <> $FFFFFFFF) or
         (A.LongWords[2] <> $FFFFFFFF) then
        Result := -1 else
      if A.Int64s[0] < B then
        Result := -1 else
      if A.Int64s[0] > B then
        Result := 1
      else
        Result := 0
    else
      if (A.LongWords[3] <> 0) or
         (A.LongWords[2] <> 0) or
         (A.LongWords[1] >= $80000000) then
        Result := 1 else
      if A.Int64s[0] < B then
        Result := -1 else
      if A.Int64s[0] > B then
        Result := 1
      else
        Result := 0;
end;

function Int128CompareInt128(const A, B: Int128): Integer;
var P, Q : Boolean;
begin
  P := A.LongWords[3] and $80000000 <> 0;
  Q := B.LongWords[3] and $80000000 <> 0;
  if P <> Q then
    if P then
      Result := -1
    else
      Result := 1
  else
    if A.LongWords[3] < B.LongWords[3] then
      Result := -1 else
    if A.LongWords[3] > B.LongWords[3] then
      Result := 1 else
    if A.LongWords[2] < B.LongWords[2] then
      Result := -1 else
    if A.LongWords[2] > B.LongWords[2] then
      Result := 1 else
    if A.LongWords[1] < B.LongWords[1] then
      Result := -1 else
    if A.LongWords[1] > B.LongWords[1] then
      Result := 1 else
    if A.LongWords[0] < B.LongWords[0] then
      Result := -1 else
    if A.LongWords[0] > B.LongWords[0] then
      Result := 1
    else
      Result := 0;
end;

function Int128Min(const A, B: Int128): Int128;
begin
  if Int128CompareInt128(A, B) <= 0 then
    Result := A
  else
    Result := B;
end;

function Int128Max(const A, B: Int128): Int128;
begin
  if Int128CompareInt128(A, B) >= 0 then
    Result := A
  else
    Result := B;
end;

procedure Int128Not(var A: Int128);
begin
  A.LongWords[0] := not A.LongWords[0];
  A.LongWords[1] := not A.LongWords[1];
  A.LongWords[2] := not A.LongWords[2];
  A.LongWords[3] := not A.LongWords[3];
end;

procedure Int128OrInt128(var A: Int128; const B: Int128);
begin
  A.LongWords[0] := A.LongWords[0] or B.LongWords[0];
  A.LongWords[1] := A.LongWords[1] or B.LongWords[1];
  A.LongWords[2] := A.LongWords[2] or B.LongWords[2];
  A.LongWords[3] := A.LongWords[3] or B.LongWords[3];
end;

procedure Int128AndInt128(var A: Int128; const B: Int128);
begin
  A.LongWords[0] := A.LongWords[0] and B.LongWords[0];
  A.LongWords[1] := A.LongWords[1] and B.LongWords[1];
  A.LongWords[2] := A.LongWords[2] and B.LongWords[2];
  A.LongWords[3] := A.LongWords[3] and B.LongWords[3];
end;

procedure Int128XorInt128(var A: Int128; const B: Int128);
begin
  A.LongWords[0] := A.LongWords[0] xor B.LongWords[0];
  A.LongWords[1] := A.LongWords[1] xor B.LongWords[1];
  A.LongWords[2] := A.LongWords[2] xor B.LongWords[2];
  A.LongWords[3] := A.LongWords[3] xor B.LongWords[3];
end;

function Int128IsBitSet(const A: Int128; const B: Integer): Boolean;
begin
  if (B < 0) or (B > 127) then
    Result := False
  else
    Result := (A.LongWords[B shr 5] and (1 shl (B and $1F)) <> 0);
end;

procedure Int128SetBit(var A: Int128; const B: Integer);
begin
  if (B < 0) or (B > 127) then
    exit;
  A.LongWords[B shr 5] := A.LongWords[B shr 5] or (1 shl (B and $1F));
end;

procedure Int128ClearBit(var A: Int128; const B: Integer);
begin
  if (B < 0) or (B > 127) then
    exit;
  A.LongWords[B shr 5] := A.LongWords[B shr 5] and not (1 shl (B and $1F));
end;

procedure Int128ToggleBit(var A: Int128; const B: Integer);
begin
  if (B < 0) or (B > 127) then
    exit;
  A.LongWords[B shr 5] := A.LongWords[B shr 5] xor (1 shl (B and $1F));
end;

// Returns index (0-127) of lowest set bit in A, or -1 if none set
function Int128SetBitScanForward(const A: Int128): Integer;
var B : Integer;
begin
  for B := 0 to 127 do
    if A.LongWords[B shr 5] and (1 shl (B and $1F)) <> 0 then
      begin
        Result := B;
        exit;
      end;
  Result := -1;
end;

// Returns index (0-127) of highest set bit in A, or -1 if none set
function Int128SetBitScanReverse(const A: Int128): Integer;
var B : Integer;
begin
  for B := 127 downto 0 do
    if A.LongWords[B shr 5] and (1 shl (B and $1F)) <> 0 then
      begin
        Result := B;
        exit;
      end;
  Result := -1;
end;

// Returns index (0-127) of lowest clear bit in A, or -1 if none clear
function Int128ClearBitScanForward(const A: Int128): Integer;
var B : Integer;
begin
  for B := 0 to 127 do
    if A.LongWords[B shr 5] and (1 shl (B and $1F)) = 0 then
      begin
        Result := B;
        exit;
      end;
  Result := -1;
end;

// Returns index (0-127) of highest clear bit in A, or -1 if none clear
function Int128ClearBitScanReverse(const A: Int128): Integer;
var B : Integer;
begin
  for B := 127 downto 0 do
    if A.LongWords[B shr 5] and (1 shl (B and $1F)) = 0 then
      begin
        Result := B;
        exit;
      end;
  Result := -1;
end;

procedure Int128Shl(var A: Int128; const B: Byte);
var C, D : Byte;
begin
  if B = 0 then
    exit;
  if B >= 128 then
    Int128InitZero(A) else
  if B < 32 then // 1 <= B <= 31
    begin
      C := 32 - B;
      A.LongWords[3] := (A.LongWords[3] shl B) or (A.LongWords[2] shr C);
      A.LongWords[2] := (A.LongWords[2] shl B) or (A.LongWords[1] shr C);
      A.LongWords[1] := (A.LongWords[1] shl B) or (A.LongWords[0] shr C);
      A.LongWords[0] := (A.LongWords[0] shl B);
    end else
  if B < 64 then // 32 <= B <= 63
    begin
      D := B - 32;
      C := 32 - D;
      A.LongWords[3] := (A.LongWords[2] shl D) or (A.LongWords[1] shr C);
      A.LongWords[2] := (A.LongWords[1] shl D) or (A.LongWords[0] shr C);
      A.LongWords[1] := (A.LongWords[0] shl D);
      A.LongWords[0] := 0;
    end else
  if B < 96 then // 64 <= B <= 95
    begin
      D := B - 64;
      C := 32 - D;
      A.LongWords[3] := (A.LongWords[1] shl D) or (A.LongWords[0] shr C);
      A.LongWords[2] := (A.LongWords[0] shl D);
      A.LongWords[1] := 0;
      A.LongWords[0] := 0;
    end
  else           // 96 <= B <= 127
    begin
      D := B - 96;
      A.LongWords[3] := (A.LongWords[0] shl D);
      A.LongWords[2] := 0;
      A.LongWords[1] := 0;
      A.LongWords[0] := 0;
    end;
end;

procedure Int128Shl1(var A: Int128);
begin
  A.LongWords[3] := (A.LongWords[3] shl 1) or (A.LongWords[2] shr 31);
  A.LongWords[2] := (A.LongWords[2] shl 1) or (A.LongWords[1] shr 31);
  A.LongWords[1] := (A.LongWords[1] shl 1) or (A.LongWords[0] shr 31);
  A.LongWords[0] := (A.LongWords[0] shl 1);
end;

procedure Int128Shr(var A: Int128; const B: Byte);
var C, D : Byte;
begin
  if B = 0 then
    exit;
  if B >= 128 then
    Int128InitZero(A) else
  if B < 32 then // 1 <= B <= 31
    begin
      C := 32 - B;
      A.LongWords[0] := (A.LongWords[0] shr B) or (A.LongWords[1] shl C);
      A.LongWords[1] := (A.LongWords[1] shr B) or (A.LongWords[2] shl C);
      A.LongWords[2] := (A.LongWords[2] shr B) or (A.LongWords[3] shl C);
      A.LongWords[3] := (A.LongWords[3] shr B);
    end else
  if B < 64 then // 32 <= B <= 63
    begin
      D := B - 32;
      C := 32 - D;
      A.LongWords[0] := (A.LongWords[1] shr D) or (A.LongWords[2] shl C);
      A.LongWords[1] := (A.LongWords[2] shr D) or (A.LongWords[3] shl C);
      A.LongWords[2] := (A.LongWords[3] shr D);
      A.LongWords[3] := 0;
    end else
  if B < 96 then // 64 <= B <= 95
    begin
      D := B - 64;
      C := 32 - D;
      A.LongWords[0] := (A.LongWords[2] shr D) or (A.LongWords[3] shl C);
      A.LongWords[1] := (A.LongWords[3] shr D);
      A.LongWords[2] := 0;
      A.LongWords[3] := 0;
    end
  else           // 96 <= B <= 127
    begin
      D := B - 96;
      A.LongWords[0] := (A.LongWords[3] shr D);
      A.LongWords[1] := 0;
      A.LongWords[2] := 0;
      A.LongWords[3] := 0;
    end;
end;

procedure Int128Shr1(var A: Int128);
begin
  A.LongWords[0] := (A.LongWords[0] shr 1) or (A.LongWords[1] shl 31);
  A.LongWords[1] := (A.LongWords[1] shr 1) or (A.LongWords[2] shl 31);
  A.LongWords[2] := (A.LongWords[2] shr 1) or (A.LongWords[3] shl 31);
  A.LongWords[3] := (A.LongWords[3] shr 1);
end;

procedure Int128Rol(var A: Int128; const B: Byte);
var C, D : Byte;
    E    : Int128;
begin
  C := B mod 128;
  if C = 0 then
    exit;
  if C < 32 then
    begin
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[3] shl C) or (A.LongWords[2] shr D);
      E.LongWords[2] := (A.LongWords[2] shl C) or (A.LongWords[1] shr D);
      E.LongWords[1] := (A.LongWords[1] shl C) or (A.LongWords[0] shr D);
      E.LongWords[0] := (A.LongWords[0] shl C) or (A.LongWords[3] shr D);
    end else
  if C < 64 then
    begin
      Dec(C, 32);
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[2] shl C) or (A.LongWords[1] shr D);
      E.LongWords[2] := (A.LongWords[1] shl C) or (A.LongWords[0] shr D);
      E.LongWords[1] := (A.LongWords[0] shl C) or (A.LongWords[3] shr D);
      E.LongWords[0] := (A.LongWords[3] shl C) or (A.LongWords[2] shr D);
    end else
  if C < 96 then
    begin
      Dec(C, 64);
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[1] shl C) or (A.LongWords[0] shr D);
      E.LongWords[2] := (A.LongWords[0] shl C) or (A.LongWords[3] shr D);
      E.LongWords[1] := (A.LongWords[3] shl C) or (A.LongWords[2] shr D);
      E.LongWords[0] := (A.LongWords[2] shl C) or (A.LongWords[1] shr D);
    end
  else
    begin
      Dec(C, 96);
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[0] shl C) or (A.LongWords[3] shr D);
      E.LongWords[2] := (A.LongWords[3] shl C) or (A.LongWords[2] shr D);
      E.LongWords[1] := (A.LongWords[2] shl C) or (A.LongWords[1] shr D);
      E.LongWords[0] := (A.LongWords[1] shl C) or (A.LongWords[0] shr D);
    end;
  A := E;
end;

procedure Int128Rol1(var A: Int128);
var B : Int128;
begin
  B.LongWords[3] := (A.LongWords[3] shl 1) or (A.LongWords[2] shr 31);
  B.LongWords[2] := (A.LongWords[2] shl 1) or (A.LongWords[1] shr 31);
  B.LongWords[1] := (A.LongWords[1] shl 1) or (A.LongWords[0] shr 31);
  B.LongWords[0] := (A.LongWords[0] shl 1) or (A.LongWords[3] shr 31);
  A := B;
end;

procedure Int128Ror(var A: Int128; const B: Byte);
var C, D : Byte;
    E    : Int128;
begin
  C := B mod 128;
  if C = 0 then
    exit;
  if C < 32 then
    begin
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[3] shr C) or (A.LongWords[0] shl D);
      E.LongWords[2] := (A.LongWords[2] shr C) or (A.LongWords[3] shl D);
      E.LongWords[1] := (A.LongWords[1] shr C) or (A.LongWords[2] shl D);
      E.LongWords[0] := (A.LongWords[0] shr C) or (A.LongWords[1] shl D);
    end else
  if C < 64 then
    begin
      Dec(C, 32);
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[2] shr C) or (A.LongWords[3] shl D);
      E.LongWords[2] := (A.LongWords[1] shr C) or (A.LongWords[2] shl D);
      E.LongWords[1] := (A.LongWords[0] shr C) or (A.LongWords[1] shl D);
      E.LongWords[0] := (A.LongWords[3] shr C) or (A.LongWords[0] shl D);
    end else
  if C < 96 then
    begin
      Dec(C, 64);
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[1] shr C) or (A.LongWords[2] shl D);
      E.LongWords[2] := (A.LongWords[0] shr C) or (A.LongWords[1] shl D);
      E.LongWords[1] := (A.LongWords[3] shr C) or (A.LongWords[0] shl D);
      E.LongWords[0] := (A.LongWords[2] shr C) or (A.LongWords[3] shl D);
    end
  else
    begin
      Dec(C, 96);
      D := 32 - C;
      E.LongWords[3] := (A.LongWords[0] shr C) or (A.LongWords[1] shl D);
      E.LongWords[2] := (A.LongWords[3] shr C) or (A.LongWords[0] shl D);
      E.LongWords[1] := (A.LongWords[2] shr C) or (A.LongWords[3] shl D);
      E.LongWords[0] := (A.LongWords[1] shr C) or (A.LongWords[2] shl D);
    end;
  A := E;
end;

procedure Int128Ror1(var A: Int128);
var B : Int128;
begin
  B.LongWords[3] := (A.LongWords[3] shr 1) or (A.LongWords[0] shl 31);
  B.LongWords[2] := (A.LongWords[2] shr 1) or (A.LongWords[3] shl 31);
  B.LongWords[1] := (A.LongWords[1] shr 1) or (A.LongWords[2] shl 31);
  B.LongWords[0] := (A.LongWords[0] shr 1) or (A.LongWords[1] shl 31);
  A := B;
end;

procedure Int128Swap(var A, B: Int128);
var D : LongWord;
begin
  D := A.LongWords[0];
  A.LongWords[0] := B.LongWords[0];
  B.LongWords[0] := D;
  D := A.LongWords[1];
  A.LongWords[1] := B.LongWords[1];
  B.LongWords[1] := D;
  D := A.LongWords[2];
  A.LongWords[2] := B.LongWords[2];
  B.LongWords[2] := D;
  D := A.LongWords[3];
  A.LongWords[3] := B.LongWords[3];
  B.LongWords[3] := D;
end;

procedure Int128ReverseBits(var A: Int128);
var B : Int128;
    I : Integer;
begin
  Int128InitZero(B);
  for I := 0 to 127 do
    if Int128IsBitSet(A, I) then
      Int128SetBit(B, 127 - I);
  A := B;
end;

procedure Int128ReverseBytes(var A: Int128);
var I : Integer;
    B : Byte;
begin
  for I := 0 to 15 do
    begin
      B := A.Bytes[I];
      A.Bytes[I] := A.Bytes[15 - I];
      A.Bytes[15 - I] := B;
    end;
end;

function Int128BitCount(const A: Int128): Integer;
var I : Integer;
begin
  Result := 0;
  for I := 0 to 15 do
    Inc(Result, Word8BitCountTable[A.Bytes[I]]);
end;

function Int128IsPowerOfTwo(const A: Int128): Boolean;
begin
  Result := not Int128IsNegative(A) and (Int128BitCount(A) = 1);
end;

{$IFDEF ASM386_DELPHI}
// Assembly version is optimised version of the Pascal version below.
// It avoids using a Int64 value by using the carry flag and the "adc"
// instruction. It uses the overflow flag to check for overflow errors.
procedure Int128AddWord32(var A: Int128; const B: LongWord);
asm
    add [eax], edx
    jnc @Fin
    adc dword ptr [eax + 4], 0
    adc dword ptr [eax + 8], 0
    adc dword ptr [eax + 12], 0
    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
    {$ENDIF}
  @Fin:
end;
{$ELSE}
procedure Int128AddWord32(var A: Int128; const B: LongWord);
var C : Int64;
    {$IFOPT Q+}
    P : Boolean;
    {$ENDIF}
begin
  {$IFOPT Q+}
  P := A.LongWords[3] and $80000000 = 0;
  {$ENDIF}
  // Do 32-bit addition in 64-bit value with carry in bit 33
  C := Int64(A.LongWords[0]) + Int64(B);
  A.LongWords[0] := Int64Rec(C).LongWords[0];
  if Int64Rec(C).LongWords[1] = 0 then exit;
  C := Int64(A.LongWords[1]) + 1;
  A.LongWords[1] := Int64Rec(C).LongWords[0];
  if Int64Rec(C).LongWords[1] = 0 then exit;
  C := Int64(A.LongWords[2]) + 1;
  A.LongWords[2] := Int64Rec(C).LongWords[0];
  if Int64Rec(C).LongWords[1] = 0 then exit;
  C := Int64(A.LongWords[3]) + 1;
  A.LongWords[3] := Int64Rec(C).LongWords[0];
  {$IFOPT Q+}
  // Overflow if sign changed from positive to negative
  if P and (A.LongWords[3] and $80000000 <> 0) then
    RaiseOverflowError;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure Int128AddWord64(var A: Int128; const B: Word64);
asm
    mov ecx, [eax]
    add ecx, [edx]
    mov [eax], ecx
    mov ecx, [eax + 4]
    adc ecx, [edx + 4]
    mov [eax + 4], ecx
    jnc @Fin
    adc dword ptr [eax + 8], 0
    adc dword ptr [eax + 12], 0
    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
    {$ENDIF}
  @Fin:
end;
{$ELSE}
procedure Int128AddWord64(var A: Int128; const B: Word64);
var C : LongWord;
    D : Integer;
    {$IFOPT Q+}
    P : Boolean;
    {$ENDIF}
begin
  {$IFOPT Q+}
  P := A.LongWords[3] and $80000000 = 0;
  {$ENDIF}
  // Do 16-bit addition in 32-bit value with carry in bit 17
  C := LongWord(A.Words[0]) + B.Words[0];
  A.Words[0] := Word(C);
  for D := 1 to 3 do
    begin
      C := C shr 16;
      Inc(C, A.Words[D]);
      Inc(C, B.Words[D]);
      A.Words[D] := Word(C);
    end;
  for D := 4 to 7 do
    begin
      C := C shr 16;
      if C = 0 then exit;
      Inc(C, A.Words[D]);
      A.Words[D] := Word(C);
    end;
  {$IFOPT Q+}
  // Overflow if sign changed from positive to negative
  if P and (A.LongWords[3] and $80000000 <> 0) then
    RaiseOverflowError;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure Int128AddWord128(var A: Int128; const B: Word128);
asm
    mov ecx, [eax]
    add ecx, [edx]
    mov [eax], ecx
    mov ecx, [eax + 4]
    adc ecx, [edx + 4]
    mov [eax + 4], ecx
    mov ecx, [eax + 8]
    adc ecx, [edx + 8]
    mov [eax + 8], ecx
    mov ecx, [eax + 12]
    adc ecx, [edx + 12]
    mov [eax + 12], ecx
    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
  @Fin:
    {$ENDIF}
end;
{$ELSE}
procedure Int128AddWord128(var A: Int128; const B: Word128);
var C : LongWord;
    D : Integer;
    {$IFOPT Q+}
    P : Boolean;
    Q : Boolean;
    {$ENDIF}
begin
  {$IFOPT Q+}
  P := A.LongWords[3] and $80000000 = 0;
  {$ENDIF}
  // Do 16-bit addition in 32-bit value with carry in bit 17
  C := LongWord(A.Words[0]) + B.Words[0];
  A.Words[0] := Word(C and $FFFF);
  for D := 1 to 7 do
    begin
      C := C shr 16;
      Inc(C, A.Words[D]);
      Inc(C, B.Words[D]);
      A.Words[D] := Word(C and $FFFF);
    end;
  {$IFOPT Q+}
  // Overflow if sign changed from positive to negative
  Q := A.LongWords[3] and $80000000 = 0;
  if P and not Q then
    RaiseOverflowError;
  // Overflow if sign stayed negative and B high bit set
  if not P and not Q and (B.LongWords[3] and $80000000 = 1) then
    RaiseOverflowError;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
// Assembly version is optimised version of the Pascal version below.
// It avoids using a Int64 value by using the carry flag and the "sbb"
// instruction. It uses the overflow flag to check for overflow errors.
procedure Int128SubtractWord32(var A: Int128; const B: LongWord);
asm
    sub [eax], edx
    jnc @Fin
    sbb dword ptr [eax + 4], 0
    sbb dword ptr [eax + 8], 0
    sbb dword ptr [eax + 12], 0
    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
    {$ENDIF}
  @Fin:
end;
{$ELSE}
procedure Int128SubtractWord32(var A: Int128; const B: LongWord);
var C : Int64;
    {$IFOPT Q+}
    N : Boolean;
    {$ENDIF}
begin
  {$IFOPT Q+}
  N := A.LongWords[3] and $80000000 <> 0;
  {$ENDIF}
  // Do 32-bit subtraction in 64-bit value with borrow in bit 33
  C := Int64(A.LongWords[0]) - Int64(B);
  A.LongWords[0] := Int64Rec(C).LongWords[0];
  if Int64Rec(C).LongWords[1] = 0 then exit;
  C := Int64(A.LongWords[1]) - 1;
  A.LongWords[1] := Int64Rec(C).LongWords[0];
  if Int64Rec(C).LongWords[1] = 0 then exit;
  C := Int64(A.LongWords[2]) - 1;
  A.LongWords[2] := Int64Rec(C).LongWords[0];
  if Int64Rec(C).LongWords[1] = 0 then exit;
  C := Int64(A.LongWords[3]) - 1;
  A.LongWords[3] := Int64Rec(C).LongWords[0];
  {$IFOPT Q+}
  // Overflow if sign changed from negative to positive
  if N and (A.LongWords[3] and $80000000 = 0) then
    RaiseOverflowError;
  {$ENDIF}
end;
{$ENDIF}

procedure Int128SubtractWord64(var A: Int128; const B: Word64);
var C, D : Integer;
    {$IFOPT Q+}
    N    : Boolean;
    {$ENDIF}
begin
  {$IFOPT Q+}
  N := A.LongWords[3] and $80000000 <> 0;
  {$ENDIF}
  // Do 16-bit subtraction in 32-bit value with borrow in bit 17
  C := A.Words[0];
  Dec(C, B.Words[0]);
  A.Words[0] := Word(C);
  for D := 1 to 3 do
    begin
      if C < 0 then C := -1 else C := 0;
      Inc(C, A.Words[D]);
      Dec(C, B.Words[D]);
      A.Words[D] := Word(C);
    end;
  if C < 0 then C := -1 else exit;
  Inc(C, A.Words[4]);
  A.Words[4] := Word(C);
  for D := 5 to 7 do
    begin
      if C >= 0 then exit;
      Inc(C, A.Words[D]);
      A.Words[D] := Word(C);
    end;
  {$IFOPT Q+}
  // Overflow if sign changed from negative to positive
  if N and (A.LongWords[3] and $80000000 = 0) then
    RaiseOverflowError;
  {$ENDIF}
end;

procedure Int128SubtractWord128(var A: Int128; const B: Word128);
var C, D : Integer;
    {$IFOPT Q+}
    N, Q : Boolean;
    {$ENDIF}
begin
  {$IFOPT Q+}
  N := A.LongWords[3] and $80000000 <> 0;
  {$ENDIF}
  C := A.Words[0];
  Dec(C, B.Words[0]);
  A.Words[0] := Word(C);
  for D := 1 to 7 do
    begin
      if C < 0 then C := -1 else C := 0;
      Inc(C, A.Words[D]);
      Dec(C, B.Words[D]);
      A.Words[D] := Word(C);
    end;
  {$IFOPT Q+}
  // Overflow if sign changed from negative to positive
  Q := A.LongWords[3] and $80000000 <> 0;
  if N and not Q then
    RaiseOverflowError;
  // Overflow if sign stayed positive and B high bit set
  if not N and not Q and (B.LongWords[3] and $80000000 = 1) then
    RaiseOverflowError;
  {$ENDIF}
end;

{$IFDEF ASM386_DELPHI}
procedure Int128AddInt32(var A: Int128; const B: LongInt);
asm
    or edx, edx
    js @Negative
    add [eax], edx
    adc dword ptr [eax + 4], 0
    adc dword ptr [eax + 8], 0
    adc dword ptr [eax + 12], 0
    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
    {$ENDIF}
    jmp @Fin
  @Negative:
    neg edx
    sub [eax], edx
    sbb dword ptr [eax + 4], 0
    sbb dword ptr [eax + 8], 0
    sbb dword ptr [eax + 12], 0
    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
    {$ENDIF}
  @Fin:
end;
{$ELSE}
procedure Int128AddInt32(var A: Int128; const B: LongInt);
begin
  if B > 0 then
    Int128AddWord32(A, LongWord(B)) else
  if B < 0 then
    Int128SubtractWord32(A, LongWord(-Int64(B)));
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure Int128SubtractInt32(var A: Int128; const B: LongInt);
asm
    or edx, edx
    js @Negative
    sub [eax], edx
    sbb dword ptr [eax + 4], 0
    sbb dword ptr [eax + 8], 0
    sbb dword ptr [eax + 12], 0
    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
    {$ENDIF}
    jmp @Fin
  @Negative:
    neg edx
    add [eax], edx
    adc dword ptr [eax + 4], 0
    adc dword ptr [eax + 8], 0
    adc dword ptr [eax + 12], 0
    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
    {$ENDIF}
  @Fin:
end;
{$ELSE}
procedure Int128SubtractInt32(var A: Int128; const B: LongInt);
begin
  if B > 0 then
    Int128SubtractWord32(A, LongWord(B)) else
  if B < 0 then
    Int128AddWord32(A, LongWord(-B));
end;
{$ENDIF}

procedure Int128AddInt64(var A: Int128; const B: Int64);
var C : Word64;
begin
  if B > 0 then
    begin
      C.LongWords[0] := Int64Rec(B).LongWords[0];
      C.LongWords[1] := Int64Rec(B).LongWords[1];
      Int128AddWord64(A, C);
    end else
  if B < 0 then
    begin
      if B = MinInt64 then
        begin
          C.LongWords[0] := $00000000;
          C.LongWords[1] := $80000000;
        end
      else
        Word64InitInt64(C, -B);
      Int128SubtractWord64(A, C);
    end;
end;

procedure Int128SubtractInt64(var A: Int128; const B: Int64);
var C : Word64;
begin
  if B > 0 then
    begin
      C.LongWords[0] := Int64Rec(B).LongWords[0];
      C.LongWords[1] := Int64Rec(B).LongWords[1];
      Int128SubtractWord64(A, C);
    end else
  if B < 0 then
    begin
      if B = MinInt64 then
        begin
          C.LongWords[0] := $00000000;
          C.LongWords[1] := $80000000;
        end
      else
        Word64InitInt64(C, -B);
      Int128AddWord64(A, C);
    end;
end;

{$IFDEF ASM386_DELPHI}
// Assembly version is optimised version of the Pascal version below.
// It avoids using a Int64 value by using the carry flag and the "adc"
// instruction. It uses the overflow flag to check for overflow errors.
procedure Int128AddInt128(var A: Int128; const B: Int128);
asm
    mov ecx, [edx]
    add [eax], ecx
    mov ecx, [edx + 4]
    adc [eax + 4], ecx
    mov ecx, [edx + 8]
    adc [eax + 8], ecx
    mov ecx, [edx + 12]
    adc [eax + 12], ecx
    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
  @Fin:
    {$ENDIF}
end;
{$ELSE}
procedure Int128AddInt128(var A: Int128; const B: Int128);
var C    : Int64;
    {$IFOPT Q+}
    D, E : Boolean;
    {$ENDIF}
begin
  {$IFOPT Q+}
  D := A.LongWords[3] and $80000000 = 0;
  E := B.LongWords[3] and $80000000 = 0;
  {$ENDIF}
  // Do 32-bit addition in 64-bit value with borrow in bit 33
  C := Int64(A.LongWords[0]) + Int64(B.LongWords[0]);
  A.LongWords[0] := Int64Rec(C).LongWords[0];
  C := Int64(A.LongWords[1]) + Int64(B.LongWords[1]) + Int64Rec(C).LongWords[1];
  A.LongWords[1] := Int64Rec(C).LongWords[0];
  C := Int64(A.LongWords[2]) + Int64(B.LongWords[2]) + Int64Rec(C).LongWords[1];
  A.LongWords[2] := Int64Rec(C).LongWords[0];
  C := Int64(A.LongWords[3]) + Int64(B.LongWords[3]) + Int64Rec(C).LongWords[1];
  A.LongWords[3] := Int64Rec(C).LongWords[0];
  {$IFOPT Q+}
  // Check overflow
  if A.LongWords[3] and $80000000 <> 0 then
    begin
      if D and E then
        RaiseOverflowError;
    end
  else
    if not D and not E then
      RaiseOverflowError;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
// Assembly version is optimised version of the Pascal version below.
// It avoids using a Int64 value by using the carry flag and the "adc"
// instruction. It uses the overflow flag to check for overflow errors.
procedure Int128SubtractInt128(var A: Int128; const B: Int128);
asm
    mov ecx, [edx]
    sub [eax], ecx
    mov ecx, [edx + 4]
    sbb [eax + 4], ecx
    mov ecx, [edx + 8]
    sbb [eax + 8], ecx
    mov ecx, [edx + 12]
    sbb [eax + 12], ecx
    {$IFOPT Q+}
    jno @Fin
    jmp RaiseOverflowError
  @Fin:
    {$ENDIF}
end;
{$ELSE}
procedure Int128SubtractInt128(var A: Int128; const B: Int128);
var C : Int64;
    {$IFOPT Q+}
    D, E : Boolean;
    {$ENDIF}
begin
  {$IFOPT Q+}
  D := A.LongWords[3] and $80000000 = 0;
  E := B.LongWords[3] and $80000000 = 0;
  {$ENDIF}
  // Do 32-bit subtraction in 64-bit value with borrow in bit 33
  C := Int64(A.LongWords[0]) - Int64(B.LongWords[0]);
  A.LongWords[0] := Int64Rec(C).LongWords[0];
  if C < 0 then C := 1 else C := 0;
  C := Int64(A.LongWords[1]) - Int64(B.LongWords[1]) - C;
  A.LongWords[1] := Int64Rec(C).LongWords[0];
  if C < 0 then C := 1 else C := 0;
  C := Int64(A.LongWords[2]) - Int64(B.LongWords[2]) - C;
  A.LongWords[2] := Int64Rec(C).LongWords[0];
  if C < 0 then C := 1 else C := 0;
  C := Int64(A.LongWords[3]) - Int64(B.LongWords[3]) - C;
  A.LongWords[3] := Int64Rec(C).LongWords[0];
  {$IFOPT Q+}
  // Check overflow
  if A.LongWords[3] and $80000000 <> 0 then
    begin
      if D and not E then
        RaiseOverflowError;
    end
  else
    if not D and E then
      RaiseOverflowError;
  {$ENDIF}
end;
{$ENDIF}

procedure Int128MultiplyWord8(var A: Int128; const B: Byte);
var C : Word128;
    N : Boolean;
begin
  N := Int128Abs(A, C);
  Word128MultiplyWord8(C, B);
  if not N then
    Int128InitWord128(A, C)
  else
    Int128InitNegWord128(A, C);
end;

procedure Int128MultiplyWord16(var A: Int128; const B: Word);
var C : Word128;
    N : Boolean;
begin
  N := Int128Abs(A, C);
  Word128MultiplyWord16(C, B);
  if not N then
    Int128InitWord128(A, C)
  else
    Int128InitNegWord128(A, C);
end;

procedure Int128MultiplyWord32(var A: Int128; const B: LongWord);
var C : Word128;
    N : Boolean;
begin
  N := Int128Abs(A, C);
  Word128MultiplyWord32(C, B);
  if not N then
    Int128InitWord128(A, C)
  else
    Int128InitNegWord128(A, C);
end;

procedure Int128MultiplyWord64(var A: Int128; const B: Word64);
var C : Word128;
    N : Boolean;
begin
  N := Int128Abs(A, C);
  Word128MultiplyWord64(C, B);
  if not N then
    Int128InitWord128(A, C)
  else
    Int128InitNegWord128(A, C);
end;

procedure Int128MultiplyWord128(var A: Int128; const B: Word128);
var C : Word128;
    N : Boolean;
begin
  N := Int128Abs(A, C);
  Word128MultiplyWord128Low(C, B);
  if not N then
    Int128InitWord128(A, C)
  else
    Int128InitNegWord128(A, C);
end;

procedure Int128MultiplyInt8(var A: Int128; const B: ShortInt);
var C    : Word128;
    D    : Byte;
    P, Q : Boolean;
begin
  P := Int128Abs(A, C);
  if B >= 0 then
    begin
      Q := False;
      D := Byte(B);
    end
  else
    begin
      Q := True;
      D := Byte(-SmallInt(B));
    end;
  Word128MultiplyWord8(C, D);
  if not (P xor Q) then
    Int128InitWord128(A, C)
  else
    Int128InitNegWord128(A, C);
end;

procedure Int128MultiplyInt16(var A: Int128; const B: SmallInt);
var C    : Word128;
    D    : Word;
    P, Q : Boolean;
begin
  P := Int128Abs(A, C);
  if B >= 0 then
    begin
      Q := False;
      D := Word(B);
    end
  else
    begin
      Q := True;
      D := Word(-LongInt(B));
    end;
  Word128MultiplyWord16(C, D);
  if not (P xor Q) then
    Int128InitWord128(A, C)
  else
    Int128InitNegWord128(A, C);
end;

procedure Int128MultiplyInt32(var A: Int128; const B: LongInt);
var C    : Word128;
    D    : LongWord;
    P, Q : Boolean;
begin
  P := Int128Abs(A, C);
  if B >= 0 then
    begin
      Q := False;
      D := LongWord(B);
    end
  else
    begin
      Q := True;
      D := LongWord(-Int64(B));
    end;
  Word128MultiplyWord32(C, D);
  if not (P xor Q) then
    Int128InitWord128(A, C)
  else
    Int128InitNegWord128(A, C);
end;

procedure Int128MultiplyInt64(var A: Int128; const B: Int64);
var C    : Word128;
    D    : Word64;
    P, Q : Boolean;
begin
  P := Int128Abs(A, C);
  Q := Int64Abs(B, D);
  Word128MultiplyWord64(C, D);
  if not (P xor Q) then
    Int128InitWord128(A, C)
  else
    Int128InitNegWord128(A, C);
end;

procedure Int128MultiplyInt128(var A: Int128; const B: Int128);
var C, D : Word128;
    P, Q : Boolean;
begin
  P := Int128Abs(A, C);
  Q := Int128Abs(B, D);
  Word128MultiplyWord128Low(C, D);
  if not (P xor Q) then
    Int128InitWord128(A, C)
  else
    Int128InitNegWord128(A, C);
end;

procedure Int128Sqr(var A: Int128);
begin
  Int128MultiplyInt128(A, A);
end;

procedure Int128DivideWord8(const A: Int128; const B: Byte; var Q: Int128; var R: Byte);
var C, T : Word128;
    N    : Boolean;
begin
  N := Int128Abs(A, C);
  Word128DivideWord8(C, B, T, R);
  if not N then
    Int128InitWord128(Q, T)
  else
    Int128InitNegWord128(Q, T);
end;

procedure Int128DivideWord16(const A: Int128; const B: Word; var Q: Int128; var R: Word);
var C, T : Word128;
    N    : Boolean;
begin
  N := Int128Abs(A, C);
  Word128DivideWord16(C, B, T, R);
  if not N then
    Int128InitWord128(Q, T)
  else
    Int128InitNegWord128(Q, T);
end;

procedure Int128DivideWord32(const A: Int128; const B: LongWord; var Q: Int128; var R: LongWord);
var C, T : Word128;
    N    : Boolean;
begin
  N := Int128Abs(A, C);
  Word128DivideWord32(C, B, T, R);
  if not N then
    Int128InitWord128(Q, T)
  else
    Int128InitNegWord128(Q, T);
end;

procedure Int128DivideWord64(const A: Int128; const B: Word64; var Q: Int128; var R: Word64);
var C, T : Word128;
    N    : Boolean;
begin
  N := Int128Abs(A, C);
  Word128DivideWord64(C, B, T, R);
  if not N then
    Int128InitWord128(Q, T)
  else
    Int128InitNegWord128(Q, T);
end;

procedure Int128DivideWord128(const A: Int128; const B: Word128; var Q: Int128; var R: Word128);
var C, T : Word128;
    N    : Boolean;
begin
  N := Int128Abs(A, C);
  Word128DivideWord128(C, B, T, R);
  if not N then
    Int128InitWord128(Q, T)
  else
    Int128InitNegWord128(Q, T);
end;

procedure Int128DivideInt8(const A: Int128; const B: ShortInt; var Q: Int128; var R: ShortInt);
var C, T : Word128;
    D, E : Byte;
    F, G : Boolean;
begin
  F := Int128Abs(A, C);
  G := Int8Abs(B, D);
  Word128DivideWord8(C, D, T, E);
  if not (F xor G) then
    Int128InitWord128(Q, T)
  else
    Int128InitNegWord128(Q, T);
  R := ShortInt(E);
end;

procedure Int128DivideInt16(const A: Int128; const B: SmallInt; var Q: Int128; var R: SmallInt);
var C, T : Word128;
    D, E : Word;
    F, G : Boolean;
begin
  F := Int128Abs(A, C);
  G := Int16Abs(B, D);
  Word128DivideWord16(C, D, T, E);
  if not (F xor G) then
    Int128InitWord128(Q, T)
  else
    Int128InitNegWord128(Q, T);
  R := SmallInt(E);
end;

procedure Int128DivideInt32(const A: Int128; const B: LongInt; var Q: Int128; var R: LongInt);
var C, T : Word128;
    D, E : LongWord;
    F, G : Boolean;
begin
  F := Int128Abs(A, C);
  G := Int32Abs(B, D);
  Word128DivideWord32(C, D, T, E);
  if not (F xor G) then
    Int128InitWord128(Q, T)
  else
    Int128InitNegWord128(Q, T);
  R := LongInt(E);
end;

procedure Int128DivideInt64(const A: Int128; const B: Int64; var Q: Int128; var R: Int64);
var C, T : Word128;
    D, E : Word64;
    F, G : Boolean;
begin
  F := Int128Abs(A, C);
  G := Int64Abs(B, D);
  Word128DivideWord64(C, D, T, E);
  if not (F xor G) then
    Int128InitWord128(Q, T)
  else
    Int128InitNegWord128(Q, T);
  R := Word64ToInt64(E);
end;

procedure Int128DivideInt128(const A, B: Int128; var Q, R: Int128);
var C, T : Word128;
    D, E : Word128;
    F, G : Boolean;
begin
  F := Int128Abs(A, C);
  G := Int128Abs(B, D);
  Word128DivideWord128(C, D, T, E);
  if not (F xor G) then
    Int128InitWord128(Q, T)
  else
    Int128InitNegWord128(Q, T);
  Int128InitWord128(R, E);
end;

function Int128ToStr(const A: Int128): String;
var B : Word128;
    N : Boolean;
    S : ShortString;
    I, L, K : Integer;
begin
  N := Int128Abs(A, B);
  if N then
    K := 1
  else
    K := 0;
  Word128ToShortStrR(B, S);
  L := Length(S);
  SetLength(Result, L + K);
  for I := 1 to L do
    Result[I + K] := Char(S[L - I + 1]);
  if N then
    Result[1] := '-';
end;

function Int128ToStrA(const A: Int128): AnsiString;
var B : Word128;
    N : Boolean;
    S : ShortString;
    I, L, K : Integer;
begin
  N := Int128Abs(A, B);
  if N then
    K := 1
  else
    K := 0;
  Word128ToShortStrR(B, S);
  L := Length(S);
  SetLength(Result, L + K);
  for I := 1 to L do
    Result[I + K] := AnsiChar(S[L - I + 1]);
  if N then
    Result[1] := '-';
end;

function Int128ToStrW(const A: Int128): WideString;
var B : Word128;
    N : Boolean;
    S : ShortString;
    I, L, K : Integer;
begin
  N := Int128Abs(A, B);
  if N then
    K := 1
  else
    K := 0;
  Word128ToShortStrR(B, S);
  L := Length(S);
  SetLength(Result, L + K);
  for I := 1 to L do
    Result[I + K] := WideChar(S[L - I + 1]);
  if N then
    Result[1] := '-';
end;

function Int128ToStrU(const A: Int128): UnicodeString;
var B : Word128;
    N : Boolean;
    S : ShortString;
    I, L, K : Integer;
begin
  N := Int128Abs(A, B);
  if N then
    K := 1
  else
    K := 0;
  Word128ToShortStrR(B, S);
  L := Length(S);
  SetLength(Result, L + K);
  for I := 1 to L do
    Result[I + K] := WideChar(S[L - I + 1]);
  if N then
    Result[1] := '-';
end;

function StrToInt128(const A: String): Int128;
var I : Integer;
    B : Char;
    C : LongWord;
    D : Integer;
    N : Boolean;
begin
  if A = '' then
    RaiseConvertError;
  N := (A[1] = '-');
  if N then
    D := 1
  else
    D := 0;
  Int128InitZero(Result);
  for I := 1 + D to Length(A) do
    begin
      B := A[I];
      if Ord(B) > $7F then
        RaiseConvertError;
      if not (AnsiChar(Ord(B)) in ['0'..'9']) then
        RaiseConvertError;
      C := Ord(A[I]) - Ord('0');
      Int128MultiplyWord8(Result, 10);
      Int128AddWord32(Result, C);
    end;
  if N then
    Int128Negate(Result);
end;

function StrToInt128A(const A: AnsiString): Int128;
var I : Integer;
    B : AnsiChar;
    C : LongWord;
    D : Integer;
    N : Boolean;
begin
  if A = '' then
    RaiseConvertError;
  N := (A[1] = '-');
  if N then
    D := 1
  else
    D := 0;
  Int128InitZero(Result);
  for I := 1 + D to Length(A) do
    begin
      B := A[I];
      if not (B in ['0'..'9']) then
        RaiseConvertError;
      C := Ord(A[I]) - Ord('0');
      Int128MultiplyWord8(Result, 10);
      Int128AddWord32(Result, C);
    end;
  if N then
    Int128Negate(Result);
end;

function StrToInt128W(const A: WideString): Int128;
var I : Integer;
    B : WideChar;
    C : LongWord;
    D : Integer;
    N : Boolean;
begin
  if A = '' then
    RaiseConvertError;
  N := (A[1] = '-');
  if N then
    D := 1
  else
    D := 0;
  Int128InitZero(Result);
  for I := 1 + D to Length(A) do
    begin
      B := A[I];
      if Ord(B) > $7F then
        RaiseConvertError;
      if not (AnsiChar(Ord(B)) in ['0'..'9']) then
        RaiseConvertError;
      C := Ord(A[I]) - Ord('0');
      Int128MultiplyWord8(Result, 10);
      Int128AddWord32(Result, C);
    end;
  if N then
    Int128Negate(Result);
end;

function StrToInt128U(const A: UnicodeString): Int128;
var I : Integer;
    B : WideChar;
    C : LongWord;
    D : Integer;
    N : Boolean;
begin
  if A = '' then
    RaiseConvertError;
  N := (A[1] = '-');
  if N then
    D := 1
  else
    D := 0;
  Int128InitZero(Result);
  for I := 1 + D to Length(A) do
    begin
      B := A[I];
      if Ord(B) > $7F then
        RaiseConvertError;
      if not (AnsiChar(Ord(B)) in ['0'..'9']) then
        RaiseConvertError;
      C := Ord(A[I]) - Ord('0');
      Int128MultiplyWord8(Result, 10);
      Int128AddWord32(Result, C);
    end;
  if N then
    Int128Negate(Result);
end;

procedure Int128ISqrt(var A: Int128);
var B : Word128;
begin
  if A.LongWords[3] and $80000000 <> 0 then
    RaiseInvalidOpError;
  B := Int128ToWord128(A);
  Word128ISqrt(B);
  Int128InitWord128(A, B);
end;

function Int128Sqrt(const A: Int128): Extended;
begin
  if A.LongWords[3] and $80000000 <> 0 then
    RaiseInvalidOpError;
  Result := Word128Sqrt(Int128ToWord128(A));
end;

procedure Int128Power(var A: Int128; const B: LongWord);
var C : Word128;
    N : Boolean;
begin
  N := Int128Abs(A, C);
  Word128Power(C, B);
  if N and (B and 1 <> 0) then
    Int128InitNegWord128(A, C)
  else
    Int128InitWord128(A, C);
end;

function Int128Hash(const A: Int128): LongWord;
begin
  Result := Word32Hash(A.LongWords[0]) xor
            Word32Hash(A.LongWords[1]) xor
            Word32Hash(A.LongWords[2]) xor
            Word32Hash(A.LongWords[3]);
end;



{                                                                              }
{ Int256                                                                       }
{                                                                              }
procedure Int256InitZero(var A: Int256);
begin
  A.LongWords[0] := $00000000;
  A.LongWords[1] := $00000000;
  A.LongWords[2] := $00000000;
  A.LongWords[3] := $00000000;
  A.LongWords[4] := $00000000;
  A.LongWords[5] := $00000000;
  A.LongWords[6] := $00000000;
  A.LongWords[7] := $00000000;
end;

procedure Int256InitOne(var A: Int256);
begin
  A.LongWords[0] := $00000001;
  A.LongWords[1] := $00000000;
  A.LongWords[2] := $00000000;
  A.LongWords[3] := $00000000;
  A.LongWords[4] := $00000000;
  A.LongWords[5] := $00000000;
  A.LongWords[6] := $00000000;
  A.LongWords[7] := $00000000;
end;

procedure Int256InitMinusOne(var A: Int256);
begin
  A.LongWords[0] := $FFFFFFFF;
  A.LongWords[1] := $FFFFFFFF;
  A.LongWords[2] := $FFFFFFFF;
  A.LongWords[3] := $FFFFFFFF;
  A.LongWords[4] := $FFFFFFFF;
  A.LongWords[5] := $FFFFFFFF;
  A.LongWords[6] := $FFFFFFFF;
  A.LongWords[7] := $FFFFFFFF;
end;

procedure Int256InitMinimum(var A: Int256);
begin
  A.LongWords[0] := $00000000;
  A.LongWords[1] := $00000000;
  A.LongWords[2] := $00000000;
  A.LongWords[3] := $00000000;
  A.LongWords[4] := $00000000;
  A.LongWords[5] := $00000000;
  A.LongWords[6] := $00000000;
  A.LongWords[7] := $80000000;
end;

procedure Int256InitMaximum(var A: Int256);
begin
  A.LongWords[0] := $FFFFFFFF;
  A.LongWords[1] := $FFFFFFFF;
  A.LongWords[2] := $FFFFFFFF;
  A.LongWords[3] := $FFFFFFFF;
  A.LongWords[4] := $FFFFFFFF;
  A.LongWords[5] := $FFFFFFFF;
  A.LongWords[6] := $FFFFFFFF;
  A.LongWords[7] := $7FFFFFFF;
end;

function Int256IsNegative(const A: Int256): Boolean;
begin
  Result := A.LongWords[7] and $80000000 <> 0;
end;

function Int256IsZero(const A: Int256): Boolean;
begin
  Result :=
      (A.LongWords[0] = 0) and
      (A.LongWords[1] = 0) and
      (A.LongWords[2] = 0) and
      (A.LongWords[3] = 0) and
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

function Int256IsOne(const A: Int256): Boolean;
begin
  Result :=
      (A.LongWords[0] = 1) and
      (A.LongWords[1] = 0) and
      (A.LongWords[2] = 0) and
      (A.LongWords[3] = 0) and
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

function Int256IsMinusOne(const A: Int256): Boolean;
begin
  Result :=
      (A.LongWords[0] = $FFFFFFFF) and
      (A.LongWords[1] = $FFFFFFFF) and
      (A.LongWords[2] = $FFFFFFFF) and
      (A.LongWords[3] = $FFFFFFFF) and
      (A.LongWords[4] = $FFFFFFFF) and
      (A.LongWords[5] = $FFFFFFFF) and
      (A.LongWords[6] = $FFFFFFFF) and
      (A.LongWords[7] = $FFFFFFFF);
end;

function Int256IsMinimum(const A: Int256): Boolean;
begin
  Result :=
      (A.LongWords[0] = 0) and
      (A.LongWords[1] = 0) and
      (A.LongWords[2] = 0) and
      (A.LongWords[3] = 0) and
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = $80000000);
end;

function Int256IsMaximum(const A: Int256): Boolean;
begin
  Result :=
      (A.LongWords[0] = $FFFFFFFF) and
      (A.LongWords[1] = $FFFFFFFF) and
      (A.LongWords[2] = $FFFFFFFF) and
      (A.LongWords[3] = $FFFFFFFF) and
      (A.LongWords[4] = $FFFFFFFF) and
      (A.LongWords[5] = $FFFFFFFF) and
      (A.LongWords[6] = $FFFFFFFF) and
      (A.LongWords[7] = $7FFFFFFF);
end;

function Int256IsOdd(const A: Int256): Boolean;
begin
  Result := A.LongWords[0] and 1 = 1;
end;

function Int256Sign(const A: Int256): Integer;
begin
  if A.LongWords[7] and $80000000 <> 0 then
    Result := -1 else
  if (A.LongWords[0] = 0) and
     (A.LongWords[1] = 0) and
     (A.LongWords[2] = 0) and
     (A.LongWords[3] = 0) and
     (A.LongWords[4] = 0) and
     (A.LongWords[5] = 0) and
     (A.LongWords[6] = 0) and
     (A.LongWords[7] = 0) then
    Result := 0
  else
    Result := 1;
end;

function Int256IsWord8Range(const A: Int256): Boolean;
begin
  Result :=
      (A.LongWords[0] <= MaxWord8) and
      (A.LongWords[1] = 0) and
      (A.LongWords[2] = 0) and
      (A.LongWords[3] = 0) and
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

function Int256IsWord16Range(const A: Int256): Boolean;
begin
  Result :=
      (A.LongWords[0] <= MaxWord16) and
      (A.LongWords[1] = 0) and
      (A.LongWords[2] = 0) and
      (A.LongWords[3] = 0) and
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

function Int256IsWord32Range(const A: Int256): Boolean;
begin
  Result :=
      (A.LongWords[1] = 0) and
      (A.LongWords[2] = 0) and
      (A.LongWords[3] = 0) and
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

function Int256IsWord64Range(const A: Int256): Boolean;
begin
  Result :=
      (A.LongWords[2] = 0) and
      (A.LongWords[3] = 0) and
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

function Int256IsWord128Range(const A: Int256): Boolean;
begin
  Result :=
      (A.LongWords[4] = 0) and
      (A.LongWords[5] = 0) and
      (A.LongWords[6] = 0) and
      (A.LongWords[7] = 0);
end;

function Int256IsWord256Range(const A: Int256): Boolean;
begin
  Result := (A.LongWords[7] and $80000000 = 0);
end;

function Int256IsInt8Range(const A: Int256): Boolean;
begin
  Result := Int256IsInt32Range(A);
  if not Result then
    exit;
  Result := Int32IsInt8Range(Int256ToInt32(A));
end;

function Int256IsInt16Range(const A: Int256): Boolean;
begin
  Result := Int256IsInt32Range(A);
  if not Result then
    exit;
  Result := Int32IsInt16Range(Int256ToInt32(A));
end;

function Int256IsInt32Range(const A: Int256): Boolean;
begin
  if A.LongWords[7] and $80000000 <> 0 then
    Result := (A.LongWords[0] >= $80000000) and
              (A.LongWords[1] = $FFFFFFFF) and
              (A.LongWords[2] = $FFFFFFFF) and
              (A.LongWords[3] = $FFFFFFFF) and
              (A.LongWords[4] = $FFFFFFFF) and
              (A.LongWords[5] = $FFFFFFFF) and
              (A.LongWords[6] = $FFFFFFFF) and
              (A.LongWords[7] = $FFFFFFFF)
  else
    Result := (A.LongWords[0] < $80000000) and
              (A.LongWords[1] = 0) and
              (A.LongWords[2] = 0) and
              (A.LongWords[3] = 0) and
              (A.LongWords[4] = 0) and
              (A.LongWords[5] = 0) and
              (A.LongWords[6] = 0) and
              (A.LongWords[7] = 0);
end;

function Int256IsInt64Range(const A: Int256): Boolean;
begin
  if A.LongWords[7] and $80000000 <> 0 then
    Result := (A.LongWords[1] >= $80000000) and
              (A.LongWords[2] = $FFFFFFFF) and
              (A.LongWords[3] = $FFFFFFFF) and
              (A.LongWords[4] = $FFFFFFFF) and
              (A.LongWords[5] = $FFFFFFFF) and
              (A.LongWords[6] = $FFFFFFFF) and
              (A.LongWords[7] = $FFFFFFFF)
  else
    Result := (A.LongWords[1] < $80000000) and
              (A.LongWords[2] = 0) and
              (A.LongWords[3] = 0) and
              (A.LongWords[4] = 0) and
              (A.LongWords[5] = 0) and
              (A.LongWords[6] = 0) and
              (A.LongWords[7] = 0);
end;

function Int256IsInt128Range(const A: Int256): Boolean;
begin
  if A.LongWords[7] and $80000000 <> 0 then
    Result := (A.LongWords[3] >= $80000000) and
              (A.LongWords[4] = $FFFFFFFF) and
              (A.LongWords[5] = $FFFFFFFF) and
              (A.LongWords[6] = $FFFFFFFF) and
              (A.LongWords[7] = $FFFFFFFF)
  else
    Result := (A.LongWords[3] < $80000000) and
              (A.LongWords[4] = 0) and
              (A.LongWords[5] = 0) and
              (A.LongWords[6] = 0) and
              (A.LongWords[7] = 0);
end;

procedure Int256InitWord32(var A: Int256; const B: LongWord);
begin
  A.LongWords[0] := B;
  A.LongWords[1] := 0;
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
  A.LongWords[4] := 0;
  A.LongWords[5] := 0;
  A.LongWords[6] := 0;
  A.LongWords[7] := 0;
end;

procedure Int256InitWord64(var A: Int256; const B: Word64);
begin
  A.LongWords[0] := B.LongWords[0];
  A.LongWords[1] := B.LongWords[1];
  A.LongWords[2] := 0;
  A.LongWords[3] := 0;
  A.LongWords[4] := 0;
  A.LongWords[5] := 0;
  A.LongWords[6] := 0;
  A.LongWords[7] := 0;
end;

procedure Int256InitWord128(var A: Int256; const B: Word128);
begin
  A.LongWords[0] := B.LongWords[0];
  A.LongWords[1] := B.LongWords[1];
  A.LongWords[2] := B.LongWords[2];
  A.LongWords[3] := B.LongWords[3];
  A.LongWords[4] := 0;
  A.LongWords[5] := 0;
  A.LongWords[6] := 0;
  A.LongWords[7] := 0;
end;

procedure Int256InitInt32(var A: Int256; const B: LongInt);
var C : LongWord;
begin
  A.LongInts[0] := B;
  if B < 0 then
    C := $FFFFFFFF
  else
    C := 0;
  A.LongWords[1] := C;
  A.LongWords[2] := C;
  A.LongWords[3] := C;
  A.LongWords[4] := C;
  A.LongWords[5] := C;
  A.LongWords[6] := C;
  A.LongWords[7] := C;
end;

procedure Int256InitInt64(var A: Int256; const B: Int64);
var C : LongWord;
begin
  A.Int64s[0] := B;
  if B < 0 then
    C := $FFFFFFFF
  else
    C := 0;
  A.LongWords[2] := C;
  A.LongWords[3] := C;
  A.LongWords[4] := C;
  A.LongWords[5] := C;
  A.LongWords[6] := C;
  A.LongWords[7] := C;
end;

procedure Int256InitInt128(var A: Int256; const B: Int128);
var C : LongWord;
begin
  A.LongWords[0] := B.LongWords[0];
  A.LongWords[1] := B.LongWords[1];
  A.LongWords[2] := B.LongWords[2];
  A.LongWords[3] := B.LongWords[3];
  if B.LongWords[3] and $80000000 <> 0 then
    C := $FFFFFFFF
  else
    C := 0;
  A.LongWords[4] := C;
  A.LongWords[5] := C;
  A.LongWords[6] := C;
  A.LongWords[7] := C;
end;

function Int256ToWord32(const A: Int256): LongWord;
begin
  {$IFOPT R+}
  if (A.LongWords[1] <> 0) or
     (A.LongWords[2] <> 0) or
     (A.LongWords[3] <> 0) or
     (A.LongWords[4] <> 0) or
     (A.LongWords[5] <> 0) or
     (A.LongWords[6] <> 0) or
     (A.LongWords[7] <> 0) then
    RaiseRangeError;
  {$ENDIF}
  Result := A.LongWords[0];
end;

function Int256ToWord128(const A: Int256): Word128;
begin
  {$IFOPT R+}
  if (A.LongWords[4] <> 0) or
     (A.LongWords[5] <> 0) or
     (A.LongWords[6] <> 0) or
     (A.LongWords[7] <> 0) then
    RaiseRangeError;
  {$ENDIF}
  Result.Word64s[0] := A.Word64s[0];
  Result.Word64s[1] := A.Word64s[1];
end;

function Int256ToInt32(const A: Int256): LongInt;
begin
  {$IFOPT R+}
  if not Int256IsInt32Range(A) then
    RaiseRangeError;
  {$ENDIF}
  Result := A.LongInts[0];
end;

function Int256ToInt64(const A: Int256): Int64;
begin
  {$IFOPT R+}
  if not Int256IsInt64Range(A) then
    RaiseRangeError;
  {$ENDIF}
  Result := A.Int64s[0];
end;

function Int256ToInt128(const A: Int256): Int128;
begin
  {$IFOPT R+}
  if not Int256IsInt128Range(A) then
    RaiseRangeError;
  {$ENDIF}
  Result.Word64s[0] := A.Word64s[0];
  Result.Word64s[1] := A.Word64s[1];
end;



{                                                                              }
{ IntBuf                                                                       }
{                                                                              }
function IntBufInitSign(const Buf; const Sign: ShortInt): PLongWord; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert((Sign >= -1) and (Sign <= 1));

  S := @Buf;
  S^ := Sign;
  Inc(S);
  Result := PLongWord(S);
end;

procedure IntBufInitZero(const Buf; const Size: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  P := IntBufInitSign(Buf, 0);
  BufInitZero(P^, Size - SizeOf(ShortInt));
end;

procedure IntBufInitOne(const Buf; const Size: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  P := IntBufInitSign(Buf, 1);
  P^ := 1;
  Inc(P);
  BufInitZero(P^, Size - SizeOf(ShortInt) - SizeOf(LongWord));
end;

procedure IntBufInitMinusOne(const Buf; const Size: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  P := IntBufInitSign(Buf, -1);
  P^ := 1;
  Inc(P);
  BufInitZero(P^, Size - SizeOf(ShortInt) - SizeOf(LongWord));
end;

procedure IntBufInitMinimum(const Buf; const Size: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  S^ := -1;
  Inc(S);
  BufInitValue(S^, Size - SizeOf(ShortInt), $FFFFFFFF);
end;

procedure IntBufInitMaximum(const Buf; const Size: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  S^ := 1;
  Inc(S);
  BufInitValue(S^, Size - SizeOf(ShortInt), $FFFFFFFF);
end;

function IntBufIsNegative(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  Result := (S^ < 0);
end;

function IntBufIsZero(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  Result := (S^ = 0);
end;

function IntBufIsMinimum(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  if S^ >= 0 then
    begin
      Result := False;
      exit;
    end;
  Inc(S);
  Result := BufIsValue(S^, Size - SizeOf(ShortInt), $FFFFFFFF);
end;

function IntBufIsMaximum(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  if S^ <= 0 then
    begin
      Result := False;
      exit;
    end;
  Inc(S);
  Result := BufIsValue(S^, Size - SizeOf(ShortInt), $FFFFFFFF);
end;

function IntBufGetSign(const Buf; const Size: LongWord; var Data: PLongWord): ShortInt; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  Result := S^;
  Inc(S);
  Data := PLongWord(S);
end;

function IntBufIsOne(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  Result := IntBufGetSign(Buf, Size, P) > 0;
  if not Result then
    exit;

  Result := (P^ = 1);
  if not Result then
    exit;

  Result := BufIsZero(P^, Size - SizeOf(ShortInt) - SizeOf(LongWord));
end;

function IntBufIsMinusOne(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PLongWord;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  Result := IntBufGetSign(Buf, Size, P) < 0;
  if not Result then
    exit;

  Result := (P^ = 1);
  if not Result then
    exit;

  Result := BufIsZero(P^, Size - SizeOf(ShortInt) - SizeOf(LongWord));
end;

function IntBufIsOdd(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    P : PLongWord;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  Inc(S);
  P := PLongWord(S);
  Result := P^ and 1 <> 0;
end;

function IntBufIsWord32Range(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  if S^ < 0 then
    begin
      Result := False;
      exit;
    end;
  Inc(S);
  Result := WordBufIsWord32Range(S^, Size - SizeOf(ShortInt));
end;

function IntBufIsWord64Range(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  if S^ < 0 then
    begin
      Result := False;
      exit;
    end;
  Inc(S);
  Result := WordBufIsWord64Range(S^, Size - SizeOf(ShortInt));
end;

function IntBufIsWord128Range(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  if S^ < 0 then
    begin
      Result := False;
      exit;
    end;
  Inc(S);
  Result := WordBufIsWord128Range(S^, Size - SizeOf(ShortInt));
end;

function IntBufIsWord256Range(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  if S^ < 0 then
    begin
      Result := False;
      exit;
    end;
  Inc(S);
  Result := WordBufIsWord256Range(S^, Size - SizeOf(ShortInt));
end;

function IntBufIsInt32Range(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    T : ShortInt;
    P : PLongWord;
    V : LongWord;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  T := S^;
  Inc(S);
  P := PLongWord(S);
  V := P^;
  if ((T >= 0) and (V > $7FFFFFFF)) or
     ((T < 0) and (V > $80000000)) then
    begin
      Result := False;
      exit;
    end;
  Inc(P);
  Result := BufIsZero(P^, Size - SizeOf(ShortInt) - SizeOf(LongWord));
end;

function IntBufIsInt64Range(const Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    T : ShortInt;
    P : PWord64;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  T := S^;
  Inc(S);
  P := PWord64(S);
  if ((T >= 0) and (Word64CompareInt64(P^, $7FFFFFFFFFFFFFFF) > 0)) or
     ((T < 0) and (Word64CompareInt64(P^, $8000000000000000) > 0)) then
    begin
      Result := False;
      exit;
    end;
  Inc(P);
  Result := BufIsZero(P^, Size - SizeOf(ShortInt) - SizeOf(Word64));
end;

function IntBufSign(const Buf; const Size: LongWord): Integer; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  Result := S^;
  
  Assert((Result >= -1) and (Result <= 1));
end;

procedure IntBufNegate(var Buf; const Size: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  S^ := -S^;
end;

procedure IntBufInitNegWordBuf(var BufA; const BufB; const SizeB: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(SizeB > 0);
  Assert(SizeB mod 4 = 0);

  S := @BufA;
  S^ := -1;
  Inc(S);
  Move(BufB, S^, SizeB);
end;

function IntBufAbsInPlace(var Buf; const Size: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    T : ShortInt;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  T := S^;
  if T = 0 then
    begin
      Result := False;
      exit;
    end;
  S^ := 1;
  Result := True;
end;

function IntBufAbs(const BufA; const SizeA: LongWord; var BufB): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    T : ShortInt;
begin
  Assert(SizeA > 4);
  Assert(SizeA mod 4 = 1);

  Move(BufA, BufB, SizeA);
  S := @BufB;
  T := S^;
  if T < 0 then
    begin
      S^ := 1;
      Result := True;
    end
  else
    Result := False;
end;

procedure IntBufInitWord32(var BufA; const SizeA: LongWord; const B: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    P : PLongWord;
begin
  Assert(SizeA > 4);
  Assert(SizeA mod 4 = 1);

  S := @BufA;
  S^ := 1;
  Inc(S);
  P := PLongWord(S);
  P^ := B;
end;

procedure IntBufInitWord64(var BufA; const SizeA: LongWord; const B: Word64); {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(SizeA > 4);
  Assert(SizeA mod 4 = 1);

  S := @BufA;
  S^ := 1;
  Inc(S);
  if SizeA < SizeOf(ShortInt) + SizeOf(Word64) then
    begin
      {$IFOPT R+}
      if Word64Hi(B) > 0 then
        RaiseRangeError;
      {$ENDIF}
      PLongWord(S)^ := Word64Lo(B);
    end
  else
    PWord64(S)^ := B;
end;

procedure IntBufInitWord128(var BufA; const SizeA: LongWord; const B: Word128); {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  Assert(SizeA > 4);
  Assert(SizeA mod 4 = 1);

  S := @BufA;
  S^ := 1;
  Inc(S);
  if SizeA < SizeOf(ShortInt) + SizeOf(Word128) then
    begin
      {$IFOPT R+}
      {$ENDIF}
    end
  else
    PWord128(S)^ := B;
end;

procedure IntBufInitWordBuf(var BufA; const SizeA: LongWord; const BufB); {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    SizeB : LongWord;
begin
  Assert(SizeA > 4);
  Assert(SizeA mod 4 = 1);

  SizeB := SizeA - 1;
  S := @BufA;
  if WordBufIsZero(BufB, SizeB) then
    S^ := 0
  else
    S^ := 1;
  Inc(S);
  Move(BufB, S^, SizeB);
end;

procedure IntBufInitInt32(var BufA; const SizeA: LongWord; const B: LongInt); {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  if B = 0 then
    IntBufInitZero(BufA, SizeA)
  else
    begin
      S := @BufA;
      if B > 0 then
        begin
          S^ := 1;
          Inc(S);
          PLongInt(S)^ := B;
        end
      else
        begin
          S^ := -1;
          Inc(S);
          PLongInt(S)^ := -B;
        end;
    end;
end;

procedure IntBufInitInt64(var BufA; const SizeA: LongWord; const B: Int64); {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
begin
  if B = 0 then
    IntBufInitZero(BufA, SizeA)
  else
    begin
      S := @BufA;
      if B > 0 then
        begin
          S^ := 1;
          Inc(S);
          PInt64(S)^ := B;
        end
      else
        begin
          S^ := -1;
          Inc(S);
          PInt64(S)^ := -B;
        end;
    end;
end;

procedure IntBufInitInt128(var BufA; const SizeA: LongWord; const B: Int128); {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    T : Integer;
begin
  T := Int128Sign(B);
  if T = 0 then
    IntBufInitZero(BufA, SizeA)
  else
    begin
      S := @BufA;
      if T >= 0 then
        begin      
          S^ := 1;
          Inc(S);
          PInt128(S)^ := B;
        end
      else
        begin
          S^ := -1;
          Inc(S);
          PInt128(S)^ := B;
          Int128Negate(PInt128(S)^);
        end;
    end;
end;

procedure IntBufInitIntBuf(var BufA; const SizeA: LongWord; const BufB; const SizeB: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    L, M, N : Integer;
begin
  Assert(SizeA > 4);
  Assert(SizeB > 4);
  Assert(SizeA mod 4 = 1);
  Assert(SizeB mod 4 = 1);

  if SizeB > SizeA then
    begin
      S := @BufB;
      Inc(S);
      L := SizeA - 1;
      M := SizeB - 1;
      Inc(S, L);
      {$IFOPT R+}
      if not BufIsZero(S^, M - L) then
        RaiseRangeError;
      {$ENDIF}
      N := SizeA;
    end
  else
    N := SizeB;
  Move(BufB, BufA, N);
end;

procedure IntBufInitFloat(var BufA; const SizeA: LongWord; const B: Extended); {$IFDEF UseInline}inline;{$ENDIF}
var
  S, T : PShortInt;
begin
  Assert(SizeA > 4);
  Assert(SizeA mod 4 = 1);

  S := @BufA;
  T := S;
  Inc(T);
  WordBufInitFloat(T^, SizeA - 1, Abs(B));
  if B < 0.0 then
    S^ := -1 else
  if B = 0.0 then
    S^ := 0
  else
    S^ := 1;
end;

function IntBufToWord32(const Buf; const Size: LongWord): LongWord; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    P : PLongWord;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  {$IFOPT Q+}
  if S^ < 0 then
    RaiseOverflowError;
  {$ENDIF}
  Inc(S);
  P := PLongWord(S);
  Result := P^;
  {$IFOPT Q+}
  Inc(P);
  if not BufIsZero(P^, Size - SizeOf(ShortInt) - SizeOf(LongWord)) then
    RaiseOverflowError;
  {$ENDIF}
end;

function IntBufToInt32(const Buf; const Size: LongWord): LongInt; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    T : ShortInt;
    P : PLongInt;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  T := S^;
  Inc(S);
  P := PLongInt(S);
  Result := P^;
  {$IFOPT Q+}
  Inc(P);
  if not BufIsZero(P^, Size - SizeOf(ShortInt) - SizeOf(LongInt)) then
    RaiseOverflowError;
  {$ENDIF}
  if T < 0 then
    Result := -Result;
end;

function IntBufToInt64(const Buf; const Size: LongWord): Int64; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    T : ShortInt;
    P : PInt64;
begin
  Assert(Size > 4);
  Assert(Size mod 4 = 1);

  S := @Buf;
  T := S^;
  Inc(S);
  if Size = SizeOf(ShortInt) + SizeOf(LongWord) then
    begin
      {$IFOPT R+}
      {$ENDIF}
      Result := PLongWord(S)^;
      if T < 0 then
        Result := -Result;
      exit;
    end;
  P := PInt64(S);
  Result := P^;
  {$IFOPT Q+}
  Inc(P);
  if not BufIsZero(P^, Size - SizeOf(ShortInt) - SizeOf(Int64)) then
    RaiseOverflowError;
  {$ENDIF}
  if T < 0 then
    Result := -Result;
end;

function IntBufToFloat(const Buf; const Size: LongWord): Extended; {$IFDEF UseInline}inline;{$ENDIF}
var S, T : PShortInt;
begin
  S := @Buf;
  T := S;
  Inc(T);
  Result := WordBufToFloat(T^, Size - 1);
  if S^ < 0 then
    Result := -Result;
end;

function IntBufEqualsWord32(const BufA; const SizeA: LongWord; const B: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    P : PLongWord;
begin
  Assert(SizeA > 4);
  Assert(SizeA mod 4 = 1);

  S := @BufA;
  if S^ < 0 then
    begin
      Result := False;
      exit;
    end;
  Inc(S);
  P := PLongWord(S);
  Result := P^ = B;
  if not Result then
    exit;
  Inc(P);
  if not BufIsZero(P^, SizeA - SizeOf(ShortInt) - SizeOf(LongWord)) then
    Result := False;
end;

function IntBufEqualsInt32(const BufA; const SizeA: LongWord; const B: LongInt): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    T : ShortInt;
    P : PLongInt;
begin
  Assert(SizeA > 4);
  Assert(SizeA mod 4 = 1);

  S := @BufA;
  T := S^;
  if (T >= 0) and (B < 0) or
     (T < 0) or (B >= 0) then
    begin
      Result := False;
      exit;
    end;
  Inc(S);
  P := PLongInt(S);
  Result := P^ = Abs(B);
  if not Result then
    exit;
  Inc(P);
  if not BufIsZero(P^, SizeA - SizeOf(ShortInt) - SizeOf(LongInt)) then
    Result := False;
end;

function IntBufEqualsIntBuf(const BufA; const SizeA: LongWord; const BufB; const SizeB: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var S, T : PShortInt;
    P, Q : PLongWord;
    L, M, N, I : Integer;
begin
  Assert(SizeA > 4);
  Assert(SizeB > 4);
  Assert(SizeA mod 4 = 1);
  Assert(SizeB mod 4 = 1);

  S := @BufA;
  T := @BufB;
  Result := (S^ = T^);
  if not Result then
    exit;
  Inc(S);
  Inc(T);
  P := PLongWord(S);
  Q := PLongWord(T);
  L := (SizeA - 1) div 4;
  M := (SizeB - 1) div 4;
  if L < M then
    N := L
  else
    N := M;
  for I := 0 to N - 1 do
    if P^ <> Q^ then
      begin
        Result := False;
        exit;
      end
    else
      begin
        Inc(P);
        Inc(Q);
      end;
  if L > M then
    Result := BufIsZero(P^, (L - M) * 4) else
  if M > L then
    Result := BufIsZero(Q^, (M - L) * 4)
  else
    Result := True;
end;

function IntBufCompareWord32(const BufA; const SizeA: LongWord; const B: LongWord): Integer; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    P, Q : PLongWord;
    V : LongWord;
begin
  Assert(SizeA > 4);
  Assert(SizeA mod 4 = 1);

  S := @BufA;
  if S^ < 0 then
    begin
      Result := -1;
      exit;
    end;
  Inc(S);
  P := PLongWord(S);
  Q := P;
  Inc(Q);
  if not BufIsZero(Q, SizeA - SizeOf(ShortInt) - SizeOf(LongWord)) then
    begin
      Result := 1;
      exit;
    end;
  V := P^;
  if V > B then
    Result := 1 else
  if V < B then
    Result := -1
  else
    Result := 0;
end;

function IntBufCompareInt32(const BufA; const SizeA: LongWord; const B: LongInt): Integer; {$IFDEF UseInline}inline;{$ENDIF}
var S : PShortInt;
    T : ShortInt;
    P, Q : PLongInt;
    V : LongInt;
begin
  Assert(SizeA > 4);
  Assert(SizeA mod 4 = 1);

  S := @BufA;
  T := S^;
  if (T < 0) and (B >= 0) then
    begin
      Result := -1;
      exit;
    end else
  if (T >= 0) and (B < 0) then
    begin
      Result := 1;
      exit;
    end;
  Inc(S);
  P := PLongInt(S);
  Q := P;
  Inc(Q);
  if not BufIsZero(Q, SizeA - SizeOf(ShortInt) - SizeOf(LongInt)) then
    begin
      if T < 0 then
        Result := -1
      else
        Result := 1;
      exit;
    end;
  V := P^;
  if V > B then
    Result := 1 else
  if V < B then
    Result := -1
  else
    Result := 0;
end;

function IntBufCompareIntBuf(const BufA; const SizeA: LongWord; const BufB; const SizeB: LongWord): Integer; {$IFDEF UseInline}inline;{$ENDIF}
var S, T : PShortInt;
    U, V : ShortInt;
    C : Integer;
begin
  Assert(SizeA > 4);
  Assert(SizeB > 4);
  Assert(SizeA mod 4 = 1);
  Assert(SizeB mod 4 = 1);

  S := @BufA;
  T := @BufB;
  U := S^;
  V := T^;
  if (U < 0) and (V >= 0) then
    begin
      Result := -1;
      exit;
    end else
  if (U >= 0) and (V < 0) then
    begin
      Result := 1;
      exit;
    end;
  Inc(S);
  Inc(T);
  C := WordBufCompareWordBuf(S^, SizeA - 1, T^, SizeB - 1);
  if C = 0 then
    Result := 0
  else
    if U = V then
      Result := C
    else
      Result := -C;
end;

procedure IntBufMin(const BufA; const SizeA: LongWord; const BufB; const SizeB: LongWord; var BufR); {$IFDEF UseInline}inline;{$ENDIF}
begin
  if IntBufCompareIntBuf(BufA, SizeA, BufB, SizeB) <= 0 then
    Move(BufA, BufR, SizeA)
  else
    Move(BufB, BufR, SizeB);
end;

procedure IntBufMax(const BufA; const SizeA: LongWord; const BufB; const SizeB: LongWord; var BufR); {$IFDEF UseInline}inline;{$ENDIF}
begin
  if IntBufCompareIntBuf(BufA, SizeA, BufB, SizeB) >= 0 then
    Move(BufA, BufR, SizeA)
  else
    Move(BufB, BufR, SizeB);
end;

procedure IntBufAddWord32(var BufA; const SizeA: LongWord; const B: LongWord);
var S, R : PShortInt;
    V : LongWord;
begin
  Assert(SizeA > 4);
  Assert(SizeA mod 4 = 1);

  S := @BufA;
  R := S;
  Inc(R);
  if S^ >= 0 then
    WordBufAddWord32(R^, SizeA - 1, B)
  else
    case WordBufCompareWord32(R^, SizeA - 1, B) of
      0  : IntBufInitZero(BufA, SizeA);
      1  : WordBufSubtractWord32(R^, SizeA - 1, B);
      -1 : begin
             S^ := 1;
             V := B - WordBufToWord32(R^, SizeA - 1);
             PLongWord(R)^ := V;
           end;
    end;
end;


{                                                                              }
{ Int512                                                                       }
{                                                                              }
procedure Int512InitZero(var A: Int512);
begin
  IntBufInitZero(A, Int512Size);
end;

procedure Int512InitOne(var A: Int512);
begin
  IntBufInitOne(A, Int512Size);
end;

procedure Int512InitMinusOne(var A: Int512);
begin
  IntBufInitMinusOne(A, Int512Size);
end;

procedure Int512InitMinimum(var A: Int512);
begin
  IntBufInitMinimum(A, Int512Size);
end;

procedure Int512InitMaximum(var A: Int512);
begin
  IntBufInitMaximum(A, Int512Size);
end;

function Int512IsNegative(const A: Int512): Boolean;
begin
  Result := IntBufIsNegative(A, Int512Size);
end;

function Int512IsZero(const A: Int512): Boolean;
begin
  Result := IntBufIsZero(A, Int512Size);
end;

function Int512IsOne(const A: Int512): Boolean;
begin
  Result := IntBufIsOne(A, Int512Size);
end;

function Int512IsMinusOne(const A: Int512): Boolean;
begin
  Result := IntBufIsMinusOne(A, Int512Size);
end;

function Int512IsMinimum(const A: Int512): Boolean;
begin
  Result := IntBufIsMinimum(A, Int512Size);
end;

function Int512IsMaximum(const A: Int512): Boolean;
begin
  Result := IntBufIsMaximum(A, Int512Size);
end;

function Int512IsOdd(const A: Int512): Boolean;
begin
  Result := IntBufIsOdd(A, Int512Size);
end;

function Int512Sign(const A: Int512): Integer;
begin
  Result := IntBufSign(A, Int512Size);
end;

function Int512IsWord32Range(const A: Int512): Boolean;
begin
  Result := IntBufIsWord32Range(A, Int512Size);
end;

function Int512IsWord64Range(const A: Int512): Boolean;
begin
  Result := IntBufIsWord64Range(A, Int512Size);
end;

function Int512IsWord128Range(const A: Int512): Boolean;
begin
  Result := IntBufIsWord128Range(A, Int512Size);
end;

function Int512IsWord256Range(const A: Int512): Boolean;
begin
  Result := IntBufIsWord256Range(A, Int512Size);
end;

function Int512IsInt32Range(const A: Int512): Boolean;
begin
  Result := IntBufIsInt32Range(A, Int512Size);
end;

function Int512IsInt64Range(const A: Int512): Boolean;
begin
  Result := IntBufIsInt64Range(A, Int512Size);
end;

procedure Int512InitWord32(var A: Int512; const B: LongWord);
begin
  IntBufInitWord32(A, Int512Size, B);
end;

procedure Int512InitWord64(var A: Int512; const B: Word64);
begin
  IntBufInitWord64(A, Int512Size, B);
end;

procedure Int512InitWord128(var A: Int512; const B: Word128);
begin
  IntBufInitWord128(A, Int512Size, B);
end;

procedure Int512InitInt32(var A: Int512; const B: LongInt);
begin
  IntBufInitInt32(A, Int512Size, B);
end;

procedure Int512InitInt64(var A: Int512; const B: Int64);
begin
  IntBufInitInt64(A, Int512Size, B);
end;

procedure Int512InitInt128(var A: Int512; const B: Int128);
begin
  IntBufInitInt128(A, Int512Size, B);
end;

function Int512ToWord32(const A: Int512): LongWord;
begin
  Result := IntBufToWord32(A, Int512Size);
end;

function Int512ToInt32(const A: Int512): LongInt;
begin
  Result := IntBufToInt32(A, Int512Size);
end;

function Int512ToInt64(const A: Int512): Int64;
begin
  Result := IntBufToInt64(A, Int512Size);
end;



{                                                                              }
{ Int1024                                                                       }
{                                                                              }
procedure Int1024InitZero(var A: Int1024);
begin
  IntBufInitZero(A, Int1024Size);
end;

procedure Int1024InitOne(var A: Int1024);
begin
  IntBufInitOne(A, Int1024Size);
end;

procedure Int1024InitMinusOne(var A: Int1024);
begin
  IntBufInitMinusOne(A, Int1024Size);
end;

procedure Int1024InitMinimum(var A: Int1024);
begin
  IntBufInitMinimum(A, Int1024Size);
end;

procedure Int1024InitMaximum(var A: Int1024);
begin
  IntBufInitMaximum(A, Int1024Size);
end;

function Int1024IsNegative(const A: Int1024): Boolean;
begin
  Result := IntBufIsNegative(A, Int1024Size);
end;

function Int1024IsZero(const A: Int1024): Boolean;
begin
  Result := IntBufIsZero(A, Int1024Size);
end;

function Int1024IsOne(const A: Int1024): Boolean;
begin
  Result := IntBufIsOne(A, Int1024Size);
end;

function Int1024IsMinusOne(const A: Int1024): Boolean;
begin
  Result := IntBufIsMinusOne(A, Int1024Size);
end;

function Int1024IsMinimum(const A: Int1024): Boolean;
begin
  Result := IntBufIsMinimum(A, Int1024Size);
end;

function Int1024IsMaximum(const A: Int1024): Boolean;
begin
  Result := IntBufIsMaximum(A, Int1024Size);
end;

function Int1024IsOdd(const A: Int1024): Boolean;
begin
  Result := IntBufIsOdd(A, Int1024Size);
end;

function Int1024Sign(const A: Int1024): Integer;
begin
  Result := IntBufSign(A, Int1024Size);
end;

function Int1024IsWord32Range(const A: Int1024): Boolean;
begin
  Result := IntBufIsWord32Range(A, Int1024Size);
end;

function Int1024IsWord64Range(const A: Int1024): Boolean;
begin
  Result := IntBufIsWord64Range(A, Int1024Size);
end;

function Int1024IsWord128Range(const A: Int1024): Boolean;
begin
  Result := IntBufIsWord128Range(A, Int1024Size);
end;

function Int1024IsWord256Range(const A: Int1024): Boolean;
begin
  Result := IntBufIsWord256Range(A, Int1024Size);
end;

function Int1024IsInt32Range(const A: Int1024): Boolean;
begin
  Result := IntBufIsInt32Range(A, Int1024Size);
end;

function Int1024IsInt64Range(const A: Int1024): Boolean;
begin
  Result := IntBufIsInt64Range(A, Int1024Size);
end;

procedure Int1024InitWord32(var A: Int1024; const B: LongWord);
begin
  IntBufInitWord32(A, Int1024Size, B);
end;

procedure Int1024InitWord64(var A: Int1024; const B: Word64);
begin
  IntBufInitWord64(A, Int1024Size, B);
end;

procedure Int1024InitWord128(var A: Int1024; const B: Word128);
begin
  IntBufInitWord128(A, Int1024Size, B);
end;

procedure Int1024InitInt32(var A: Int1024; const B: LongInt);
begin
  IntBufInitInt32(A, Int1024Size, B);
end;

procedure Int1024InitInt64(var A: Int1024; const B: Int64);
begin
  IntBufInitInt64(A, Int1024Size, B);
end;

procedure Int1024InitInt128(var A: Int1024; const B: Int128);
begin
  IntBufInitInt128(A, Int1024Size, B);
end;

function Int1024ToWord32(const A: Int1024): LongWord;
begin
  Result := IntBufToWord32(A, Int1024Size);
end;

function Int1024ToInt32(const A: Int1024): LongInt;
begin
  Result := IntBufToInt32(A, Int1024Size);
end;

function Int1024ToInt64(const A: Int1024): Int64;
begin
  Result := IntBufToInt64(A, Int1024Size);
end;



{                                                                              }
{ Int2048                                                                       }
{                                                                              }
procedure Int2048InitZero(var A: Int2048);
begin
  IntBufInitZero(A, Int2048Size);
end;

procedure Int2048InitOne(var A: Int2048);
begin
  IntBufInitOne(A, Int2048Size);
end;

procedure Int2048InitMinusOne(var A: Int2048);
begin
  IntBufInitMinusOne(A, Int2048Size);
end;

procedure Int2048InitMinimum(var A: Int2048);
begin
  IntBufInitMinimum(A, Int2048Size);
end;

procedure Int2048InitMaximum(var A: Int2048);
begin
  IntBufInitMaximum(A, Int2048Size);
end;

function Int2048IsNegative(const A: Int2048): Boolean;
begin
  Result := IntBufIsNegative(A, Int2048Size);
end;

function Int2048IsZero(const A: Int2048): Boolean;
begin
  Result := IntBufIsZero(A, Int2048Size);
end;

function Int2048IsOne(const A: Int2048): Boolean;
begin
  Result := IntBufIsOne(A, Int2048Size);
end;

function Int2048IsMinusOne(const A: Int2048): Boolean;
begin
  Result := IntBufIsMinusOne(A, Int2048Size);
end;

function Int2048IsMinimum(const A: Int2048): Boolean;
begin
  Result := IntBufIsMinimum(A, Int2048Size);
end;

function Int2048IsMaximum(const A: Int2048): Boolean;
begin
  Result := IntBufIsMaximum(A, Int2048Size);
end;

function Int2048IsOdd(const A: Int2048): Boolean;
begin
  Result := IntBufIsOdd(A, Int2048Size);
end;

function Int2048Sign(const A: Int2048): Integer;
begin
  Result := IntBufSign(A, Int2048Size);
end;

function Int2048IsWord32Range(const A: Int2048): Boolean;
begin
  Result := IntBufIsWord32Range(A, Int2048Size);
end;

function Int2048IsWord64Range(const A: Int2048): Boolean;
begin
  Result := IntBufIsWord64Range(A, Int2048Size);
end;

function Int2048IsWord128Range(const A: Int2048): Boolean;
begin
  Result := IntBufIsWord128Range(A, Int2048Size);
end;

function Int2048IsWord256Range(const A: Int2048): Boolean;
begin
  Result := IntBufIsWord256Range(A, Int2048Size);
end;

function Int2048IsInt32Range(const A: Int2048): Boolean;
begin
  Result := IntBufIsInt32Range(A, Int2048Size);
end;

function Int2048IsInt64Range(const A: Int2048): Boolean;
begin
  Result := IntBufIsInt64Range(A, Int2048Size);
end;

procedure Int2048InitWord32(var A: Int2048; const B: LongWord);
begin
  IntBufInitWord32(A, Int2048Size, B);
end;

procedure Int2048InitWord64(var A: Int2048; const B: Word64);
begin
  IntBufInitWord64(A, Int2048Size, B);
end;

procedure Int2048InitWord128(var A: Int2048; const B: Word128);
begin
  IntBufInitWord128(A, Int2048Size, B);
end;

procedure Int2048InitInt32(var A: Int2048; const B: LongInt);
begin
  IntBufInitInt32(A, Int2048Size, B);
end;

procedure Int2048InitInt64(var A: Int2048; const B: Int64);
begin
  IntBufInitInt64(A, Int2048Size, B);
end;

procedure Int2048InitInt128(var A: Int2048; const B: Int128);
begin
  IntBufInitInt128(A, Int2048Size, B);
end;

function Int2048ToWord32(const A: Int2048): LongWord;
begin
  Result := IntBufToWord32(A, Int2048Size);
end;

function Int2048ToInt32(const A: Int2048): LongInt;
begin
  Result := IntBufToInt32(A, Int2048Size);
end;

function Int2048ToInt64(const A: Int2048): Int64;
begin
  Result := IntBufToInt64(A, Int2048Size);
end;



{                                                                              }
{ Int4096                                                                       }
{                                                                              }
procedure Int4096InitZero(var A: Int4096);
begin
  IntBufInitZero(A, Int4096Size);
end;

procedure Int4096InitOne(var A: Int4096);
begin
  IntBufInitOne(A, Int4096Size);
end;

procedure Int4096InitMinusOne(var A: Int4096);
begin
  IntBufInitMinusOne(A, Int4096Size);
end;

procedure Int4096InitMinimum(var A: Int4096);
begin
  IntBufInitMinimum(A, Int4096Size);
end;

procedure Int4096InitMaximum(var A: Int4096);
begin
  IntBufInitMaximum(A, Int4096Size);
end;

function Int4096IsNegative(const A: Int4096): Boolean;
begin
  Result := IntBufIsNegative(A, Int4096Size);
end;

function Int4096IsZero(const A: Int4096): Boolean;
begin
  Result := IntBufIsZero(A, Int4096Size);
end;

function Int4096IsOne(const A: Int4096): Boolean;
begin
  Result := IntBufIsOne(A, Int4096Size);
end;

function Int4096IsMinusOne(const A: Int4096): Boolean;
begin
  Result := IntBufIsMinusOne(A, Int4096Size);
end;

function Int4096IsMinimum(const A: Int4096): Boolean;
begin
  Result := IntBufIsMinimum(A, Int4096Size);
end;

function Int4096IsMaximum(const A: Int4096): Boolean;
begin
  Result := IntBufIsMaximum(A, Int4096Size);
end;

function Int4096IsOdd(const A: Int4096): Boolean;
begin
  Result := IntBufIsOdd(A, Int4096Size);
end;

function Int4096Sign(const A: Int4096): Integer;
begin
  Result := IntBufSign(A, Int4096Size);
end;

function Int4096IsWord32Range(const A: Int4096): Boolean;
begin
  Result := IntBufIsWord32Range(A, Int4096Size);
end;

function Int4096IsWord64Range(const A: Int4096): Boolean;
begin
  Result := IntBufIsWord64Range(A, Int4096Size);
end;

function Int4096IsWord128Range(const A: Int4096): Boolean;
begin
  Result := IntBufIsWord128Range(A, Int4096Size);
end;

function Int4096IsWord256Range(const A: Int4096): Boolean;
begin
  Result := IntBufIsWord256Range(A, Int4096Size);
end;

function Int4096IsInt32Range(const A: Int4096): Boolean;
begin
  Result := IntBufIsInt32Range(A, Int4096Size);
end;

function Int4096IsInt64Range(const A: Int4096): Boolean;
begin
  Result := IntBufIsInt64Range(A, Int4096Size);
end;

procedure Int4096InitWord32(var A: Int4096; const B: LongWord);
begin
  IntBufInitWord32(A, Int4096Size, B);
end;

procedure Int4096InitWord64(var A: Int4096; const B: Word64);
begin
  IntBufInitWord64(A, Int4096Size, B);
end;

procedure Int4096InitWord128(var A: Int4096; const B: Word128);
begin
  IntBufInitWord128(A, Int4096Size, B);
end;

procedure Int4096InitInt32(var A: Int4096; const B: LongInt);
begin
  IntBufInitInt32(A, Int4096Size, B);
end;

procedure Int4096InitInt64(var A: Int4096; const B: Int64);
begin
  IntBufInitInt64(A, Int4096Size, B);
end;

procedure Int4096InitInt128(var A: Int4096; const B: Int128);
begin
  IntBufInitInt128(A, Int4096Size, B);
end;

function Int4096ToWord32(const A: Int4096): LongWord;
begin
  Result := IntBufToWord32(A, Int4096Size);
end;

function Int4096ToInt32(const A: Int4096): LongInt;
begin
  Result := IntBufToInt32(A, Int4096Size);
end;

function Int4096ToInt64(const A: Int4096): Int64;
begin
  Result := IntBufToInt64(A, Int4096Size);
end;




{ INTEGER ENCODINGS }

{                                                                              }
{ VarWord32                                                                    }
{                                                                              }
{  7             0                                                             }
{ +-+-+-+-+-+-+-+-+--.                                                         }
{ |F|F|D|D|D|D|D|D|...                                                         }
{ +-+-+-+-+-+-+-+-+--.                                                         }
{                                                                              }
{ FF  DDDDDD                                                                   }
{ 00  AAAAAA   A = 0 - 63                                                      }
{ 01  AAAAAA   A = FFFFFFFF - A (0 - 63)                                       }
{ 10  0000LL   L = 0, 1 or 3; A = 1, 2 or 4 bytes                              }
{ 11  0000AA   A = 7FFFFFFE + A (0 - 3)                                        }
{                                                                              }
procedure VarWord32InitZero(var A: VarWord32);
begin
  A.ControlByte := 0;
end;

procedure VarWord32InitMaximum(var A: VarWord32);
begin
  A.ControlByte := $40;
end;

function VarWord32IsZero(const A: VarWord32): Boolean;
begin
  Result := (A.ControlByte = 0) or
            ((A.ControlByte = $80) and (A.DataWord8 = 0)) or
            ((A.ControlByte = $81) and (A.DataWord16 = 0)) or
            ((A.ControlByte = $83) and (A.DataWord32 = 0));
end;

function VarWord32IsMaximum(const A: VarWord32): Boolean;
begin
  Result := (A.ControlByte = $40) or
            ((A.ControlByte = $83) and (A.DataWord32 = $FFFFFFFF));
end;

procedure VarWord32InitWord8(var A: VarWord32; const B: Byte);
begin
  if B < $40 then
    A.ControlByte := B
  else
    begin
      A.ControlByte := $80;
      A.DataWord8 := B;
    end;
end;

procedure VarWord32InitWord16(var A: VarWord32; const B: Word);
begin
  if B <= $FF then
    VarWord32InitWord8(A, Byte(B))
  else
    begin
      A.ControlByte := $81;
      A.DataWord16 := B;
    end;
end;

procedure VarWord32InitWord32(var A: VarWord32; const B: LongWord);
begin
  if B <= $FFFF then
    VarWord32InitWord16(A, Word(B)) else
  if B <= $FFFFFF then
    begin
      A.ControlByte := $82;
      A.DataWord32 := B;
    end else
  if B >= $FFFFFFC0 then
    A.ControlByte := $40 or Byte($FFFFFFFF - B) else
  if (B >= $7FFFFFFE) and (B <= $80000001) then
    A.ControlByte := $C0 or Byte(B - $7FFFFFFE)
  else
    begin
      A.ControlByte := $83;
      A.DataWord32 := B;
    end;
end;

function VarWord32ToWord8(const A: VarWord32): Byte;
var C : Byte;
begin
  Result := 0;
  C := A.ControlByte;
  case C shr 6 of
    0 : Result := C;
    {$IFOPT R+}
    1 : RaiseRangeError;
    {$ENDIF}
    2 : case C and $03 of
          0 : Result := A.DataWord8;
        else
          {$IFOPT R+}
          RaiseRangeError;
          {$ENDIF}
        end;
    {$IFOPT R+}
    3 : RaiseRangeError;
    {$ENDIF}
  end;
end;

function VarWord32ToWord16(const A: VarWord32): Word;
var C : Byte;
begin
  Result := 0;
  C := A.ControlByte;
  case C shr 6 of
    0 : Result := C;
    {$IFOPT R+}
    1 : RaiseRangeError;
    {$ENDIF}
    2 : case C and $03 of
          0 : Result := A.DataWord8;
          1 : Result := A.DataWord16;
        else
          {$IFOPT R+}
          RaiseRangeError;
          {$ENDIF}
        end;
    {$IFOPT R+}
    3 : RaiseRangeError;
    {$ENDIF}
  end;
end;

function VarWord32ToWord32(const A: VarWord32): LongWord;
var C : Byte;
begin
  Result := 0;
  C := A.ControlByte;
  case C shr 6 of
    0 : Result := C;
    1 : Result := $FFFFFFFF - (C and $3F);
    2 : case C and $03 of
          0 : Result := A.DataWord8;
          1 : Result := A.DataWord16;
          2 : Result := A.DataWord32 and $00FFFFFF;
        else
          Result := A.DataWord32;
        end;
    3 : case C and $3C of
          0 : Result := $7FFFFFFE + (C and $03);
        else
          RaiseInvalidOpError;
        end;
  end;
end;

function VarWord32Size(const A: VarWord32): Integer;
var C : Byte;
begin
  Result := -1;
  C := A.ControlByte;
  case C shr 6 of
    0, 1 : Result := 1;
    2    : Result := 2 + (C and $03);
    3    : case C and $3C of
             0 : Result := 1;
           else
             RaiseInvalidOpError;
           end;
  end;
end;



{                                                                              }
{ VarInt32                                                                     }
{                                                                              }
{  7             0                                                             }
{ +-+-+-+-+-+-+-+-+--.                                                         }
{ |F|F|D|D|D|D|D|D|...                                                         }
{ +-+-+-+-+-+-+-+-+--.                                                         }
{                                                                              }
{ FF  DDDDDD                                                                   }
{ 00  AAAAAA   A = 0 - 63                                                      }
{ 01  AAAAAA   A = -1 - -64                                                    }
{ 10  0000LL   A = 1 - 4 bytes (+)                                             }
{ 11  0000LL   A = 1 - 4 bytes (-)                                             }
{                                                                              }
procedure VarInt32InitZero(var A: VarInt32);
begin
  A.ControlByte := 0;
end;

procedure VarInt32InitWord8(var A: VarInt32; const B: Byte);
begin
  if B < $40 then
    A.ControlByte := B
  else
    begin
      A.ControlByte := $80;
      A.DataWord8 := B;
    end;
end;

procedure VarInt32InitWord16(var A: VarInt32; const B: Word);
begin
  if B <= $FF then
    VarInt32InitWord8(A, Byte(B))
  else
    begin
      A.ControlByte := $81;
      A.DataWord16 := B;
    end;
end;

procedure VarInt32InitWord32(var A: VarInt32; const B: LongWord);
begin
  if B <= $FFFF then
    VarInt32InitWord16(A, Word(B))
  else
    begin
      A.ControlByte := $83;
      A.DataWord32 := B;
    end;
end;

procedure VarInt32InitInt8(var A: VarInt32; const B: ShortInt);
begin
  if B >= 0 then
    VarInt32InitWord8(A, Byte(B))
  else
    if B >= -$40 then
      A.ControlByte := $40 or (B + $40)
    else
      begin
        A.ControlByte := $C0;
        A.DataInt8 := B;
      end;
end;

procedure VarInt32InitInt16(var A: VarInt32; const B: SmallInt);
begin
  if B >= 0 then
    VarInt32InitWord16(A, Word(B))
  else
    if B >= -$7F then
      VarInt32InitInt8(A, ShortInt(B))
    else
      begin
        A.ControlByte := $C1;
        A.DataInt16 := B;
      end;
end;

procedure VarInt32InitInt32(var A: VarInt32; const B: LongInt);
begin
  if B >= 0 then
    VarInt32InitWord32(A, LongWord(B))
  else
    if B >= -$7FFF then
      VarInt32InitInt16(A, SmallInt(B))
    else
      begin
        A.ControlByte := $C3;
        A.DataInt32 := B;
      end;
end;

function VarInt32ToWord8(const A: VarWord32): Byte;
var C : Byte;
begin
  Result := 0;
  C := A.ControlByte;
  case C shr 6 of
    0 : Result := C and $3F;
    {$IFOPT R+}
    1 : RaiseRangeError;
    {$ENDIF}
    2 : case C and $03 of
          0 : Result := A.DataWord8;
        else
          {$IFOPT R+}
          RaiseRangeError;
          {$ENDIF}
        end;
  else
    {$IFOPT R+}
    RaiseRangeError;
    {$ENDIF}
  end;
end;

function VarInt32ToWord16(const A: VarWord32): Word;
var C : Byte;
begin
  Result := 0;
  C := A.ControlByte;
  case C shr 6 of
    0 : Result := C and $3F;
    {$IFOPT R+}
    1 : RaiseRangeError;
    {$ENDIF}
    2 : case C and $03 of
          0 : Result := A.DataWord8;
          1 : Result := A.DataWord16;
        else
          {$IFOPT R+}
          RaiseRangeError;
          {$ENDIF}
        end;
  else
    {$IFOPT R+}
    RaiseRangeError;
    {$ENDIF}
  end;
end;

function VarInt32ToWord32(const A: VarWord32): LongWord;
var C : Byte;
begin
  Result := 0;
  C := A.ControlByte;
  case C shr 6 of
    0 : Result := C and $3F;
    {$IFOPT R+}
    1 : RaiseRangeError;
    {$ENDIF}
    2 : case C and $03 of
          0 : Result := A.DataWord8;
          1 : Result := A.DataWord16;
          2 : RaiseInvalidOpError;
          3 : Result := A.DataWord32;
        end;
  else
    {$IFOPT R+}
    RaiseRangeError;
    {$ENDIF}
  end;
end;

function VarInt32ToInt8(const A: VarInt32): ShortInt;
var C : Byte;
begin
  Result := 0;
  C := A.ControlByte;
  case C shr 6 of
    0 : Result := C and $3F;
    1 : Result := C and $3F - $40;
    2 : case C and $03 of
          0 : Result := A.DataWord8;
        else
          {$IFOPT R+}
          RaiseRangeError;
          {$ENDIF}
        end;
    3 : case C and $03 of
          0 : Result := A.DataInt8;
        else
          {$IFOPT R+}
          RaiseRangeError;
          {$ENDIF}
        end;
  end;
end;

function VarInt32ToInt16(const A: VarInt32): SmallInt;
var C : Byte;
begin
  Result := 0;
  C := A.ControlByte;
  case C shr 6 of
    0 : Result := C and $3F;
    1 : Result := C and $3F - $40;
    2 : case C and $03 of
          0 : Result := A.DataWord8;
          1 : Result := A.DataWord16;
        else
          {$IFOPT R+}
          RaiseRangeError;
          {$ENDIF}
        end;
    3 : case C and $03 of
          0 : Result := A.DataInt8;
          1 : Result := A.DataInt16;
        else
          {$IFOPT R+}
          RaiseRangeError;
          {$ENDIF}
        end;
  end;
end;

function VarInt32ToInt32(const A: VarInt32): LongInt;
var C : Byte;
begin
  Result := 0;
  C := A.ControlByte;
  case C shr 6 of
    0 : Result := C and $3F;
    1 : Result := C and $3F - $40;
    2 : case C and $03 of
          0 : Result := A.DataWord8;
          1 : Result := A.DataWord16;
          2 : RaiseInvalidOpError;
          3 : Result := A.DataWord32;
        end;
    3 : case C and $03 of
          0 : Result := A.DataInt8;
          1 : Result := A.DataInt16;
          2 : RaiseInvalidOpError;
          3 : Result := A.DataInt32;
        end;
  end;
end;

function VarInt32Size(const A: VarInt32): Integer;
var C : Byte;
begin
  C := A.ControlByte;
  case C shr 6 of
    0, 1 : Result := 1;
    2, 3 : Result := C and $03 + 1;
  else
    Result := -1;
  end;
end;




{                                                                              }
{ VarWord32Pair                                                                }
{                                                                              }
{ ControlByte                                                                  }
{  0             7                                                             }
{ +-+-+-+-+-+-+-+-+--.                                                         }
{ |F|F|D|D|D|D|D|D|...                                                         }
{ +-+-+-+-+-+-+-+-+--.                                                         }
{                                                                              }
{ FF  DDDDDD                                                                   }
{ 00  AAABBB   A = 0-7       B = 0-7                                           }
{ 01  AAAAMM   A = 0-15      B = 1-4 bytes                                     }
{ 10  BBBBLL   A = 1-4 bytes B = 0-15                                          }
{ 11  00LLMM   A = 1-4 bytes B = 1-4 bytes                                     }
{                                                                              }
function VarWord32PairWord32Size(const A: LongWord): LongWord; {$IFDEF UseInline}inline;{$ENDIF}
begin
  if A <= $FF then
    Result := 1 else
  if A > $FFFFFF then
    Result := 4 else
  if A <= $FFFF then
    Result := 2
  else
    Result := 3
end;

function VarWord32PairLongWord(const P: Pointer; const Size: Integer): LongWord;
begin
  case Size of
    1 : Result := PByte(P)^;
    2 : Result := PWord(P)^;
    3 : Result := PLongWord(P)^ and $00FFFFFF;
    4 : Result := PLongWord(P)^;
  else
    Result := 0;
  end;
end;

procedure VarWord32PairInitZero(var A: VarWord32Pair);
begin
  A.ControlByte := 0;
end;

procedure VarWord32PairInitWord32(var C: VarWord32Pair; const A, B: LongWord);
var L, M : LongWord;
begin
  if (A <= 7) and (B <= 7) then
    C.ControlByte := (A shl 3) or B else
  if A <= 15 then
    begin
      C.ControlByte := $40 or (A shl 2) or VarWord32PairWord32Size(B);
      C.DataWord32s[0] := B;
    end else
  if B <= 15 then
    begin
      C.ControlByte := $80 or (B shl 2) or VarWord32PairWord32Size(A);
      C.DataWord32s[0] := A;
    end
  else
    begin
      L := VarWord32PairWord32Size(A);
      M := VarWord32PairWord32Size(B);
      C.ControlByte := $C0 or ((L - 1) shl 2) or (M - 1);
      C.DataWord32s[0] := A;
      PLongWord(@C.DataWord8s[L])^ := B;
    end;
end;

function VarWord32PairToWord32Pair(const A: VarWord32Pair): Word32Pair;
var C, L : Byte;
begin
  C := A.ControlByte;
  case C shr 6 of
    $00 :
      begin
        Result.Word32s[0] := (C shr 3) and $07;
        Result.Word32s[1] := (C and $07);
      end;
    $01 :
      begin
        Result.Word32s[0] := (C shr 2) and $0F;
        Result.Word32s[1] := VarWord32PairLongWord(@A.DataWord8s[0], (C and $03) + 1);
      end;
    $02 :
      begin
        Result.Word32s[0] := VarWord32PairLongWord(@A.DataWord8s[0], (C and $03) + 1);
        Result.Word32s[1] := (C shr 2) and $0F;
      end;
    $03 :
      begin
        L := ((C shr 2) and $03) + 1;
        Result.Word32s[0] := VarWord32PairLongWord(@A.DataWord8s[0], L);
        Result.Word32s[1] := VarWord32PairLongWord(@A.DataWord8s[L], (C and $03) + 1);
      end;
  end;
end;

function VarWord32PairSize(const A: VarWord32Pair): Integer;
var C : Byte;
begin
  C := A.ControlByte;
  case C shr 6 of
    $00      : Result := 1;
    $01, $02 : Result := 2 + (C and $03);
    $03      : Result := 3 + (C and $03) + ((C shr 2) and $03);
  else
    Result := -1;
  end;
end;



{                                                                              }
{ VarInt32Pair                                                                 }
{                                                                              }
{ ControlByte                                                                  }
{  0             7                                                             }
{ +-+-+-+-+-+-+-+-+--.                                                         }
{ |F|F|D|D|D|D|D|D|...                                                         }
{ +-+-+-+-+-+-+-+-+--.                                                         }
{                                                                              }
{ FF  DDDDDD                                                                   }
{ 00  AAABBB   A = -3 to +4  B = -3 to +4                                      }
{ 01  AAAAMM   A = -7 to +8  B = 1-4 bytes                                     }
{ 10  BBBBLL   A = 1-4 bytes B = -7 to +8                                      }
{ 11  00LLMM   A = 1-4 bytes B = 1-4 bytes                                     }
{                                                                              }
function VarInt32PairInt32Size(const A: LongInt): LongWord;
begin
  Result := VarWord32PairWord32Size(LongWord(A));
end;

procedure VarInt32PairInitZero(var A: VarInt32Pair);
begin
  A.ControlByte := 0;
end;

procedure VarInt32PairInitInt32(var C: VarInt32Pair; const A, B: LongInt);
var L, M : LongWord;
begin
  if (A <= 4) and (A >= -3) and
     (B <= 4) and (B >= -3) then
    C.ControlByte := ((A + 3) shl 3) or (B + 3) else
  if (A <= 8) and (A >= -7) then
    begin
      L := VarInt32PairInt32Size(B);
      C.ControlByte := $40 or (Byte(A + 7) shl 2) or (L - 1);
      C.DataInt32s[0] := B;
    end else
  if (B <= 8) and (B >= -7) then
    begin
      L := VarInt32PairInt32Size(A);
      C.ControlByte := $80 or (Byte(B + 7) shl 2) or (L - 1);
      C.DataInt32s[0] := A;
    end
  else
    begin
      L := VarInt32PairInt32Size(A);
      M := VarInt32PairInt32Size(B);
      C.ControlByte := $80 or ((L - 1) shl 2) or (M - 1);
      C.DataInt32s[0] := A;
      PLongInt(@C.DataInt8s[L])^ := B;
    end;
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF INTEGER_TEST}
{$ASSERTIONS ON}
procedure Test_Word8;
var X : array[Byte] of Byte;
    A : Integer;
    F, G : SmallInt;
begin
  // Word8HashTable
  for A := 0 to 255 do
    X[A] := 0;
  for A := 0 to 255 do
    Inc(X[Word8HashTable[A]]);
  for A := 0 to 255 do
    Assert(X[A] = 1);
  for A := 0 to 255 do
    Assert(Word8HashTable[A] <> A);

  // Word8Hash
  for A := 0 to 255 do
    X[A] := 0;
  for A := 0 to 255 do
    Inc(X[Word8Hash(A)]);
  for A := 0 to 255 do
    Assert(X[A] = 1);

  // Hash
  Assert(Word8Hash(0) = Int8Hash(0));
  Assert(Word16Hash(0) = Int16Hash(0));
  Assert(Word32Hash(0) = Int32Hash(0));

  // GCD
  Assert(Word8GCD(1, 1) = 1);
  Assert(Word8GCD(2, 2) = 2);
  Assert(Word8GCD(2, 1) = 1);
  Assert(Word8GCD(1, 2) = 1);
  Assert(Word8GCD(0, 1) = 1);
  Assert(Word8GCD(1, 0) = 1);
  Assert(Word8GCD(1, 0) = 1);
  Assert(Word8GCD(182, 52) = 26);
  Assert(Word8GCD(91, 39) = 13);
  Assert(Word8ExtendedEuclid(182, 52, F, G) = 26);
  Assert((F = 1) and (G = -3));
end;

procedure Test_Word16;
var X : array[Word] of Word;
    A : Integer;
begin
  // Word16Hash
  for A := 0 to 65535 do
    X[A] := 0;
  for A := 0 to 65535 do
    Inc(X[Word16Hash(A)]);
  for A := 0 to 65535 do
    Assert(X[A] = 1);
  for A := 0 to 65535 do
    Assert(Word16Hash(A) <> A);
end;

procedure Test_Word32;
var C, D : Word64;
    A    : LongWord;
begin
  // Compare
  Assert(Word32Compare(1, 2) = -1);
  Assert(Word32Compare(1, 1) = 0);
  Assert(Word32Compare(2, 1) = 1);

  Word32MultiplyWord32($FFFFFFFF, $FFFFFFFE, C);
  Assert(C.LongWords[0] = $00000002);
  Assert(C.LongWords[1] = $FFFFFFFD);
  Word64DivideWord32(C, $FFFFFFFD, D, A);
  Assert(A = 2);
  Assert(Word32PowerAndMod(2, 10, 21) = 16);
  Assert(Word32PowerAndMod(2, 65, 65) = 32);
  Assert(not Word32IsPrime(0));
  Assert(not Word32IsPrime(1));
  Assert(Word32IsPrime(2));
  Assert(Word32IsPrime(3));
  Assert(not Word32IsPrime(4));
  Assert(Word32IsPrime(5));
  Assert(Word32IsPrime(100711433));
  Assert(Word32IsPrimeMR(100711433));
  Assert(Word32IsPrime($FFFFFF2F));
  Assert(Word32IsPrimeMR($FFFFFF2F));
  Assert(not Word32IsPrime($FFFFFF33));
  Assert(not Word32IsPrimeMR($FFFFFF33));
  for A := $FFFFFA00 to $FFFFFFFF do
    Assert(Word32IsPrime(A) = Word32IsPrimeMR(A));
end;

procedure Test_Word64;
const C1 = 4294967296.0 * 5.0 + 4294967295.0;
var A, B : Word64;
    C, D : Word64;
    F    : Extended;
    I    : Integer;
    X    : Byte;
    Y    : Word;
    Z    : LongWord;
begin
  // Size
  Assert(SizeOf(Word64) = 8);

  // 0
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{0}');
  {$ENDIF}
  Word64InitZero(A);
  Assert(Word64ToWord32(A) = 0);
  Assert(Word64ToInt32(A) = 0);
  Assert(Word64IsZero(A));
  Assert(Word64IsMinimum(A));
  Assert(not Word64IsMaximum(A));
  Assert(Word64IsInt32Range(A));
  Assert(Word64EqualsWord32(A, 0));
  Assert(Word64EqualsInt32(A, 0));
  Word64InitMinimum(B);
  Assert(Word64EqualsWord64(A, B));
  Assert(Word64CompareWord64(A, B) = 0);

  // 7FFFFFFF
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{7FFFFFFF}');
  {$ENDIF}
  Word64InitInt32(A, $7FFFFFFF);
  Assert(Word64IsInt32Range(A));
  Assert(Word64ToInt32(A) = $7FFFFFFF);

  // FFFFFFFF
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{FFFFFFFF}');
  {$ENDIF}
  Word64InitWord32(A, $FFFFFFFF);
  Assert(Word64EqualsWord32(A, $FFFFFFFF));
  Assert(not Word64IsInt32Range(A));
  Assert(Word64IsWord32Range(A));
  Assert(Word64EqualsInt64(A, $FFFFFFFF));

  // Maximum
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Max}');
  {$ENDIF}
  Word64InitMaximum(A);
  Assert(not Word64IsMinimum(A));
  Assert(Word64IsMaximum(A));
  Assert(not Word64IsInt64Range(A));

  // Logical operators
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Log}');
  {$ENDIF}
  Word64InitZero(A);
  Word64Not(A);
  Assert(Word64IsMaximum(A));
  Word64Not(A);
  Assert(Word64IsZero(A));
  Word64InitInt64(A, $2100000000120000);
  Word64InitInt64(B, $1234567890ABCDEF);
  Word64OrWord64(A, B);
  Assert(Word64ToInt64(A) = $3334567890BBCDEF);
  Word64AndWord64(A, B);
  Assert(Word64ToInt64(A) = $1234567890ABCDEF);
  Word64XorWord64(A, B);
  Assert(Word64IsZero(A));
  Word64XorWord64(A, B);
  Assert(Word64ToInt64(A) = $1234567890ABCDEF);

  // Bit shift
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{BitShift}');
  {$ENDIF}
  Word64InitInt64(A, $80000003);
  Word64Shl(A, 1);
  Assert(Word64ToInt64(A) = $100000006);
  Word64Shr(A, 1);
  Assert(Word64ToInt64(A) = $80000003);
  Word64Shl1(A);
  Assert(Word64ToInt64(A) = $100000006);
  Word64Shr1(A);
  Assert(Word64ToInt64(A) = $80000003);
  Word64Rol(A, 1);
  Assert(Word64ToInt64(A) = $100000006);
  Word64Ror(A, 1);
  Assert(Word64ToInt64(A) = $80000003);
  Word64Rol1(A);
  Assert(Word64ToInt64(A) = $100000006);
  Word64Ror1(A);
  Assert(Word64ToInt64(A) = $80000003);
  Word64Rol(A, 33);
  Assert(Word64ToInt64(A) = $600000001);
  Word64Ror(A, 33);
  Assert(Word64ToInt64(A) = $80000003);

  // Bit
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Bit}');
  {$ENDIF}
  Word64InitZero(A);
  Word64SetBit(A, 63);
  Assert(Word64IsBitSet(A, 63));
  Word64ClearBit(A, 63);
  Assert(not Word64IsBitSet(A, 63));
  Word64SetBit(A, 62);
  Assert(Word64ToInt64(A) = Int64($4000000000000000));
  Word64ToggleBit(A, 62);
  Assert(Word64IsZero(A));
  Assert(not Word64IsBitSet(A, 63));

  // Compare
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Compare}');
  {$ENDIF}
  Word64InitInt64(A, $123456789);
  Assert(Word64CompareWord32(A, $FFFFFFFF) = 1);
  Assert(Word64CompareInt32(A, $23456789) = 1);
  Assert(Word64CompareInt64(A, $200000000) = -1);
  Assert(Word64CompareInt64(A, $123456789) = 0);
  Assert(Word64CompareWord64(A, A) = 0);
  Word64InitInt64(B, $12345678A);
  Assert(Word64CompareWord64(A, B) = -1);

  // Add
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Add}');
  {$ENDIF}
  Word64InitWord32(A, $FFFFFFFF);
  Word64AddWord32(A, 1);
  Assert(Word64ToInt64(A) = $100000000);
  Word64InitWord32(A, $FFFFFFFF);
  Word64InitWord32(B, 1);
  Word64AddWord64(A, B);
  Assert(Word64ToInt64(A) = $100000000);
  Word64InitInt64(A, $123456789);
  B := A;
  Word64AddWord64(A, B);
  Assert(Word64ToInt64(A) = $2468ACF12);

  // Subtract
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Subtract}');
  {$ENDIF}
  Word64InitInt64(A, $100000000);
  Word64SubtractWord32(A, 1);
  Assert(Word64ToInt64(A) = $FFFFFFFF);
  Word64InitInt64(A, $100000000);
  Word64InitInt64(B, 1);
  Word64SubtractWord64(A, B);
  Assert(Word64ToInt64(A) = $FFFFFFFF);

  // Multiply
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Multiply}');
  {$ENDIF}
  Word64InitInt64(A, $1A1A1AAAAAAAAAAA);
  Word64MultiplyWord8(A, 2);
  Assert(Word64ToInt64(A) = $3434355555555554);
  Word64InitInt64(A, $1A1A1AAAAAAAAAAA);
  Word64MultiplyWord16(A, 2);
  Assert(Word64ToInt64(A) = $3434355555555554);
  Word64InitInt64(A, $1A1A1AAAAAAAAAAA);
  Word64MultiplyWord32(A, 2);
  Assert(Word64ToInt64(A) = $3434355555555554);
  Word64InitInt64(A, $1A1A1AAAAAAAAAAA);
  Word64InitInt64(B, 2);
  Word64MultiplyWord64InPlace(A, B);
  Assert(Word64ToInt64(A) = $3434355555555554);

  // Divide
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Divide}');
  {$ENDIF}
  Word64InitInt64(A, $3434355555555555);
  Word64DivideWord8(A, 2, B, X);
  Assert(Word64ToInt64(B) = $1A1A1AAAAAAAAAAA);
  Assert(X = 1);
  Word64DivideWord8(A, $FF, B, X);
  Assert(Word64ToInt64(B) = $34689DF3489DF3);
  Assert(X = $48);
  Word64DivideWord16(A, 2, B, Y);
  Assert(Word64ToInt64(B) = $1A1A1AAAAAAAAAAA);
  Assert(Y = 1);
  Word64DivideWord16(A, $FFFF, B, Y);
  Assert(Word64ToInt64(B) = $34346989BEDF);
  Assert(Y = $1434);
  Word64DivideWord32(A, 2, B, Z);
  Assert(Word64ToInt64(B) = $1A1A1AAAAAAAAAAA);
  Assert(Z = 1);
  Word64DivideWord32(A, $FFFFFFFF, B, Z);
  Assert(Word64ToInt64(B) = $34343555);
  Assert(Z = $89898AAA);
  Word64InitInt64(C, 2);
  Word64DivideWord64(A, C, B, D);
  Assert(Word64ToInt64(B) = $1A1A1AAAAAAAAAAA);
  Assert(Word64ToInt64(D) = 1);
  Word64InitMaximum(C);
  Word64DivideWord64(A, C, B, D);
  Assert(Word64ToInt64(B) = 0);
  Assert(Word64ToInt64(D) = $3434355555555555);

  // String
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{String}');
  {$ENDIF}
  Word64InitZero(A);
  Assert(Word64ToStr(A) = '0');
  Word64InitInt64(A, 1234);
  Assert(Word64ToStr(A) = '1234');
  Word64InitMaximum(A);
  Assert(Word64ToStr(A) = '18446744073709551615');
  A := StrToWord64('1234');
  Assert(Word64ToInt64(A) = 1234);
  A := StrToWord64('18446744073709551615');
  Assert(Word64IsMaximum(A));

  // Float
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Float}');
  {$ENDIF}
  Word64InitFloat(A, 1.0);
  Assert(Word64ToWord32(A) = 1);
  Assert(Word64ToFloat(A) = 1.0);
  Word64InitFloat(A, C1);
  Assert(Word64ToInt64(A) = $5FFFFFFFF);
  Assert(Abs(Word64ToFloat(A) - C1) < 1.0e-6);
  Word64InitInt64(A, $76543210FFFFFFFF);
  Assert(Word64ToInt64(A) = $76543210FFFFFFFF);
  Assert(Abs(Word64ToFloat(A) - $76543210FFFFFFFF) < 1.0e-6);

  // Sqrt
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Sqrt}');
  {$ENDIF}
  Word64InitInt64(A, 12345 * 12345 + 12344);
  Word64ISqrt(A);
  Assert(Word64ToInt64(A) = 12345);
  Word64InitInt64(A, 16);
  Word64ISqrt(A);
  Assert(Word64ToInt64(A) = 4);
  Word64InitInt64(A, $76543210FFFFFFFF);
  Word64ISqrt(A);
  Assert(Word64ToInt64(A) = $AE0BE992);
  Word64InitInt64(A, 12345 * 12345 + 12344);
  F := Word64Sqrt(A);
  Assert(Abs(F - 12345.49994937427) < 1e-11);
  Word64InitInt64(A, $76543210FFFFFFFF);
  F := Word64Sqrt(A);
  {$IFDEF FREEPASCAL}
  Assert(Abs(F - 2920016274.460644916) < 20); // ExtendedIsDouble?
  {$ELSE}
  Assert(Abs(F - 2920016274.460644916) < 1e-9);
  {$ENDIF}
  for I := 0 to 100 do
    begin
      Word64InitInt32(A, I);
      F := Word64Sqrt(A);
      Assert(Abs(F - Sqrt(I)) < 1e-11);
    end;

  // Power
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Power}');
  {$ENDIF}
  Word64InitInt64(A, 2);
  Word64Power(A, 15);
  Assert(Word64ToInt64(A) = 32768);

  // Prime
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Prime}');
  {$ENDIF}
  Word64InitInt64(A, $FFFFFF2F);
  Assert(Word64IsPrime(A));
  Word64InitInt64(A, $12345678FFFFFF2F);
  while not Word64IsPrime(A) do
    Word64AddWord32(A, 2);
  Assert(Word64ToInt64(A) = $12345678FFFFFF35);
end;

procedure Test_Word128;
var A, B : Word128;
    C, D : Word128;
    F    : Extended;
    I    : Integer;
    X    : Byte;
    Y    : Word;
    Z    : LongWord;
begin
  // Size
  Assert(SizeOf(Word128) = 16);

  // 0
  Word128InitZero(A);
  Assert(Word128ToWord32(A) = 0);
  Assert(Word128ToInt32(A) = 0);
  Assert(Word128IsZero(A));
  Assert(Word128IsMinimum(A));
  Assert(not Word128IsMaximum(A));
  Assert(Word128IsInt32Range(A));
  Assert(Word128EqualsWord32(A, 0));
  Assert(Word128EqualsInt32(A, 0));
  Word128InitMinimum(B);
  Assert(Word128EqualsWord128(A, B));
  Assert(Word128CompareWord128(A, B) = 0);

  // Maximum
  Word128InitMaximum(A);
  Assert(not Word128IsMinimum(A));
  Assert(Word128IsMaximum(A));
  Assert(not Word128IsInt64Range(A));
  Assert(not Word128IsInt128Range(A));

  // Bit shift
  Word128InitMaximum(A);
  Word128Rol1(A);
  Assert(Word128IsMaximum(A));
  Word128Ror1(A);
  Assert(Word128IsMaximum(A));
  Word128Rol(A, 33);
  Assert(Word128IsMaximum(A));
  Word128Ror(A, 33);
  Assert(Word128IsMaximum(A));
  Word128InitInt64(A, $80000003);
  Word128Shl1(A);
  Assert(Word128ToInt64(A) = $100000006);
  Word128Shr1(A);
  Assert(Word128ToInt64(A) = $80000003);
  Word128Shl(A, 1);
  Assert(Word128ToInt64(A) = $100000006);
  Word128Shr(A, 1);
  Assert(Word128ToInt64(A) = $80000003);

  // Multiply
  Word128InitInt64(A, $1A1A1AAAAAAAAAAA);
  Word128MultiplyWord8(A, 2);
  Assert(Word128ToInt64(A) = $3434355555555554);
  Word128InitInt64(A, $1A1A1AAAAAAAAAAA);
  Word128MultiplyWord16(A, 2);
  Assert(Word128ToInt64(A) = $3434355555555554);
  Word128InitInt64(A, $1A1A1AAAAAAAAAAA);
  Word128MultiplyWord32(A, 2);
  Assert(Word128ToInt64(A) = $3434355555555554);
  Word128InitInt64(A, $1A1A1AAAAAAAAAAA);
  Word128InitInt64(B, 2);
  Word128MultiplyWord128Low(A, B);
  Assert(Word128ToInt64(A) = $3434355555555554);

  // Divide
  Word128InitInt64(A, $3434355555555555);
  Word128DivideWord8(A, 2, B, X);
  Assert(Word128ToInt64(B) = $1A1A1AAAAAAAAAAA);
  Assert(X = 1);
  Word128DivideWord8(A, $FF, B, X);
  Assert(Word128ToInt64(B) = $34689DF3489DF3);
  Assert(X = $48);
  Word128DivideWord16(A, 2, B, Y);
  Assert(Word128ToInt64(B) = $1A1A1AAAAAAAAAAA);
  Assert(Y = 1);
  Word128DivideWord16(A, $FFFF, B, Y);
  Assert(Word128ToInt64(B) = $34346989BEDF);
  Assert(Y = $1434);
  Word128DivideWord32(A, 2, B, Z);
  Assert(Word128ToInt64(B) = $1A1A1AAAAAAAAAAA);
  Assert(Z = 1);
  Word128DivideWord32(A, $FFFFFFFF, B, Z);
  Assert(Word128ToInt64(B) = $34343555);
  Assert(Z = $89898AAA);
  Word128InitInt64(C, 2);
  Word128DivideWord128(A, C, B, D);
  Assert(Word128ToInt64(B) = $1A1A1AAAAAAAAAAA);
  Assert(Word128ToInt64(D) = 1);

  // String
  Word128InitZero(A);
  Assert(Word128ToStr(A) = '0');
  Word128InitInt64(A, 1234);
  Assert(Word128ToStr(A) = '1234');
  Assert(Word128ToStrA(A) = '1234');
  Assert(Word128ToStrU(A) = '1234');
  Word128InitMaximum(A);
  Assert(Word128ToStr(A) = '340282366920938463463374607431768211455');
  A := StrToWord128('1234');
  Assert(Word128ToInt64(A) = 1234);
  A := StrToWord128('340282366920938463463374607431768211455');
  Assert(Word128IsMaximum(A));

  // Sqrt
  Word128InitInt64(A, 12345 * 12345 + 12344);
  Word128ISqrt(A);
  Assert(Word128ToInt64(A) = 12345);
  Word128InitInt64(A, 16);
  Word128ISqrt(A);
  Assert(Word128ToInt64(A) = 4);
  Word128InitInt64(A, $76543210FFFFFFFF);
  Word128ISqrt(A);
  Assert(Word128ToInt64(A) = $AE0BE992);
  Word128InitInt64(A, 12345 * 12345 + 12344);
  F := Word128Sqrt(A);
  Assert(Abs(F - 12345.49994937427) < 1e-11);
  Word128InitInt64(A, $76543210FFFFFFFF);
  F := Word128Sqrt(A);
  Assert(Abs(F - 2920016274.460644916) < 1e-9);
  for I := 0 to 100 do
    begin
      Word128InitInt32(A, I);
      F := Word128Sqrt(A);
      Assert(Abs(F - Sqrt(I)) < 1e-11);
    end;

  // Power
  Word128InitInt64(A, 2);
  Word128Power(A, 15);
  Assert(Word128ToInt64(A) = 32768);

  // Prime
  Word128InitInt64(A, $12345678FFFFFF33);
  Word128MultiplyWord8(A, $10);
  Word128AddWord32(A, 1);
  while not Word128IsPrime(A) do
    Word128AddWord32(A, 2);
  Assert((A.LongWords[3] = 0) and (A.LongWords[2] = 1) and
         (A.LongWords[1] = $2345678F) and (A.LongWords[0] = $FFFFF337));
  Word128NextPrime(A);
  Assert((A.LongWords[3] = 0) and (A.LongWords[2] = 1) and
         (A.LongWords[1] = $2345678F) and (A.LongWords[0] = $FFFFF371));

  // Errors
  {$IFDEF TEST_ERRORS}
  {$IFOPT Q+}
  X := 0;
  Word128InitWord32(A, $FFFFFFFF);
  Word128Shl(A, 96);
  try
    Word128MultiplyWord32(A, $FFFFFFFF);
  except
    X := 1;
  end;
  Assert(X = 1);
  {$ENDIF}
  {$ENDIF}
end;

procedure Test_Word256;
var A, B : Word256;
    P    : Word64;
    Q    : Word128;
    R    : Int128;
begin
  // Size
  Assert(SizeOf(Word256) = 32);

  // 0
  Word256InitZero(A);
  Assert(Word256ToWord32(A) = 0);
  Assert(Word256ToInt32(A) = 0);
  Assert(Word256IsZero(A));
  Assert(Word256IsMinimum(A));
  Assert(not Word256IsMaximum(A));
  Assert(Word256IsInt32Range(A));
  Assert(Word256EqualsWord32(A, 0));
  Assert(Word256EqualsInt32(A, 0));
  Word256InitMinimum(B);
  Assert(Word256EqualsWord256(A, B));
  Assert(Word256CompareWord256(A, B) = 0);

  // Maximum
  Word256InitMaximum(A);
  Assert(not Word256IsMinimum(A));
  Assert(Word256IsMaximum(A));
  Assert(not Word256IsInt64Range(A));
  Assert(not Word256IsInt128Range(A));

  // Init
  Word64InitOne(P);
  Word256InitWord64(A, P);
  Assert(Word256IsOne(A));
  Word128InitOne(Q);
  Word256InitWord128(A, Q);
  Assert(Word256IsOne(A));
  Word256InitInt64(A, 1);
  Assert(Word256IsOne(A));
  Int128InitOne(R);
  Word256InitInt128(A, R);
  Assert(Word256IsOne(A));
end;

procedure Test_Word512;
var A, B : Word512;
    P    : Word64;
    Q    : Word128;
    R    : Int128;
begin
  // Size
  Assert(SizeOf(Word512) = 512 div 8);

  // 0
  Word512InitZero(A);
  Assert(Word512ToWord32(A) = 0);
  Assert(Word512ToInt32(A) = 0);
  Assert(Word512IsZero(A));
  Assert(Word512IsMinimum(A));
  Assert(not Word512IsMaximum(A));
  Assert(Word512IsInt32Range(A));
  Assert(Word512EqualsWord32(A, 0));
  Assert(Word512EqualsInt32(A, 0));
  Word512InitMinimum(B);
  Assert(Word512EqualsWord512(A, B));
  Assert(Word512CompareWord512(A, B) = 0);

  // Maximum
  Word512InitMaximum(A);
  Assert(not Word512IsMinimum(A));
  Assert(Word512IsMaximum(A));
  Assert(not Word512IsInt64Range(A));
  Assert(not Word512IsInt128Range(A));

  // Init
  Word64InitOne(P);
  Word512InitWord64(A, P);
  Assert(Word512IsOne(A));
  Word128InitOne(Q);
  Word512InitWord128(A, Q);
  Assert(Word512IsOne(A));
  Word512InitInt64(A, 1);
  Assert(Word512IsOne(A));
  Int128InitOne(R);
  Word512InitInt128(A, R);
  Assert(Word512IsOne(A));
  Assert(Word512EqualsWord32(A, 1));
  Assert(Word512EqualsInt32(A, 1));
  Assert(Word512ToWord32(A) = 1);
  Assert(Word512ToInt32(A) = 1);
end;

procedure Test_Word1024;
var A, B : Word1024;
    P    : Word64;
    Q    : Word128;
    R    : Int128;
begin
  // Size
  Assert(SizeOf(Word1024) = 1024 div 8);

  // 0
  Word1024InitZero(A);
  Assert(Word1024ToWord32(A) = 0);
  Assert(Word1024ToInt32(A) = 0);
  Assert(Word1024IsZero(A));
  Assert(Word1024IsMinimum(A));
  Assert(not Word1024IsMaximum(A));
  Assert(Word1024IsInt32Range(A));
  Assert(Word1024EqualsWord32(A, 0));
  Assert(Word1024EqualsInt32(A, 0));
  Word1024InitMinimum(B);
  Assert(Word1024EqualsWord1024(A, B));
  Assert(Word1024CompareWord1024(A, B) = 0);

  // Maximum
  Word1024InitMaximum(A);
  Assert(not Word1024IsMinimum(A));
  Assert(Word1024IsMaximum(A));
  Assert(not Word1024IsInt64Range(A));
  Assert(not Word1024IsInt128Range(A));

  // Init
  Word64InitOne(P);
  Word1024InitWord64(A, P);
  Assert(Word1024IsOne(A));
  Word128InitOne(Q);
  Word1024InitWord128(A, Q);
  Assert(Word1024IsOne(A));
  Word1024InitInt64(A, 1);
  Assert(Word1024IsOne(A));
  Int128InitOne(R);
  Word1024InitInt128(A, R);
  Assert(Word1024IsOne(A));
  Assert(Word1024EqualsWord32(A, 1));
  Assert(Word1024EqualsInt32(A, 1));
  Assert(Word1024ToWord32(A) = 1);
  Assert(Word1024ToInt32(A) = 1);
end;

procedure Test_Word2048;
var A, B : Word2048;
    P    : Word64;
    Q    : Word128;
    R    : Int128;
begin
  // Size
  Assert(SizeOf(Word2048) = 2048 div 8);

  // 0
  Word2048InitZero(A);
  Assert(Word2048ToWord32(A) = 0);
  Assert(Word2048ToInt32(A) = 0);
  Assert(Word2048IsZero(A));
  Assert(Word2048IsMinimum(A));
  Assert(not Word2048IsMaximum(A));
  Assert(Word2048IsInt32Range(A));
  Assert(Word2048EqualsWord32(A, 0));
  Assert(Word2048EqualsInt32(A, 0));
  Word2048InitMinimum(B);
  Assert(Word2048EqualsWord2048(A, B));
  Assert(Word2048CompareWord2048(A, B) = 0);

  // Maximum
  Word2048InitMaximum(A);
  Assert(not Word2048IsMinimum(A));
  Assert(Word2048IsMaximum(A));
  Assert(not Word2048IsInt64Range(A));
  Assert(not Word2048IsInt128Range(A));

  // Init
  Word64InitOne(P);
  Word2048InitWord64(A, P);
  Assert(Word2048IsOne(A));
  Word128InitOne(Q);
  Word2048InitWord128(A, Q);
  Assert(Word2048IsOne(A));
  Word2048InitInt64(A, 1);
  Assert(Word2048IsOne(A));
  Int128InitOne(R);
  Word2048InitInt128(A, R);
  Assert(Word2048IsOne(A));
  Assert(Word2048EqualsWord32(A, 1));
  Assert(Word2048EqualsInt32(A, 1));
  Assert(Word2048ToWord32(A) = 1);
  Assert(Word2048ToInt32(A) = 1);
end;

procedure Test_Word4096;
var A, B : Word4096;
    P    : Word64;
    Q    : Word128;
    R    : Int128;
begin
  // Size
  Assert(SizeOf(Word4096) = 4096 div 8);

  // 0
  Word4096InitZero(A);
  Assert(Word4096ToWord32(A) = 0);
  Assert(Word4096ToInt32(A) = 0);
  Assert(Word4096IsZero(A));
  Assert(Word4096IsMinimum(A));
  Assert(not Word4096IsMaximum(A));
  Assert(Word4096IsInt32Range(A));
  Assert(Word4096EqualsWord32(A, 0));
  Assert(Word4096EqualsInt32(A, 0));
  Word4096InitMinimum(B);
  Assert(Word4096EqualsWord4096(A, B));
  Assert(Word4096CompareWord4096(A, B) = 0);

  // Maximum
  Word4096InitMaximum(A);
  Assert(not Word4096IsMinimum(A));
  Assert(Word4096IsMaximum(A));
  Assert(not Word4096IsInt64Range(A));
  Assert(not Word4096IsInt128Range(A));

  // Init
  Word64InitOne(P);
  Word4096InitWord64(A, P);
  Assert(Word4096IsOne(A));
  Word128InitOne(Q);
  Word4096InitWord128(A, Q);
  Assert(Word4096IsOne(A));
  Word4096InitInt64(A, 1);
  Assert(Word4096IsOne(A));
  Int128InitOne(R);
  Word4096InitInt128(A, R);
  Assert(Word4096IsOne(A));
  Assert(Word4096EqualsWord32(A, 1));
  Assert(Word4096EqualsInt32(A, 1));
  Assert(Word4096ToWord32(A) = 1);
  Assert(Word4096ToInt32(A) = 1);
end;

procedure Test_Int8;
begin
  Assert(Int8Sign(0) = 0);
  Assert(Int8Sign(123) = 1);
  Assert(Int8Sign(-123) = -1);
end;

procedure Test_Int16;
begin
  Assert(Int16Sign(0) = 0);
  Assert(Int16Sign(12345) = 1);
  Assert(Int16Sign(-12345) = -1);
end;

procedure Test_Int32;
begin
  // Sign
  Assert(Int32Sign(0) = 0);
  Assert(Int32Sign(1234567890) = 1);
  Assert(Int32Sign(-1234567890) = -1);

  // Compare
  Assert(Int32Compare(-123, 123) = -1);
  Assert(Int32Compare(123, 123) = 0);
  Assert(Int32Compare(-123, -123) = 0);
  Assert(Int32Compare(123, -123) = 1);
end;

procedure Test_Int64;
begin
  // Sign
  Assert(Int64Sign(0) = 0);
  Assert(Int64Sign(1234567890123456789) = 1);
  Assert(Int64Sign(-1234567890123456789) = -1);

  Assert(not Int64IsWord8Range(-1));
  Assert(not Int64IsWord8Range(256));
  Assert(Int64IsWord8Range(255));
  Assert(Int64IsWord16Range($FFFF));
  Assert(not Int64IsWord16Range($10000));
  Assert(Int64IsInt8Range(-1));

  // Compare
  Assert(Int64Compare(-123, 123) = -1);
  Assert(Int64Compare(123, 123) = 0);
  Assert(Int64Compare(-123, -123) = 0);
  Assert(Int64Compare(123, -123) = 1);
end;

procedure Test_Int128;
var A, B : Int128;
    C    : Int128;
    D    : Byte;
    E    : Word;
    F    : Word64;
    G    : Word128;
    H    : LongWord;
    K    : Word64;
    L    : Word128;
    M    : ShortInt;
    N    : SmallInt;
    P    : LongInt;
    Q    : Int64;
    T    : Int128;
    X    : Extended;

begin
  // Size
  Assert(SizeOf(Int128) = 16);

  // 0
  Int128InitZero(A);
  Assert(Int128ToWord32(A) = 0);
  Assert(Int128ToInt32(A) = 0);
  Assert(Int128ToInt64(A) = 0);
  Assert(Int128IsZero(A));
  Assert(not Int128IsNegative(A));

  // 1
  Int128InitOne(A);
  Assert(Int128IsOne(A));
  Assert(Int128ToWord32(A) = 1);
  Assert(Int128ToInt32(A) = 1);
  Assert(Int128ToInt64(A) = 1);
  Assert(Int128IsWord32Range(A));
  Assert(Int128IsWord64Range(A));
  Assert(Int128IsWord128Range(A));
  Assert(Int128IsInt32Range(A));
  Assert(Int128IsInt64Range(A));
  Assert(not Int128IsZero(A));
  Assert(not Int128IsNegative(A));
  Assert(Int128IsOdd(A));

  // -1
  Int128InitMinusOne(A);
  Assert(Int128IsMinusOne(A));
  Assert(Int128ToInt32(A) = -1);
  Assert(Int128ToInt64(A) = -1);
  Assert(not Int128IsZero(A));
  Assert(Int128IsNegative(A));
  Assert(not Int128IsWord32Range(A));
  Assert(not Int128IsWord64Range(A));
  Assert(not Int128IsWord128Range(A));
  Assert(Int128IsInt32Range(A));
  Assert(Int128IsInt64Range(A));

  // Sign
  Int128InitZero(A);
  Assert(Int128Sign(A) = 0);
  Int128InitInt32(A, 10);
  Assert(Int128Sign(A) = 1);
  Int128InitInt32(A, -10);
  Assert(Int128Sign(A) = -1);
  Assert(not Int128IsZero(A));
  Assert(Int128IsNegative(A));

  // 7FFFFFFF
  Int128InitInt32(A, $7FFFFFFF);
  Assert(Int128IsInt32Range(A));
  Assert(Int128IsInt64Range(A));
  Assert(Int128IsWord32Range(A));
  Assert(Int128ToInt32(A) = $7FFFFFFF);
  Assert(Int128ToInt64(A) = $7FFFFFFF);
  Assert(Int128ToWord32(A) = $7FFFFFFF);
  Assert(Int128EqualsInt32(A, $7FFFFFFF));

  // 80000000
  Int128InitInt64(A, $80000000);
  Assert(not Int128IsInt32Range(A));
  Assert(Int128IsInt64Range(A));
  Assert(Int128IsWord32Range(A));
  Assert(Int128ToInt64(A) = $80000000);
  Assert(Int128ToWord32(A) = $80000000);
  Assert(Int128EqualsInt64(A, $80000000));
  Assert(not Int128IsOdd(A));

  // -7FFFFFFF
  Int128InitInt32(A, -$7FFFFFFF);
  Assert(Int128IsInt32Range(A));
  Assert(Int128IsInt64Range(A));
  Assert(not Int128IsWord32Range(A));
  Assert(Int128ToInt32(A) = -$7FFFFFFF);
  Assert(Int128ToInt64(A) = -$7FFFFFFF);
  Assert(Int128EqualsInt32(A, -$7FFFFFFF));
  Assert(Int128EqualsInt64(A, -$7FFFFFFF));

  // -80000000
  Int128InitInt32(A, Low(LongInt));
  Assert(Int128IsInt32Range(A));
  Assert(Int128IsInt64Range(A));
  Assert(not Int128IsWord32Range(A));
  Assert(Int128ToInt32(A) = Low(LongInt));
  Assert(Int128ToInt64(A) = Low(LongInt));
  Assert(Int128EqualsInt32(A, Low(LongInt)));
  Assert(Int128EqualsInt64(A, Low(LongInt)));

  // -80000001
  Int128InitInt64(A, -Int64($80000001));
  Assert(not Int128IsInt32Range(A));
  Assert(Int128IsInt64Range(A));
  Assert(not Int128IsWord32Range(A));
  Assert(Int128ToInt64(A) = -Int64($80000001));
  Assert(Int128EqualsInt64(A, -Int64($80000001)));

  // FFFFFFFF
  Int128InitWord32(A, $FFFFFFFF);
  Assert(not Int128IsInt32Range(A));
  Assert(Int128IsInt64Range(A));
  Assert(Int128IsWord32Range(A));
  Assert(Int128ToInt64(A) = $FFFFFFFF);
  Assert(Int128ToWord32(A) = $FFFFFFFF);
  Assert(Int128EqualsInt64(A, $FFFFFFFF));
  Assert(Int128EqualsWord32(A, $FFFFFFFF));
  Assert(Int128BitCount(A) = 32);

  // Int128EqualsInt128
  Int128InitInt64(A, $10000000000);
  B := A;
  Assert(Int128EqualsInt128(A, B));
  Int128InitInt64(A, -$10000000000);
  Assert(not Int128EqualsInt128(A, B));
  Int128InitInt64(B, -$10000000000);
  Assert(Int128EqualsInt128(A, B));

  // Int128Equals
  Word64InitInt64(F, $1234567890ABCDEF);
  Word128InitInt64(G, $1234567890ABCDEF);
  Int128InitInt64(A, $1234567890ABCDEF);
  Assert(Int128EqualsWord64(A, F));
  Assert(Int128EqualsWord128(A, G));
  Assert(not Int128EqualsWord32(A, $90ABCDEF));
  Int128InitInt64(A, $90ABCDEF);
  Assert(Int128EqualsWord32(A, $90ABCDEF));
  Assert(not Int128EqualsWord64(A, F));
  Assert(not Int128EqualsWord128(A, G));
  Int128InitWord64(A, F);
  Assert(Int128EqualsWord64(A, F));
  F := Int128ToWord64(A);
  Assert(Int128EqualsWord64(A, F));
  Int128InitWord128(A, G);
  Assert(Int128EqualsWord128(A, G));
  G := Int128ToWord128(A);
  Assert(Int128EqualsWord128(A, G));

  // Int128CompareInt32
  Int128InitInt32(A, 1);
  Assert(Int128CompareInt32(A, 0) = 1);
  Assert(Int128CompareInt32(A, 1) = 0);
  Assert(Int128CompareInt32(A, 2) = -1);
  Assert(Int128CompareInt32(A, -1) = 1);
  Int128InitInt32(A, -1);
  Assert(Int128CompareInt32(A, -2) = 1);
  Assert(Int128CompareInt32(A, -1) = 0);
  Assert(Int128CompareInt32(A, 0) = -1);
  Assert(Int128CompareInt32(A, 1) = -1);
  Int128InitInt64(A, -5000000000000000000);
  Assert(Int128CompareInt32(A, 100) = -1);
  Assert(Int128CompareInt32(A, -100) = -1);

  // Int128CompareInt64
  Int128InitInt64(A, 1);
  Assert(Int128CompareInt64(A, 0) = 1);
  Assert(Int128CompareInt64(A, 1) = 0);
  Assert(Int128CompareInt64(A, 2) = -1);
  Assert(Int128CompareInt64(A, -1) = 1);
  Int128InitInt64(A, -1);
  Assert(Int128CompareInt64(A, -2) = 1);
  Assert(Int128CompareInt64(A, -1) = 0);
  Assert(Int128CompareInt64(A, 0) = -1);
  Assert(Int128CompareInt64(A, 1) = -1);
  Int128InitInt64(A, -5000000000000000000);
  Assert(Int128CompareInt64(A, 100) = -1);
  Assert(Int128CompareInt64(A, -100) = -1);

  // Int128CompareInt128
  Int128InitInt64(A, -5000000000000000000);
  Assert(Int128CompareInt128(A, A) = 0);
  Int128InitInt32(B, 100);
  Assert(Int128CompareInt128(A, B) = -1);
  Assert(Int128CompareInt128(B, A) = 1);
  Int128InitInt32(B, -100);
  Assert(Int128CompareInt128(A, B) = -1);
  Assert(Int128CompareInt128(B, A) = 1);

  // Int128Compare
  Int128InitInt64(A, $12345678);
  Assert(Int128CompareWord32(A, $12345678) = 0);
  Assert(Int128CompareWord32(A, 0) = 1);
  Assert(Int128CompareWord32(A, $22345678) = -1);
  Word64InitInt64(F, $12345678);
  Assert(Int128CompareWord64(A, F) = 0);
  Word128InitInt64(G, $12345678);
  Assert(Int128CompareWord128(A, G) = 0);
  Word64InitWord32(F, $22345678);
  Assert(Int128CompareWord64(A, F) = -1);
  Int128InitInt64(A, -$12345678);
  Assert(Int128CompareWord64(A, F) = -1);
  Assert(Int128CompareWord128(A, G) = -1);

  // Int128Min/Max
  Int128InitInt64(A, $1234567890ABCDEF);
  Int128InitInt64(B, $2234567890ABCDEF);
  Assert(not Int128EqualsInt128(A, B));
  C := Int128Max(A, B);
  Assert(Int128EqualsInt128(C, B));
  C := Int128Min(A, B);
  Assert(Int128EqualsInt128(C, A));

  // Int128Negate
  Int128InitInt32(A, 0);
  Int128Negate(A);
  Assert(Int128ToInt32(A) = 0);
  Int128InitInt32(A, 1);
  Int128Negate(A);
  Assert(Int128ToInt32(A) = -1);
  Int128Negate(A);
  Assert(Int128ToInt32(A) = 1);
  Int128InitInt64(A, -Int64($80000000));
  Int128Negate(A);
  Assert(Int128ToInt64(A) = $80000000);
  Int128Negate(A);
  Assert(Int128ToInt64(A) = -Int64($80000000));
  Int128InitInt64(A, $100000000);
  Int128Negate(A);
  Assert(Int128ToInt64(A) = -$100000000);
  Int128Negate(A);
  Assert(Int128ToInt64(A) = $100000000);
  Int128InitMaximum(A);
  Int128Negate(A);
  Assert(not Int128IsMinimum(A));
  Int128Negate(A);
  Assert(Int128IsMaximum(A));

  // Float
  Int128InitFloat(A, 1.0);
  Assert(Int128IsOne(A));
  Assert(Int128ToFloat(A) = 1.0);
  Int128InitInt32(A, -1);
  Assert(Int128ToFloat(A) = -1.0);
  Int128InitFloat(A, -1311768469162688511.0);
  {$IFNDEF CPU_X86_64} // FAIL
  Assert(Int128ToInt64(A) = -$12345678FFFFFFFF);
  {$ENDIF}
  Assert(Int128ToFloat(A) = -1311768469162688511.0);

  // Logic
  Int128InitZero(A);
  Int128Not(A);
  Assert(Int128ToInt64(A) = -1);
  Assert(Int128BitCount(A) = 128);
  Int128Not(A);
  Assert(Int128IsZero(A));
  Int128InitInt64(B, -1);
  Int128OrInt128(A, B);
  Assert(Int128ToInt64(A) = -1);
  Int128InitOne(B);
  Int128AndInt128(A, B);
  Assert(Int128IsOne(A));
  Assert(Int128IsBitSet(A, 0));
  Assert(not Int128IsBitSet(A, 1));
  Int128XorInt128(A, B);
  Assert(Int128IsZero(A));
  Int128SetBit(A, 0);
  Assert(Int128IsOne(A));
  Assert(Int128SetBitScanForward(A) = 0);
  Assert(Int128SetBitScanReverse(A) = 0);
  Assert(Int128ClearBitScanForward(A) = 1);
  Assert(Int128ClearBitScanReverse(A) = 127);
  Int128ClearBit(A, 0);
  Assert(Int128IsZero(A));
  Int128ToggleBit(A, 0);
  Assert(Int128IsOne(A));
  Int128Shl(A, 33);
  Assert(Int128ToInt64(A) = $200000000);
  Int128Shl1(A);
  Assert(Int128ToInt64(A) = $400000000);
  Int128Shr1(A);
  Assert(Int128ToInt64(A) = $200000000);
  Int128Shr(A, 33);
  Assert(Int128IsOne(A));
  Int128Shr1(A);
  Assert(Int128IsZero(A));
  Int128InitOne(A);
  Int128Ror(A, 95);
  Assert(Int128ToInt64(A) = $200000000);
  Int128Ror1(A);
  Assert(Int128ToInt64(A) = $100000000);
  Int128Rol1(A);
  Assert(Int128ToInt64(A) = $200000000);
  Int128Rol(A, 95);
  Assert(Int128IsOne(A));
  Assert(Int128BitCount(A) = 1);
  Assert(Int128IsPowerOfTwo(A));

  // Swap
  Int128InitInt64(A, $1234567890ABCDEF);
  Int128InitInt64(B, 1);
  Int128Swap(A, B);
  Assert(Int128ToInt64(A) = 1);
  Assert(Int128ToInt64(B) = $1234567890ABCDEF);

  // Int128Add
  Int128InitInt32(A, -1);
  Word64InitOne(F);
  Int128AddWord64(A, F);
  Assert(Int128IsZero(A));
  Int128AddWord64(A, F);
  Assert(Int128IsOne(A));
  Int128InitInt32(A, -1);
  Word128InitOne(G);
  Int128AddWord128(A, G);
  Assert(Int128IsZero(A));
  Int128AddWord128(A, G);
  Assert(Int128IsOne(A));
  Int128InitInt32(A, -1);
  Int128AddInt64(A, 1);
  Assert(Int128ToInt32(A) = 0);
  Int128AddInt64(A, 1);
  Assert(Int128ToInt32(A) = 1);
  Int128AddInt64(A, -1);
  Assert(Int128ToInt32(A) = 0);
  Int128AddInt64(A, -1);
  Assert(Int128ToInt32(A) = -1);

  // Int128AddWord32
  Int128InitInt32(A, -1);
  Int128AddWord32(A, 1);
  Assert(Int128ToInt32(A) = 0);
  Int128AddWord32(A, 1);
  Assert(Int128ToInt32(A) = 1);
  Int128AddWord32(A, $FFFFFFFF);
  Assert(Int128ToInt64(A) = $100000000);
  Int128InitInt64(A, $FFFFFFFF);
  Int128AddWord32(A, 1);
  Assert(Int128ToInt64(A) = $100000000);
  Int128InitInt32(A, 0);
  Int128AddWord32(A, 0);
  Assert(Int128ToInt32(A) = 0);
  Int128AddWord32(A, 1);
  Assert(Int128ToInt32(A) = 1);
  Int128InitInt32(A, -1);
  Int128AddWord32(A, 0);
  Assert(Int128ToInt32(A) = -1);
  Int128AddWord32(A, 1);
  Assert(Int128ToInt32(A) = 0);
  Int128InitInt32(A, 1);
  Int128AddWord32(A, 0);
  Assert(Int128ToInt32(A) = 1);

  // Int128Subtract
  Int128InitInt32(A, 1);
  Word64InitOne(F);
  Int128SubtractWord64(A, F);
  Assert(Int128IsZero(A));
  Int128SubtractWord64(A, F);
  Assert(Int128IsMinusOne(A));
  Int128InitInt32(A, 1);
  Word128InitOne(G);
  Int128SubtractWord128(A, G);
  Assert(Int128IsZero(A));
  Int128SubtractWord128(A, G);
  Assert(Int128IsMinusOne(A));
  Int128InitInt32(A, 1);
  Int128SubtractInt64(A, 1);
  Assert(Int128ToInt32(A) = 0);
  Int128SubtractInt64(A, 1);
  Assert(Int128ToInt32(A) = -1);
  Int128SubtractInt64(A, -1);
  Assert(Int128ToInt32(A) = 0);
  Int128SubtractInt64(A, -1);
  Assert(Int128ToInt32(A) = 1);

  // Int128SubtractWord32
  Int128InitInt32(A, 1);
  Int128SubtractWord32(A, 1);
  Assert(Int128ToInt32(A) = 0);
  Int128SubtractWord32(A, 1);
  Assert(Int128ToInt32(A) = -1);
  Int128SubtractWord32(A, 1);
  Assert(Int128ToInt32(A) = -2);
  Int128InitInt64(A, $100000000);
  Int128SubtractWord32(A, 1);
  Assert(Int128ToInt64(A) = $FFFFFFFF);

  // Int128AddInt32
  Int128InitInt32(A, 5);
  Int128AddInt32(A, 9);
  Assert(Int128ToInt32(A) = 14);
  Int128InitInt32(A, -5);
  Int128AddInt32(A, 9);
  Assert(Int128ToInt32(A) = 4);
  Int128InitWord32(A, $FFFFFFFF);
  Int128AddInt32(A, 1);
  Assert(Int128ToInt64(A) = $100000000);
  Int128AddInt32(A, -1);
  Assert(Int128ToInt64(A) = $FFFFFFFF);
  Int128InitInt32(A, 0);
  Int128AddInt32(A, -1);
  Assert(Int128ToInt32(A) = -1);
  Int128AddInt32(A, 1);
  Assert(Int128ToInt32(A) = 0);
  Int128InitInt32(A, 0);
  Int128AddInt32(A, Low(LongInt));
  Assert(Int128ToInt32(A) = Low(LongInt));

  // Int128SubtractInt32
  Int128InitInt32(A, 0);
  Int128SubtractInt32(A, 1);
  Assert(Int128ToInt32(A) = -1);
  Int128SubtractInt32(A, -1);
  Assert(Int128ToInt32(A) = 0);
  Int128InitInt32(A, 0);
  Int128SubtractInt32(A, $7FFFFFFF);
  Assert(Int128ToInt32(A) = -$7FFFFFFF);
  Int128SubtractInt32(A, $7FFFFFFF);
  Assert(Int128ToInt64(A) = -Int64($FFFFFFFE));
  Int128SubtractInt32(A, $7FFFFFFF);
  Assert(Int128ToInt64(A) = -$17FFFFFFD);

  // Int128AddInt128
  Int128InitInt32(A, $7FFFFFFF);
  Int128InitInt32(B, $70000000);
  Int128AddInt128(A, B);
  Assert(Int128ToInt64(A) = $EFFFFFFF);
  Assert(not Int128IsInt32Range(A));
  Assert(Int128IsWord32Range(A));
  Int128AddInt128(A, B);
  Assert(Int128ToInt64(A) = $15FFFFFFF);
  Assert(not Int128IsWord32Range(A));
  Assert(Int128IsInt64Range(A));
  Int128InitInt32(A, -10);
  Int128InitInt32(B, 30);
  Int128AddInt128(A, B);
  Assert(Int128ToInt32(A) = 20);
  Int128InitInt32(A, 10);
  Int128InitInt32(B, -30);
  Int128AddInt128(A, B);
  Assert(Int128ToInt32(A) = -20);
  Int128InitInt64(A, $100000000);
  Int128InitInt64(B, -$100000000);
  Int128AddInt128(A, B);
  Assert(Int128ToInt32(A) = 0);
  Int128InitInt64(A, -$100000000);
  Int128InitInt64(B, $100000000);
  Int128AddInt128(A, B);
  Assert(Int128ToInt32(A) = 0);

  // Int128SubtractInt128
  Int128InitInt32(A, 30);
  Int128InitInt32(B, 10);
  Int128SubtractInt128(A, B);
  Assert(Int128ToInt32(A) = 20);
  Int128InitInt32(A, 10);
  Int128InitInt32(B, 30);
  Int128SubtractInt128(A, B);
  Assert(Int128ToInt32(A) = -20);
  Int128InitInt32(A, 1000000000);
  Int128InitInt32(B, 2000000000);
  Int128SubtractInt128(A, B);
  Assert(Int128ToInt64(A) = -1000000000);
  Int128SubtractInt128(A, B);
  Int128SubtractInt128(A, B);
  Assert(Int128ToInt64(A) = -5000000000);
  Int128InitInt64(A, $100000000);
  Int128InitInt64(B, $100000000);
  Int128SubtractInt128(A, B);
  Assert(Int128ToInt32(A) = 0);
  Int128InitInt64(A, -$100000000);
  Int128InitInt64(B, -$100000000);
  Int128SubtractInt128(A, B);
  Assert(Int128ToInt32(A) = 0);

  // Int128Multiply
  Int128InitInt32(A, 1000000000);
  Int128MultiplyWord8(A, 5);
  Assert(Int128ToInt64(A) = 5000000000);
  Int128InitInt32(A, 1000000000);
  Int128MultiplyWord16(A, 5);
  Assert(Int128ToInt64(A) = 5000000000);
  Int128InitInt32(A, 1000000000);
  Word64InitInt32(F, 5);
  Int128MultiplyWord64(A, F);
  Assert(Int128ToInt64(A) = 5000000000);
  Int128InitInt32(A, -1000000000);
  Word64InitInt32(F, 5);
  Int128MultiplyWord64(A, F);
  Assert(Int128ToInt64(A) = -5000000000);
  Int128InitInt32(A, 1000000000);
  Word128InitInt32(G, 5);
  Int128MultiplyWord128(A, G);
  Assert(Int128ToInt64(A) = 5000000000);
  Int128InitInt32(A, -1000000000);
  Word128InitInt32(G, 5);
  Int128MultiplyWord128(A, G);
  Assert(Int128ToInt64(A) = -5000000000);
  Int128InitInt32(A, 1000000000);
  Int128MultiplyInt8(A, -5);
  Assert(Int128ToInt64(A) = -5000000000);
  Int128InitInt32(A, 1000000000);
  Int128MultiplyInt16(A, -5);
  Assert(Int128ToInt64(A) = -5000000000);
  Int128InitInt32(A, 1000000000);
  Int128MultiplyInt32(A, -5);
  Assert(Int128ToInt64(A) = -5000000000);
  Int128InitInt32(A, 1000000000);
  Int128MultiplyInt64(A, -5);
  Assert(Int128ToInt64(A) = -5000000000);
  Int128InitInt32(A, -1000000000);
  Int128MultiplyInt64(A, -5);
  Assert(Int128ToInt64(A) = 5000000000);

  // Int128MultiplyWord32
  Int128InitInt32(A, 1000000000);
  Int128MultiplyWord32(A, 5);
  Assert(Int128ToInt64(A) = 5000000000);
  Int128MultiplyWord32(A, 1);
  Assert(Int128ToInt64(A) = 5000000000);
  Int128MultiplyWord32(A, 1000000000);
  Assert(Int128ToInt64(A) = 5000000000000000000);
  Int128MultiplyWord32(A, 0);
  Assert(Int128ToInt32(A) = 0);
  Int128InitInt32(A, -1000000000);
  Int128MultiplyWord32(A, 5);
  Assert(Int128ToInt64(A) = -5000000000);
  Int128MultiplyWord32(A, 1000000000);
  Assert(Int128ToInt64(A) = -5000000000000000000);

  // Int128MultiplyInt128
  Int128InitInt32(A, 10);
  Int128InitInt32(B, 30);
  Int128MultiplyInt128(A, B);
  Assert(Int128ToInt64(A) = 300);
  Int128InitInt64(A, 1000000000);
  Int128InitInt64(B, 5000000000);
  Assert(Int128CompareInt128(A, B) = -1);
  Assert(Int128CompareInt128(B, A) = 1);
  Assert(Int128CompareInt128(A, A) = 0);
  Int128MultiplyInt128(A, B);
  Assert(Int128IsInt64Range(A));
  Assert(Int128ToInt64(A) = 5000000000000000000);
  Int128InitInt64(A, 1000000000);
  Int128InitInt64(B, -5000000000);
  Int128MultiplyInt128(A, B);
  Assert(Int128ToInt64(A) = -5000000000000000000);

  // Int128Divide
  Int128InitInt64(A, -5000000000000000001);
  Int128DivideWord8(A, 5, B, D);
  Assert(Int128ToInt64(B) = -1000000000000000000);
  Assert(D = 1);
  Int128InitInt64(A, -5000000000000000001);
  Int128DivideWord16(A, 50000, B, E);
  Assert(Int128ToInt64(B) = -100000000000000);
  Assert(E = 1);
  Int128InitInt64(A, -5000000000000000001);
  Int128DivideWord32(A, 500000000, B, H);
  Assert(Int128ToInt64(B) = -10000000000);
  Assert(H = 1);
  Int128InitInt64(A, -5000000000000000001);
  Word64InitInt64(F, 500000000);
  Int128DivideWord64(A, F, B, K);
  Assert(Int128ToInt64(B) = -10000000000);
  Assert(Word64ToInt64(K) = 1);
  Int128InitInt64(A, -5000000000000000001);
  Word128InitInt64(G, 500000000);
  Int128DivideWord128(A, G, B, L);
  Assert(Int128ToInt64(B) = -10000000000);
  Assert(Word128ToInt64(L) = 1);
  Int128InitInt64(A, -5000000000000000001);
  Int128DivideInt8(A, 5, B, M);
  Assert(Int128ToInt64(B) = -1000000000000000000);
  Assert(M = 1);
  Int128InitInt64(A, -5000000000000000001);
  Int128DivideInt16(A, 5000, B, N);
  Assert(Int128ToInt64(B) = -1000000000000000);
  Assert(E = 1);
  Int128InitInt64(A, -5000000000000000001);
  Int128DivideInt32(A, 50000000, B, P);
  Assert(Int128ToInt64(B) = -100000000000);
  Assert(H = 1);
  Int128InitInt64(A, -5000000000000000001);
  Int128DivideInt64(A, 50000000, B, Q);
  Assert(Int128ToInt64(B) = -100000000000);
  Assert(Word64ToInt64(K) = 1);
  Int128InitInt64(A, -5000000000000000001);
  Int128InitInt64(C, 500000000);
  Int128DivideInt128(A, C, B, T);
  Assert(Int128ToInt64(B) = -10000000000);
  Assert(Word128ToInt64(L) = 1);

  // Int128Sqr
  Int128InitInt64(A, -1000000000);
  Int128Sqr(A);
  Assert(Int128ToInt64(A) = 1000000000000000000);

  // String
  Int128InitInt64(A, 1234567890);
  Assert(Int128ToStr(A) = '1234567890');
  Assert(Int128ToStrA(A) = '1234567890');
  Assert(Int128ToStrU(A) = '1234567890');
  Int128InitInt64(A, -1234567890);
  Assert(Int128ToStr(A) = '-1234567890');
  Assert(Int128ToStrA(A) = '-1234567890');
  Assert(Int128ToStrU(A) = '-1234567890');
  A := StrToInt128('-1234567890');
  Assert(Int128ToInt64(A) = -1234567890);
  A := StrToInt128A('-1234567890');
  Assert(Int128ToInt64(A) = -1234567890);
  A := StrToInt128U('-1234567890');
  Assert(Int128ToInt64(A) = -1234567890);

  // Int128Sqrt
  Int128InitInt64(A, 12345 * 12345 + 12344);
  Int128ISqrt(A);
  Assert(Int128ToInt64(A) = 12345);
  Int128InitInt64(A, 12345 * 12345 + 12344);
  X := Int128Sqrt(A);
  Assert(Abs(X - 12345.49994937427) < 1e-11);

  // Power
  Int128InitInt64(A, -2);
  Int128Power(A, 15);
  Assert(Int128ToInt64(A) = -32768);
end;

procedure Test_Int256;
var A : Int256;
begin
  // Size
  Assert(SizeOf(Int256) = 32);

  // 0
  Int256InitZero(A);
  Assert(Int256ToWord32(A) = 0);
  Assert(Int256ToInt32(A) = 0);
  Assert(Int256ToInt64(A) = 0);
  Assert(Int256IsZero(A));
  Assert(not Int256IsNegative(A));

  // 1
  Int256InitOne(A);
  Assert(Int256IsOne(A));
  Assert(Int256ToWord32(A) = 1);
  Assert(Int256ToInt32(A) = 1);
  Assert(Int256ToInt64(A) = 1);
  Assert(Int256IsWord32Range(A));
  Assert(Int256IsWord64Range(A));
  Assert(Int256IsWord128Range(A));
  Assert(Int256IsInt32Range(A));
  Assert(Int256IsInt64Range(A));
  Assert(not Int256IsZero(A));
  Assert(not Int256IsNegative(A));
  Assert(Int256IsOdd(A));

  // -1
  Int256InitMinusOne(A);
  Assert(Int256IsMinusOne(A));
  Assert(Int256ToInt32(A) = -1);
  Assert(Int256ToInt64(A) = -1);
  Assert(not Int256IsZero(A));
  Assert(Int256IsNegative(A));
  Assert(not Int256IsWord32Range(A));
  Assert(not Int256IsWord64Range(A));
  Assert(not Int256IsWord128Range(A));
  Assert(Int256IsInt32Range(A));
  Assert(Int256IsInt64Range(A));

  // Sign
  Int256InitZero(A);
  Assert(Int256Sign(A) = 0);
  Int256InitInt32(A, 10);
  Assert(Int256Sign(A) = 1);
  Int256InitInt32(A, -10);
  Assert(Int256Sign(A) = -1);
  Assert(not Int256IsZero(A));
  Assert(Int256IsNegative(A));

  // 7FFFFFFF
  Int256InitInt32(A, $7FFFFFFF);
  Assert(Int256IsInt32Range(A));
  Assert(Int256IsInt64Range(A));
  Assert(Int256IsWord32Range(A));
  Assert(Int256ToInt32(A) = $7FFFFFFF);
  Assert(Int256ToInt64(A) = $7FFFFFFF);
  Assert(Int256ToWord32(A) = $7FFFFFFF);

  // 80000000
  Int256InitInt64(A, $80000000);
  Assert(not Int256IsInt32Range(A));
  Assert(Int256IsInt64Range(A));
  Assert(Int256IsWord32Range(A));
  Assert(Int256ToInt64(A) = $80000000);
  Assert(Int256ToWord32(A) = $80000000);
  Assert(not Int256IsOdd(A));

  // -7FFFFFFF
  Int256InitInt32(A, -$7FFFFFFF);
  Assert(Int256IsInt32Range(A));
  Assert(Int256IsInt64Range(A));
  Assert(not Int256IsWord32Range(A));
  Assert(Int256ToInt32(A) = -$7FFFFFFF);
  Assert(Int256ToInt64(A) = -$7FFFFFFF);

  // -80000000
  Int256InitInt32(A, Low(LongInt));
  Assert(Int256IsInt32Range(A));
  Assert(Int256IsInt64Range(A));
  Assert(not Int256IsWord32Range(A));
  Assert(Int256ToInt32(A) = Low(LongInt));
  Assert(Int256ToInt64(A) = Low(LongInt));

  // -80000001
  Int256InitInt64(A, -Int64($80000001));
  Assert(not Int256IsInt32Range(A));
  Assert(Int256IsInt64Range(A));
  Assert(not Int256IsWord32Range(A));
  Assert(Int256ToInt64(A) = -Int64($80000001));

  // FFFFFFFF
  Int256InitWord32(A, $FFFFFFFF);
  Assert(not Int256IsInt32Range(A));
  Assert(Int256IsInt64Range(A));
  Assert(Int256IsWord32Range(A));
  Assert(Int256ToInt64(A) = $FFFFFFFF);
  Assert(Int256ToWord32(A) = $FFFFFFFF);
end;

procedure Test_Int512;
var A : Int512;
begin
  // Size
  Assert(SizeOf(Int512) = 512 div 8 + 1);

  // 0
  Int512InitZero(A);
  Assert(Int512ToWord32(A) = 0);
  Assert(Int512ToInt32(A) = 0);
  Assert(Int512ToInt64(A) = 0);
  Assert(Int512IsZero(A));
  Assert(not Int512IsNegative(A));

  // 1
  Int512InitOne(A);
  Assert(Int512IsOne(A));
  Assert(Int512ToWord32(A) = 1);
  Assert(Int512ToInt32(A) = 1);
  Assert(Int512ToInt64(A) = 1);
  Assert(Int512IsWord32Range(A));
  Assert(Int512IsWord64Range(A));
  Assert(Int512IsWord128Range(A));
  Assert(Int512IsInt32Range(A));
  Assert(Int512IsInt64Range(A));
  Assert(not Int512IsZero(A));
  Assert(not Int512IsNegative(A));
  Assert(Int512IsOdd(A));

  // -1
  Int512InitMinusOne(A);
  Assert(Int512IsMinusOne(A));
  Assert(Int512ToInt32(A) = -1);
  Assert(Int512ToInt64(A) = -1);
  Assert(not Int512IsZero(A));
  Assert(Int512IsNegative(A));
  Assert(not Int512IsWord32Range(A));
  Assert(not Int512IsWord64Range(A));
  Assert(not Int512IsWord128Range(A));
  Assert(Int512IsInt32Range(A));
  Assert(Int512IsInt64Range(A));

  // Sign
  Int512InitZero(A);
  Assert(Int512Sign(A) = 0);
  Int512InitInt32(A, 10);
  Assert(Int512Sign(A) = 1);
  Int512InitInt32(A, -10);
  Assert(Int512Sign(A) = -1);
  Assert(not Int512IsZero(A));
  Assert(Int512IsNegative(A));

  // 7FFFFFFF
  Int512InitInt32(A, $7FFFFFFF);
  Assert(Int512IsInt32Range(A));
  Assert(Int512IsInt64Range(A));
  Assert(Int512IsWord32Range(A));
  Assert(Int512ToInt32(A) = $7FFFFFFF);
  Assert(Int512ToInt64(A) = $7FFFFFFF);
  Assert(Int512ToWord32(A) = $7FFFFFFF);

  // 80000000
  Int512InitInt64(A, $80000000);
  Assert(not Int512IsInt32Range(A));
  Assert(Int512IsInt64Range(A));
  Assert(Int512IsWord32Range(A));
  Assert(Int512ToInt64(A) = $80000000);
  Assert(Int512ToWord32(A) = $80000000);
  Assert(not Int512IsOdd(A));

  // -7FFFFFFF
  Int512InitInt32(A, -$7FFFFFFF);
  Assert(Int512IsInt32Range(A));
  Assert(Int512IsInt64Range(A));
  Assert(not Int512IsWord32Range(A));
  Assert(Int512ToInt32(A) = -$7FFFFFFF);
  Assert(Int512ToInt64(A) = -$7FFFFFFF);

  // -80000000
  Int512InitInt32(A, Low(LongInt));
  Assert(Int512IsInt32Range(A));
  Assert(Int512IsInt64Range(A));
  Assert(not Int512IsWord32Range(A));
  Assert(Int512ToInt32(A) = Low(LongInt));
  Assert(Int512ToInt64(A) = Low(LongInt));

  // -80000001
  Int512InitInt64(A, -Int64($80000001));
  Assert(not Int512IsInt32Range(A));
  Assert(Int512IsInt64Range(A));
  Assert(not Int512IsWord32Range(A));
  Assert(Int512ToInt64(A) = -Int64($80000001));

  // FFFFFFFF
  Int512InitWord32(A, $FFFFFFFF);
  Assert(not Int512IsInt32Range(A));
  Assert(Int512IsInt64Range(A));
  Assert(Int512IsWord32Range(A));
  Assert(Int512ToInt64(A) = $FFFFFFFF);
  Assert(Int512ToWord32(A) = $FFFFFFFF);
end;

procedure Test_Int1024;
var A : Int1024;
begin
  // Size
  Assert(SizeOf(Int1024) = 1024 div 8 + 1);

  // 0
  Int1024InitZero(A);
  Assert(Int1024ToWord32(A) = 0);
  Assert(Int1024ToInt32(A) = 0);
  Assert(Int1024ToInt64(A) = 0);
  Assert(Int1024IsZero(A));
  Assert(not Int1024IsNegative(A));

  // 1
  Int1024InitOne(A);
  Assert(Int1024IsOne(A));
  Assert(Int1024ToWord32(A) = 1);
  Assert(Int1024ToInt32(A) = 1);
  Assert(Int1024ToInt64(A) = 1);
  Assert(Int1024IsWord32Range(A));
  Assert(Int1024IsWord64Range(A));
  Assert(Int1024IsWord128Range(A));
  Assert(Int1024IsInt32Range(A));
  Assert(Int1024IsInt64Range(A));
  Assert(not Int1024IsZero(A));
  Assert(not Int1024IsNegative(A));
  Assert(Int1024IsOdd(A));

  // -1
  Int1024InitMinusOne(A);
  Assert(Int1024IsMinusOne(A));
  Assert(Int1024ToInt32(A) = -1);
  Assert(Int1024ToInt64(A) = -1);
  Assert(not Int1024IsZero(A));
  Assert(Int1024IsNegative(A));
  Assert(not Int1024IsWord32Range(A));
  Assert(not Int1024IsWord64Range(A));
  Assert(not Int1024IsWord128Range(A));
  Assert(Int1024IsInt32Range(A));
  Assert(Int1024IsInt64Range(A));

  // Sign
  Int1024InitZero(A);
  Assert(Int1024Sign(A) = 0);
  Int1024InitInt32(A, 10);
  Assert(Int1024Sign(A) = 1);
  Int1024InitInt32(A, -10);
  Assert(Int1024Sign(A) = -1);
  Assert(not Int1024IsZero(A));
  Assert(Int1024IsNegative(A));

  // 7FFFFFFF
  Int1024InitInt32(A, $7FFFFFFF);
  Assert(Int1024IsInt32Range(A));
  Assert(Int1024IsInt64Range(A));
  Assert(Int1024IsWord32Range(A));
  Assert(Int1024ToInt32(A) = $7FFFFFFF);
  Assert(Int1024ToInt64(A) = $7FFFFFFF);
  Assert(Int1024ToWord32(A) = $7FFFFFFF);

  // 80000000
  Int1024InitInt64(A, $80000000);
  Assert(not Int1024IsInt32Range(A));
  Assert(Int1024IsInt64Range(A));
  Assert(Int1024IsWord32Range(A));
  Assert(Int1024ToInt64(A) = $80000000);
  Assert(Int1024ToWord32(A) = $80000000);
  Assert(not Int1024IsOdd(A));

  // -7FFFFFFF
  Int1024InitInt32(A, -$7FFFFFFF);
  Assert(Int1024IsInt32Range(A));
  Assert(Int1024IsInt64Range(A));
  Assert(not Int1024IsWord32Range(A));
  Assert(Int1024ToInt32(A) = -$7FFFFFFF);
  Assert(Int1024ToInt64(A) = -$7FFFFFFF);

  // -80000000
  Int1024InitInt32(A, Low(LongInt));
  Assert(Int1024IsInt32Range(A));
  Assert(Int1024IsInt64Range(A));
  Assert(not Int1024IsWord32Range(A));
  Assert(Int1024ToInt32(A) = Low(LongInt));
  Assert(Int1024ToInt64(A) = Low(LongInt));

  // -80000001
  Int1024InitInt64(A, -Int64($80000001));
  Assert(not Int1024IsInt32Range(A));
  Assert(Int1024IsInt64Range(A));
  Assert(not Int1024IsWord32Range(A));
  Assert(Int1024ToInt64(A) = -Int64($80000001));

  // FFFFFFFF
  Int1024InitWord32(A, $FFFFFFFF);
  Assert(not Int1024IsInt32Range(A));
  Assert(Int1024IsInt64Range(A));
  Assert(Int1024IsWord32Range(A));
  Assert(Int1024ToInt64(A) = $FFFFFFFF);
  Assert(Int1024ToWord32(A) = $FFFFFFFF);
end;

procedure Test_Int2048;
var A : Int2048;
begin
  // Size
  Assert(SizeOf(Int2048) = 2048 div 8 + 1);

  // 0
  Int2048InitZero(A);
  Assert(Int2048ToWord32(A) = 0);
  Assert(Int2048ToInt32(A) = 0);
  Assert(Int2048ToInt64(A) = 0);
  Assert(Int2048IsZero(A));
  Assert(not Int2048IsNegative(A));

  // 1
  Int2048InitOne(A);
  Assert(Int2048IsOne(A));
  Assert(Int2048ToWord32(A) = 1);
  Assert(Int2048ToInt32(A) = 1);
  Assert(Int2048ToInt64(A) = 1);
  Assert(Int2048IsWord32Range(A));
  Assert(Int2048IsWord64Range(A));
  Assert(Int2048IsWord128Range(A));
  Assert(Int2048IsInt32Range(A));
  Assert(Int2048IsInt64Range(A));
  Assert(not Int2048IsZero(A));
  Assert(not Int2048IsNegative(A));
  Assert(Int2048IsOdd(A));

  // -1
  Int2048InitMinusOne(A);
  Assert(Int2048IsMinusOne(A));
  Assert(Int2048ToInt32(A) = -1);
  Assert(Int2048ToInt64(A) = -1);
  Assert(not Int2048IsZero(A));
  Assert(Int2048IsNegative(A));
  Assert(not Int2048IsWord32Range(A));
  Assert(not Int2048IsWord64Range(A));
  Assert(not Int2048IsWord128Range(A));
  Assert(Int2048IsInt32Range(A));
  Assert(Int2048IsInt64Range(A));

  // Sign
  Int2048InitZero(A);
  Assert(Int2048Sign(A) = 0);
  Int2048InitInt32(A, 10);
  Assert(Int2048Sign(A) = 1);
  Int2048InitInt32(A, -10);
  Assert(Int2048Sign(A) = -1);
  Assert(not Int2048IsZero(A));
  Assert(Int2048IsNegative(A));

  // 7FFFFFFF
  Int2048InitInt32(A, $7FFFFFFF);
  Assert(Int2048IsInt32Range(A));
  Assert(Int2048IsInt64Range(A));
  Assert(Int2048IsWord32Range(A));
  Assert(Int2048ToInt32(A) = $7FFFFFFF);
  Assert(Int2048ToInt64(A) = $7FFFFFFF);
  Assert(Int2048ToWord32(A) = $7FFFFFFF);

  // 80000000
  Int2048InitInt64(A, $80000000);
  Assert(not Int2048IsInt32Range(A));
  Assert(Int2048IsInt64Range(A));
  Assert(Int2048IsWord32Range(A));
  Assert(Int2048ToInt64(A) = $80000000);
  Assert(Int2048ToWord32(A) = $80000000);
  Assert(not Int2048IsOdd(A));

  // -7FFFFFFF
  Int2048InitInt32(A, -$7FFFFFFF);
  Assert(Int2048IsInt32Range(A));
  Assert(Int2048IsInt64Range(A));
  Assert(not Int2048IsWord32Range(A));
  Assert(Int2048ToInt32(A) = -$7FFFFFFF);
  Assert(Int2048ToInt64(A) = -$7FFFFFFF);

  // -80000000
  Int2048InitInt32(A, Low(LongInt));
  Assert(Int2048IsInt32Range(A));
  Assert(Int2048IsInt64Range(A));
  Assert(not Int2048IsWord32Range(A));
  Assert(Int2048ToInt32(A) = Low(LongInt));
  Assert(Int2048ToInt64(A) = Low(LongInt));

  // -80000001
  Int2048InitInt64(A, -Int64($80000001));
  Assert(not Int2048IsInt32Range(A));
  Assert(Int2048IsInt64Range(A));
  Assert(not Int2048IsWord32Range(A));
  Assert(Int2048ToInt64(A) = -Int64($80000001));

  // FFFFFFFF
  Int2048InitWord32(A, $FFFFFFFF);
  Assert(not Int2048IsInt32Range(A));
  Assert(Int2048IsInt64Range(A));
  Assert(Int2048IsWord32Range(A));
  Assert(Int2048ToInt64(A) = $FFFFFFFF);
  Assert(Int2048ToWord32(A) = $FFFFFFFF);
end;

procedure Test_Int4096;
var A : Int4096;
begin
  // Size
  Assert(SizeOf(Int4096) = 4096 div 8 + 1);

  // 0
  Int4096InitZero(A);
  Assert(Int4096ToWord32(A) = 0);
  Assert(Int4096ToInt32(A) = 0);
  Assert(Int4096ToInt64(A) = 0);
  Assert(Int4096IsZero(A));
  Assert(not Int4096IsNegative(A));

  // 1
  Int4096InitOne(A);
  Assert(Int4096IsOne(A));
  Assert(Int4096ToWord32(A) = 1);
  Assert(Int4096ToInt32(A) = 1);
  Assert(Int4096ToInt64(A) = 1);
  Assert(Int4096IsWord32Range(A));
  Assert(Int4096IsWord64Range(A));
  Assert(Int4096IsWord128Range(A));
  Assert(Int4096IsInt32Range(A));
  Assert(Int4096IsInt64Range(A));
  Assert(not Int4096IsZero(A));
  Assert(not Int4096IsNegative(A));
  Assert(Int4096IsOdd(A));

  // -1
  Int4096InitMinusOne(A);
  Assert(Int4096IsMinusOne(A));
  Assert(Int4096ToInt32(A) = -1);
  Assert(Int4096ToInt64(A) = -1);
  Assert(not Int4096IsZero(A));
  Assert(Int4096IsNegative(A));
  Assert(not Int4096IsWord32Range(A));
  Assert(not Int4096IsWord64Range(A));
  Assert(not Int4096IsWord128Range(A));
  Assert(Int4096IsInt32Range(A));
  Assert(Int4096IsInt64Range(A));

  // Sign
  Int4096InitZero(A);
  Assert(Int4096Sign(A) = 0);
  Int4096InitInt32(A, 10);
  Assert(Int4096Sign(A) = 1);
  Int4096InitInt32(A, -10);
  Assert(Int4096Sign(A) = -1);
  Assert(not Int4096IsZero(A));
  Assert(Int4096IsNegative(A));

  // 7FFFFFFF
  Int4096InitInt32(A, $7FFFFFFF);
  Assert(Int4096IsInt32Range(A));
  Assert(Int4096IsInt64Range(A));
  Assert(Int4096IsWord32Range(A));
  Assert(Int4096ToInt32(A) = $7FFFFFFF);
  Assert(Int4096ToInt64(A) = $7FFFFFFF);
  Assert(Int4096ToWord32(A) = $7FFFFFFF);

  // 80000000
  Int4096InitInt64(A, $80000000);
  Assert(not Int4096IsInt32Range(A));
  Assert(Int4096IsInt64Range(A));
  Assert(Int4096IsWord32Range(A));
  Assert(Int4096ToInt64(A) = $80000000);
  Assert(Int4096ToWord32(A) = $80000000);
  Assert(not Int4096IsOdd(A));

  // -7FFFFFFF
  Int4096InitInt32(A, -$7FFFFFFF);
  Assert(Int4096IsInt32Range(A));
  Assert(Int4096IsInt64Range(A));
  Assert(not Int4096IsWord32Range(A));
  Assert(Int4096ToInt32(A) = -$7FFFFFFF);
  Assert(Int4096ToInt64(A) = -$7FFFFFFF);

  // -80000000
  Int4096InitInt32(A, Low(LongInt));
  Assert(Int4096IsInt32Range(A));
  Assert(Int4096IsInt64Range(A));
  Assert(not Int4096IsWord32Range(A));
  Assert(Int4096ToInt32(A) = Low(LongInt));
  Assert(Int4096ToInt64(A) = Low(LongInt));

  // -80000001
  Int4096InitInt64(A, -Int64($80000001));
  Assert(not Int4096IsInt32Range(A));
  Assert(Int4096IsInt64Range(A));
  Assert(not Int4096IsWord32Range(A));
  Assert(Int4096ToInt64(A) = -Int64($80000001));

  // FFFFFFFF
  Int4096InitWord32(A, $FFFFFFFF);
  Assert(not Int4096IsInt32Range(A));
  Assert(Int4096IsInt64Range(A));
  Assert(Int4096IsWord32Range(A));
  Assert(Int4096ToInt64(A) = $FFFFFFFF);
  Assert(Int4096ToWord32(A) = $FFFFFFFF);
end;

procedure Test;
begin
  Assert(Word8192Size = 1024);
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Word8}');
  {$ENDIF}
  Test_Word8;
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Word16}');
  {$ENDIF}
  Test_Word16;
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Word32}');
  {$ENDIF}
  Test_Word32;
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Word64}');
  {$ENDIF}
  Test_Word64;
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Word128}');
  {$ENDIF}
  Test_Word128;
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Word256}');
  {$ENDIF}
  Test_Word256;
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Word512}');
  {$ENDIF}
  Test_Word512;
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Word1024}');
  {$ENDIF}
  Test_Word1024;
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Word2048}');
  {$ENDIF}
  Test_Word2048;
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Word4096}');
  {$ENDIF}
  Test_Word4096;
  {$IFDEF INTEGER_TEST_OUTPUT}
  Write('{Int}');
  {$ENDIF}
  Test_Int8;
  Test_Int16;
  Test_Int32;
  Test_Int64;
  Test_Int128;
  Test_Int256;
end;
{$ENDIF}



end.

