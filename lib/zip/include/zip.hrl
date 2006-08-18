%%% File    : zip.hrl
%%% Author  : Tony Rogvall <tony@bit.hemma.se>
%%% Description : ZIP file declarations
%%% Created : 25 Apr 2003 by Tony Rogvall <tony@bit.hemma.se>
-ifndef(__ZIP_HRL__).
-define(__ZIP_HRL__, true).

-define(Z_MDDOS,   0).
-define(Z_AMIGA,   1).
-define(Z_OPENVMS, 2).
-define(Z_UNIX,    3).
-define(Z_VMCMS,   4).
-define(Z_ATARI,   5).
-define(Z_OS2,     6).
-define(Z_MAC,     7).
-define(Z_ZSYS,    8).
-define(Z_CPM,     9).
-define(Z_NTFS,   10).
-define(Z_MVS,    11).
-define(Z_VSE,    12).
-define(Z_ACORN,  13).
-define(Z_VFAT,   14).
-define(Z_AMVS,   15).
-define(Z_BEOS,   16).
-define(Z_TANDEM, 17).


-define(Z_METH_UNCOMPRESSED, 0).
-define(Z_METH_SHRUNK,       1).
-define(Z_METH_REDUCED1,     2).
-define(Z_METH_REDUCED2,     3).
-define(Z_METH_REDUCED3,     4).
-define(Z_METH_REDUCED4,     5).
-define(Z_METH_IMPLODED,     6).
-define(Z_METH_RESERVED,     7).
-define(Z_METH_DEFLATED,     8).
-define(Z_METH_DEFLATED64,   9).
-define(Z_METH_DCLI,        10).


-define(ZIP_LFILE_SIG, 16#04034B50).

-record(zip_lfile, {
          sig,       %% :32 - Local File Header Signature
          ver,       %% :16 - Version needed to extract
          flags,     %% :16 - General purpose bit flag
          method,    %% :16 - Compression method
          time,      %% :16 - Last mod file time (MS-DOS)
          date,      %% :16 - Last mod file date (MS-DOS)
          crc,       %% :32 - CRC-32
          size,      %% :32 - Compressed size
          usize,     %% :32 - Uncompressed size
          fnamelen,  %% :16 - Filename length
          extralen,  %% :16 - Extra field length
          fname = "",%% filename
          extra = "" %% extra field
         }).

-define(ZIP_FILE_SIG, 16#02014B50).

-record(zip_file, {
          sig,        %% :32 - Central file header signature
          ver,        %% :16 - Version made by
          verx,       %% :16 - Version needed to extract
          flags,      %% :16 - General purpose bit flag
          method,     %% :16 - Compression method
          time,       %% :16 - Last mod file time (MS-DOS)
          date,       %% :16 - Last mod file date (MS-DOS)
          crc,        %% :32 - CRC-32
          size,       %% :32 - Compressed size
          usize,      %% :32 - Uncompressed size
          fnamelen,   %% :16 - Filename length
          extralen,   %% :16 - Extra field length
          commentlen, %% :16 - File comment length
          dsk,        %% :16 - Disk number start
          iattr,      %% :16 - Internal file attributes
	  attr,       %% :32 - External file attributes
	  offset,     %% :32 - Offset to local header
	  fname = "",
	  extra = "",
	  comment = ""
         }).

-define(ZIP_SIGNATURE_SIG, 16#05054b50).
-record(zip_signature, {
	  sig,         %% :32 - header signature
	  size,        %% :16 - signature size
	  data = ""    %% signature data
	  }).

-define(ZIP64_END_SIG, 16#06064b50).
-record(zip64_end, {
	  sig,         %% :32  - Header signature
	  rsize,       %% :64  - Size of this record 64 bits????(32?)
	  ver,         %% :16  - Version made by
	  verx,        %% :16  - Version needed to extract
	  dsk,         %% :32  - Number of this disk
	  cdsk,        %% :32  - Num of the disk of the central dir
	  nent,        %% :64  - Number of entries on this disk
	  tent,        %% :64  - Total number of entries
	  size,        %% :64  - Size of the central directory
	  offset,      %% :64  - Offset of the central directory
	  extension="" %% zip64 extension data sector
	  }).

-define(ZIP64_END_LOCATOR_SIG, 16#07064b50).
-record(zip64_end_locator, {
	  sig,         %% :32  - Header signature
	  dsk,         %% :32  - Number of disk with start of zip64 end
	  offset,      %% :64  - Relative offset to zip64_eocdir record
	  ndisks       %% :32  - Total number of disk
	 }).

-define(ZIP_END_SIG, 16#06054b50).

-record(zip_end, {
	  sig,         %% :32  - Header signature
	  dsk,         %% :16  - Number of this disk
	  cdsk,        %% :16  - Num of the disk of the central dir
	  nent,        %% :16  - Number of entries on this disk
	  tent,        %% :16  - Total number of entries
	  size,        %% :32  - Size of central directory
	  offset,      %% :32  - Offset of the central directory
	  zlen,        %% :16  - Zip file comment
	  zcomment ="" %% zip comment
	 }).

-endif.
