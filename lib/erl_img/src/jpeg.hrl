-ifndef(__IMAGE_JPG_HRL__).
-define(__IMAGE_JPG_HRL__, true).

-define(M_SOF0, 16#FFC0).      %% Start Of Frame N
-define(M_SOF1, 16#FFC1).      %% N indicates which compression process
-define(M_SOF2, 16#FFC2).      %% Only SOF0-SOF2 are now in common use
-define(M_SOF3, 16#FFC3).
-define(M_SOF5, 16#FFC5).      %% NB: codes C4 and CC are NOT SOF markers
-define(M_SOF6, 16#FFC6).
-define(M_SOF7, 16#FFC7).
-define(M_SOF9, 16#FFC9).
-define(M_SOF10,16#FFCA).
-define(M_SOF11,16#FFCB).
-define(M_SOF13,16#FFCD).
-define(M_SOF14,16#FFCE).
-define(M_SOF15,16#FFCF).
-define(M_SOI,  16#FFD8).       %% Start Of Image (beginning of datastream)
-define(M_EOI,  16#FFD9).       %% End Of Image (end of datastream)
-define(M_SOS,  16#FFDA).       %% Start Of Scan (begins compressed data)
-define(M_JFIF, 16#FFE0).       %% Jfif marker
-define(M_APP1, 16#FFE1).       %% Exif marker
-define(M_COM,  16#FFFE).       %% COMment 

-endif.


