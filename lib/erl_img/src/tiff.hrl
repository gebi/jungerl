-ifndef(__IMAGE_TIFF_HRL__).
-define(__IMAGE_TIFF_HRL__, true).

-define(BYTE,      1). %% 8-bit unsigned integer
-define(ASCII,     2).
-define(SHORT,     3). %% 16-bit unsigned integer
-define(LONG,      4). %% 32-bit unsigned integer
-define(RATIONAL,  5). %% 32-bit numerator and 32-bit denominator
-define(SBYTE,     6).
-define(UNDEFINED, 7).
-define(SSHORT,    8).
-define(SLONG,     9).
-define(SRATIONAL, 10).
-define(FLOAT,     11).
-define(DOUBLE,    12).

-record(tiff_entry, {
	  ifd,      %% ifd number I.J.K (reversed)
	  tag,      %% tag name (atom) or number
	  type,     %% value type
	  endian,   %% little or big
	  value,    %% value
	  offs      %% offset to value start (strings,binaries...)
	 }).

%% TIFF tags
-define(NewSubfileType,254). %% long 
-define(SubfileType, 255).  %% short
-define(ImageWidth,  256).  %% short/long
-define(ImageLength, 257).  %% short/long
-define(BitsPerSample,258). %% short  N=SamplesPerPixel
-define(Compression, 259).  %% short  N=1 
                            %%  1     no-compression (packed)
                            %%  2     ccitt huffman run-length encofing
                            %%  3     Group 3 Fax
                            %%  4     Group 4 Fax
                            %%  5     LZW
                            %%  6     JPEG 
                            %%  32773 Packbits simple run-length
-define(PhotoMetricInterpretation, 262).        %% short
-define(Threshholding, 263).   %% short N=1
-define(CellWidth, 264).       %% short  N=1
-define(CellLength, 265).      %% short   N=1
-define(FillOrder, 266).       %% short N=1
-define(DocumentName, 269).    %% ASCII Document name
-define(ImageDescription,270). %% ASCII subject of image
-define(Make, 271).            %% Scanner Manufaturer
-define(Model, 272).           %% Scanner Model
-define(StripOffset,  273).    %% short or long
-define(Orientation, 274).     %% short
-define(SamplesPerPixel, 277). %% short
-define(RowsPerStrip, 278).    %% short or long
-define(StripByteCounts, 279). %% short ot long
-define(MinSampleValue,280).   %% short
-define(MaxSampleValue,281).   %% short
-define(XResolution, 282).    %% rational
-define(YResolution, 283).    %% rational
-define(PlanarConfiguration,284). %% short N=1
-define(PageName, 285).       %% ASCII page name
-define(XPosition, 286).      %% rational
-define(YPosition, 287).      %% rational
-define(FreeOffsets, 288).    %% long
-define(FreeByteCounts, 289). %% long
-define(GrayResponseUnit,290). %% short N=1
-define(GrayResponseCurve,291). %% short N=2^BitsPerSample
-define(T4Options, 292).        %% long N=1
-define(T6Options, 293).        %% long N=1
-define(ResolutionUnit, 296). %% short
-define(PageNumber, 297).     %% short N=2
-define(TransferFunction,301). %% short
-define(Software, 305).       %% ASCII software used 
-define(DateTime,  306).      %% ASCII YYYY:MM:DD:HH:MM:SS
-define(Artist, 315).         %% ASCII 
-define(HostComputer,316).    %% ASCII computer/os where image was created
-define(Predictor, 317).      %% short
-define(WhitePoint, 318).     %% rational
-define(PrimaryChromaticities,319). %% rational
-define(ColorMap, 320).       %% short N=3*(2^BitsPerSample)
-define(HalftoneHints,321).   %% short
-define(TileWidth, 322).      %% short/long
-define(TileLength, 323).     %% short/long
-define(TileOffset, 324).     %% long
-define(TileByteCounts, 325). %% short/long
-define(InkSet, 332).         %% short N=1
-define(InkNames, 333).       %% ASCII
-define(NumberOfInks,334).    %% short
-define(DotRange, 336).       %% byte or short
-define(TargetPrinter,337).   %% ASCII
-define(ExtraSamples,338).    %% Short N=m
-define(SampleFormat, 339).   %% short
-define(SMinSampleValue,340). %% Any
-define(SMaxSampleValue,341). %% Any
-define(TransferRange, 342).  %% short N=6
-define(JPEGProc, 512).       %% short N=1
-define(JPEGInterchangeFormat, 513).
-define(JPEGInterchangeFormatLength, 514).
-define(JPEGRestartInterval, 515).
-define(JPEGLosslessPredictors, 517).
-define(JPEGPointTransforms, 518).
-define(JPEGQTables, 519).
-define(JPEGDCTables, 520).
-define(JPEGACTables, 5201.
-define(YCbCrCoefficients, 529).
-define(YCbCrSampling, 530).
-define(YCbCrPositioning, 531).
-define(ReferenceBlackWhite, 532).
-define(Copyright, 33432).    %% ASCII

-endif.


