-ifndef(__IMAGE_EXIF_HRL__).
-define(__IMAGE_EXIF_HRL__, true).

%% IFD0 usage
-define(ExifOffset, 16#8769).  %%  unsigned long1 Offset to Exif Sub IFD

%% SubExif IFD usage
-define(ExposureTime,        16#829a).
-define(FNumber,             16#829d).
-define(ExposureProgram,     16#8822).
-define(ISOSpeedRatings,     16#8827).
-define(CFARepeatPatternDim, 16#828d ).

-define(ExifVersion,             16#9000).
-define(DateTimeOriginal,        16#9003).
-define(DateTimeDigitized,       16#9004).
-define(ComponentsConfiguration, 16#9101).
-define(CompressedBitsPerPixel,  16#9102).
-define(ShutterSpeedValue,       16#9201).
-define(ApertureValue,           16#9202).
-define(BrightnessValue,         16#9203).
-define(ExposureBiasValue,       16#9204).
-define(MaxApertureValue,        16#9205).
-define(SubjectDistance,         16#9206).
-define(MeteringMode,            16#9207).
-define(LightSource,             16#9208).
-define(Flash,                   16#9209).
-define(FocalLength,             16#920a).
-define(MakerNote,               16#927c).
-define(UserComment,             16#9286).
-define(SubsecTime,              16#9290).
-define(SubsecTimeOriginal,      16#9291).
-define(SubsecTimeDigitized,     16#9292).
-define(FlashPixVersion,         16#a000).
-define(ColorSpace,              16#a001).
-define(ExifImageWidth,          16#a002).
-define(ExifImageHeight,         16#a003).
-define(RelatedSoundFile,        16#a004).
-define(ExifInteroperabilityOffset, 16#a005 ).
-define(FocalPlaneXResolution,      16#a20e).
-define(FocalPlaneYResolution,      16#a20f).
-define(FocalPlaneResolutionUnit,   16#a210).
-define(ExposureIndex,              16#a215).
-define(SensingMethod,              16#a217).
-define(FileSource,                 16#a300).
-define(SceneType,                  16#a301).
-define(CFAPattern,                 16#a302).

%% Interoperability IFD

-define(InteroperabilityIndex, 16#0001 ).
-define(InteroperabilityVersion, 16#0002 ).
-define(RelatedImageFileFormat, 16#1000 ).
-define(RelatedImageWidth, 16#1001 ).
-define(RelatedImageLength, 16#1001 ).

%% Other tags

%% FIXME add OLYMP


-endif.


