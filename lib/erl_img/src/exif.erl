%%% File    : img_exif.erl
%%% Author  : Tony Rogvall <tony@a55.hemma.se>
%%% Description : Utils for decoding Exif tags
%%% Created :  6 Mar 2003 by Tony Rogvall <tony@a55.hemma.se>

-module(exif).

-export([decode_tag/1]).

-include("exif.hrl").

decode_tag(Tag) when integer(Tag) ->
    case Tag of
	?ExposureTime -> 'ExposureTime';
	?FNumber -> 'FNumber';
	?ExposureProgram -> 'ExposureProgram';
	?ISOSpeedRatings -> 'ISOSpeedRatings';
	?ExifVersion -> 'ExifVersion';
	?DateTimeOriginal -> 'DateTimeOriginal';
	?DateTimeDigitized -> 'DateTimeDigitized';
	?ComponentsConfiguration -> 'ComponentsConfiguration';
	?CompressedBitsPerPixel -> 'CompressedBitsPerPixel';
	?ShutterSpeedValue -> 'ShutterSpeedValue';
	?ApertureValue -> 'ApertureValue';
	?BrightnessValue -> 'BrightnessValue';
	?ExposureBiasValue -> 'ExposureBiasValue';
	?MaxApertureValue -> 'MaxApertureValue';
	?SubjectDistance -> 'SubjectDistance';
	?MeteringMode -> 'MeteringMode';
	?LightSource -> 'LightSource';
	?Flash -> 'Flash';
	?FocalLength -> 'FocalLength';
	?MakerNote -> 'MakerNote';
	?UserComment -> 'UserComment';
	?SubsecTime -> 'SubsecTime';
	?SubsecTimeOriginal -> 'SubsecTimeOriginal';
	?SubsecTimeDigitized -> 'SubsecTimeDigitized';
	?FlashPixVersion -> 'FlashPixVersion';
	?ColorSpace -> 'ColorSpace';
	?ExifImageWidth -> 'ExifImageWidth';
	?ExifImageHeight -> 'ExifImageHeight';
	?RelatedSoundFile -> 'RelatedSoundFile';
	?ExifInteroperabilityOffset -> 'ExifInteroperabilityOffset';
	?FocalPlaneXResolution -> 'FocalPlaneXResolution';
	?FocalPlaneYResolution -> 'FocalPlaneYResolution';
	?FocalPlaneResolutionUnit -> 'FocalPlaneResolutionUnit';
	?ExposureIndex -> 'ExposureIndex';
	?SensingMethod -> 'SensingMethod';
	?FileSource -> 'FileSource';
	?SceneType -> 'SceneType';
	?CFAPattern -> 'CFAPattern';
	?InteroperabilityIndex -> 'InteroperabilityIndex';
	?InteroperabilityVersion -> 'InteroperabilityVersion';
	?RelatedImageFileFormat -> 'RelatedImageFileFormat';
	?RelatedImageWidth -> 'RelatedImageWidth';
	?RelatedImageLength -> 'RelatedImageLength';
	Tag -> Tag
    end;
decode_tag(Tag) ->
    Tag.

    
	    


