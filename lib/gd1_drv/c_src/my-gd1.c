
#include	<stdio.h>
#include	<errno.h>

#include	<erl_driver_tk.h>
#include	<gd.h>
#include	<gdfontt.h>
#include	<gdfonts.h>
#include	<gdfontmb.h>
#include	<gdfontl.h>
#include	<gdfontg.h>
#include	<my-gd1.h>
#include	<gd1_drv.h>

gdImagePtr gdImageCreateFromXpm(FILE *); /* gd.h doesn't have a prototype */

int
my_gdImagePng(gdImagePtr im, char *file)
{
    FILE	*fp;

    if ((fp = fopen(file, "wb")) == NULL)
	return errno;
    gdImagePng(im, fp);
    if (fclose(fp) == EOF)
	return errno;
    else
	return 0;
}

int
my_gdImageJpeg(gdImagePtr im, char *file, int quality)
{
    FILE	*fp;

    if ((fp = fopen(file, "wb")) == NULL)
	return errno;
    gdImageJpeg(im, fp, quality);
    if (fclose(fp) == EOF)
	return errno;
    else
	return 0;
}

gdImagePtr
my_gdImageCreateFromJpeg(char *file, int *status)
{
    FILE	*fp;
    gdImagePtr	im;

    if ((fp = fopen(file, "rb")) == NULL) {
	*status = errno;
	return NULL;
    }
    if ((im = gdImageCreateFromJpeg(fp)) == NULL) {
	*status = -1;
	return NULL;
    }
    if (fclose(fp) == EOF) {
	*status = errno;
	return NULL;
    } else {
	return im;
    }
}

gdImagePtr
my_gdImageCreateFromPng(char *file, int *status)
{
    FILE	*fp;
    gdImagePtr	im;

    if ((fp = fopen(file, "rb")) == NULL) {
	*status = errno;
	return NULL;
    }
    if ((im = gdImageCreateFromPng(fp)) == NULL) {
	*status = -1;
	return NULL;
    }
    if (fclose(fp) == EOF) {
	*status = errno;
	return NULL;
    } else {
	return im;
    }
}

gdImagePtr
my_gdImageCreateFromGd(char *file, int *status)
{
    FILE	*fp;
    gdImagePtr	im;

    if ((fp = fopen(file, "rb")) == NULL) {
	*status = errno;
	return NULL;
    }
    if ((im = gdImageCreateFromGd(fp)) == NULL) {
	*status = -1;
	return NULL;
    }
    if (fclose(fp) == EOF) {
	*status = errno;
	return NULL;
    } else {
	return im;
    }
}

gdImagePtr
my_gdImageCreateFromGd2(char *file, int *status)
{
    FILE	*fp;
    gdImagePtr	im;

    if ((fp = fopen(file, "rb")) == NULL) {
	*status = errno;
	return NULL;
    }
    if ((im = gdImageCreateFromGd2(fp)) == NULL) {
	*status = -1;
	return NULL;
    }
    if (fclose(fp) == EOF) {
	*status = errno;
	return NULL;
    } else {
	return im;
    }
}

gdImagePtr
my_gdImageCreateFromGd2Part(char *file, int srcX, int srcY, int w, int h,
			    int *status)
{
    FILE	*fp;
    gdImagePtr	im;

    if ((fp = fopen(file, "rb")) == NULL) {
	*status = errno;
	return NULL;
    }
    if ((im = gdImageCreateFromGd2Part(fp, srcX, srcY, w, h)) == NULL) {
	*status = -1;
	return NULL;
    }
    if (fclose(fp) == EOF) {
	*status = errno;
	return NULL;
    } else {
	return im;
    }
}

gdImagePtr
my_gdImageCreateFromXpm(char *file, int *status)
{
    FILE	*fp;
    gdImagePtr	im;

    if ((fp = fopen(file, "rb")) == NULL) {
	*status = errno;
	return NULL;
    }
    if ((im = gdImageCreateFromXpm(fp)) == NULL) {
	*status = -1;
	return NULL;
    }
    if (fclose(fp) == EOF) {
	*status = errno;
	return NULL;
    } else {
	return im;
    }
}

int
my_gdImageWBMP(gdImagePtr im, int fg, char *file)
{
    FILE	*fp;

    if ((fp = fopen(file, "wb")) == NULL)
	return errno;
    gdImageWBMP(im, fg, fp);
    if (fclose(fp) == EOF)
	return errno;
    else
	return 0;
}

int
my_gdImageGd(gdImagePtr im, char *file)
{
    FILE	*fp;

    if ((fp = fopen(file, "wb")) == NULL)
	return errno;
    gdImageGd(im, fp);
    if (fclose(fp) == EOF)
	return errno;
    else
	return 0;
}

int
my_gdImageGd2(gdImagePtr im, char *file, int chunkSize, int fmt)
{
    FILE	*fp;

    if ((fp = fopen(file, "wb")) == NULL)
	return errno;
    gdImageGd2(im, fp, chunkSize, fmt);
    if (fclose(fp) == EOF)
	return errno;
    else
	return 0;
}

int
my_gdImagePolygon(gdImagePtr im, char *vertices, int pointsTotal, int color)
{
    gdPoint	v[MAX_POLYGON_VERTICES];
    int		i;

    if (pointsTotal > MAX_POLYGON_VERTICES) {
	return -1;
    }
    for (i = 0; i < pointsTotal; i++) {
	v[i].x = get_int32(vertices);
	vertices += 4;
	v[i].y = get_int32(vertices);
	vertices += 4;
    }
    gdImagePolygon(im, v, pointsTotal, color);
    return 0;
}

int
my_gdImageFilledPolygon(gdImagePtr im, char *vertices, int pointsTotal, int color)
{
    gdPoint	v[MAX_POLYGON_VERTICES];
    int		i;

    if (pointsTotal > MAX_POLYGON_VERTICES) {
	return -1;
    }
    for (i = 0; i < pointsTotal; i++) {
	v[i].x = get_int32(vertices);
	vertices += 4;
	v[i].y = get_int32(vertices);
	vertices += 4;
    }
    gdImageFilledPolygon(im, v, pointsTotal, color);
    return 0;
}

int
my_gdImageSetStyle(gdImagePtr im, char *style, int styleLength)
{
    int		v[MAX_STYLE_LENGTH];
    int		i;

    if (styleLength > MAX_STYLE_LENGTH) {
	return -1;
    }
    for (i = 0; i < styleLength; i++) {
	v[i] = get_int32(style);
	style += 4;
    }
    gdImageSetStyle(im, v, styleLength);
    return 0;
}

gdFontPtr
my_GetFontPtr(int font_idx)
{
    switch (font_idx) {
    case FONT_TINY: return gdFontTiny;
    case FONT_SMALL: return gdFontSmall;
    case FONT_MEDIUMBOLD: return gdFontMediumBold;
    case FONT_LARGE: return gdFontLarge;
    case FONT_GIANT: return gdFontGiant;
    default: return NULL;
    }
    /* NOTREACHED */
}
