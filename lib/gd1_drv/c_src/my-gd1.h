
/* function prototypes */

int my_gdImagePng(gdImagePtr, char *);
int my_gdImageJpeg(gdImagePtr, char *, int);
gdImagePtr my_gdImageCreateFromJpeg(char *, int *);
gdImagePtr my_gdImageCreateFromPng(char *, int *);
gdImagePtr my_gdImageCreateFromGd(char *, int *);
gdImagePtr my_gdImageCreateFromGd2(char *, int *);
gdImagePtr my_gdImageCreateFromGd2Part(char *, int, int, int, int, int *);
gdImagePtr my_gdImageCreateFromXpm(char *, int *);
int my_gdImageWBMP(gdImagePtr, int, char *);
int my_gdImageGd(gdImagePtr, char *);
int my_gdImageGd2(gdImagePtr, char *, int, int);
int my_gdImagePolygon(gdImagePtr, char *, int, int);
int my_gdImageFilledPolygon(gdImagePtr, char *, int, int);
int my_gdImageSetStyle(gdImagePtr, char *, int);
gdFontPtr my_GetFontPtr(int);
