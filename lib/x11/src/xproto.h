
typedef signed int INT32;
typedef signed short INT16;
typedef signed char INT8;
typedef unsigned long  CARD32;
typedef unsigned short CARD16;
typedef unsigned char  CARD8;

typedef CARD32	BITS32;
typedef CARD16	BITS16;
typedef CARD8	BYTE;
typedef CARD8   BOOL;

typedef struct _xSegment {
    INT16 x1, y1, x2, y2;
} xSegment;

/* POINT */

typedef struct _xPoint {
	INT16		x, y;
} xPoint;

typedef struct _xRectangle {
    INT16 x, y;
    CARD16  width, height;
} xRectangle;

/*  ARC  */

typedef struct _xArc {
    INT16 x, y;
    CARD16   width, height;
    INT16   angle1, angle2;
} xArc;

/*
 * Define constants for the sizes of the network packets.  The sz_ prefix is
 * used instead of something more descriptive so that the symbols are no more
 * than 32 characters in length (which causes problems for some compilers).
 */
#define sz_xSegment 8
#define sz_xPoint 4
#define sz_xRectangle 8
#define sz_xArc 12
#define sz_xConnClientPrefix 12
#define sz_xConnSetupPrefix 8
#define sz_xConnSetup 32
#define sz_xPixmapFormat 8
#define sz_xDepth 8
#define sz_xVisualType 24
#define sz_xWindowRoot 40
#define sz_xTimecoord 8
#define sz_xHostEntry 4
#define sz_xCharInfo 12
#define sz_xFontProp 8
#define sz_xTextElt 2
#define sz_xColorItem 12
#define sz_xrgb 8
#define sz_xGenericReply 32
#define sz_xGetWindowAttributesReply 44
#define sz_xGetGeometryReply 32
#define sz_xQueryTreeReply 32
#define sz_xInternAtomReply 32
#define sz_xGetAtomNameReply 32
#define sz_xGetPropertyReply 32
#define sz_xListPropertiesReply 32
#define sz_xGetSelectionOwnerReply 32
#define sz_xGrabPointerReply 32
#define sz_xQueryPointerReply 32
#define sz_xGetMotionEventsReply 32
#define sz_xTranslateCoordsReply 32
#define sz_xGetInputFocusReply 32
#define sz_xQueryKeymapReply 40
#define sz_xQueryFontReply 60
#define sz_xQueryTextExtentsReply 32
#define sz_xListFontsReply 32
#define sz_xGetFontPathReply 32
#define sz_xGetImageReply 32
#define sz_xListInstalledColormapsReply 32
#define sz_xAllocColorReply 32
#define sz_xAllocNamedColorReply 32
#define sz_xAllocColorCellsReply 32
#define sz_xAllocColorPlanesReply 32
#define sz_xQueryColorsReply 32
#define sz_xLookupColorReply 32
#define sz_xQueryBestSizeReply 32
#define sz_xQueryExtensionReply 32
#define sz_xListExtensionsReply 32
#define sz_xSetMappingReply 32
#define sz_xGetKeyboardControlReply 52
#define sz_xGetPointerControlReply 32
#define sz_xGetScreenSaverReply 32
#define sz_xListHostsReply 32
#define sz_xSetModifierMappingReply 32
#define sz_xError 32
#define sz_xEvent 32
#define sz_xKeymapEvent 32
#define sz_xReq 4
#define sz_xResourceReq 8
#define sz_xCreateWindowReq 32
#define sz_xChangeWindowAttributesReq 12
#define sz_xChangeSaveSetReq 8
#define sz_xReparentWindowReq 16
#define sz_xConfigureWindowReq 12
#define sz_xCirculateWindowReq 8
#define sz_xInternAtomReq 8
#define sz_xChangePropertyReq 24
#define sz_xDeletePropertyReq 12
#define sz_xGetPropertyReq 24
#define sz_xSetSelectionOwnerReq 16
#define sz_xConvertSelectionReq 24
#define sz_xSendEventReq 44
#define sz_xGrabPointerReq 24
#define sz_xGrabButtonReq 24
#define sz_xUngrabButtonReq 12
#define sz_xChangeActivePointerGrabReq 16
#define sz_xGrabKeyboardReq 16
#define sz_xGrabKeyReq 16
#define sz_xUngrabKeyReq 12
#define sz_xAllowEventsReq 8
#define sz_xGetMotionEventsReq 16
#define sz_xTranslateCoordsReq 16
#define sz_xWarpPointerReq 24
#define sz_xSetInputFocusReq 12
#define sz_xOpenFontReq 12
#define sz_xQueryTextExtentsReq 8
#define sz_xListFontsReq 8
#define sz_xSetFontPathReq 8
#define sz_xCreatePixmapReq 16
#define sz_xCreateGCReq 16
#define sz_xChangeGCReq 12
#define sz_xCopyGCReq 16
#define sz_xSetDashesReq 12
#define sz_xSetClipRectanglesReq 12
#define sz_xCopyAreaReq 28
#define sz_xCopyPlaneReq 32
#define sz_xPolyPointReq 12
#define sz_xPolySegmentReq 12
#define sz_xFillPolyReq 16
#define sz_xPutImageReq 24
#define sz_xGetImageReq 20
#define sz_xPolyTextReq 16
#define sz_xImageTextReq 16
#define sz_xCreateColormapReq 16
#define sz_xCopyColormapAndFreeReq 12
#define sz_xAllocColorReq 16
#define sz_xAllocNamedColorReq 12
#define sz_xAllocColorCellsReq 12
#define sz_xAllocColorPlanesReq 16
#define sz_xFreeColorsReq 12
#define sz_xStoreColorsReq 8
#define sz_xStoreNamedColorReq 16
#define sz_xQueryColorsReq 8
#define sz_xLookupColorReq 12
#define sz_xCreateCursorReq 32
#define sz_xCreateGlyphCursorReq 32
#define sz_xRecolorCursorReq 20
#define sz_xQueryBestSizeReq 12
#define sz_xQueryExtensionReq 8
#define sz_xChangeKeyboardControlReq 8
#define sz_xBellReq 4
#define sz_xChangePointerControlReq 12
#define sz_xSetScreenSaverReq 12
#define sz_xChangeHostsReq 8
#define sz_xListHostsReq 4
#define sz_xChangeModeReq 4
#define sz_xRotatePropertiesReq 12
#define sz_xReply 32
#define sz_xGrabKeyboardReply 32
#define sz_xListFontsWithInfoReply 60
#define sz_xSetPointerMappingReply 32
#define sz_xGetKeyboardMappingReply 32
#define sz_xGetPointerMappingReply 32
#define sz_xGetModifierMappingReply 32
#define sz_xListFontsWithInfoReq 8
#define sz_xPolyLineReq 12
#define sz_xPolyArcReq 12
#define sz_xPolyRectangleReq 12
#define sz_xPolyFillRectangleReq 12
#define sz_xPolyFillArcReq 12
#define sz_xPolyText8Req 16
#define sz_xPolyText16Req 16
#define sz_xImageText8Req 16
#define sz_xImageText16Req 16
#define sz_xSetPointerMappingReq 4
#define sz_xForceScreenSaverReq 4
#define sz_xSetCloseDownModeReq 4
#define sz_xClearAreaReq 16
#define sz_xSetAccessControlReq 4
#define sz_xGetKeyboardMappingReq 8
#define sz_xSetModifierMappingReq 4
#define sz_xPropIconSize 24
#define sz_xChangeKeyboardMappingReq 8

typedef CARD32 WINDOW;
typedef CARD32 DRAWABLE;
typedef CARD32 FONT;
typedef CARD32 PIXMAP;
typedef CARD32 CURSOR;
typedef CARD32 COLORMAP;
typedef CARD32 GCONTEXT;
typedef CARD32 ATOM;
typedef CARD32 VISUALID;
typedef CARD32 TIME;
typedef CARD8 KEYCODE;
typedef CARD32 KEYSYM;

#define X_TCP_PORT 6000     /* add display number */

#define xTrue        1
#define xFalse       0


typedef CARD16 KEYBUTMASK;

/***************** 
   connection setup structure.  This is followed by
   numRoots xWindowRoot structs.
*****************/

typedef struct {
    CARD8	byteOrder;
    BYTE	pad;
    CARD16	majorVersion, minorVersion;
    CARD16	nbytesAuthProto;	/* Authorization protocol */
    CARD16	nbytesAuthString;	/* Authorization string */
    CARD16	pad2;
} xConnClientPrefix;

typedef struct {
    CARD8          success;
    BYTE           lengthReason; /*num bytes in string following if failure */
    CARD16         majorVersion, 
                   minorVersion;
    CARD16         length;  /* 1/4 additional bytes in setup info */
} xConnSetupPrefix;


typedef struct {
    CARD32         release;
    CARD32         ridBase, 
                   ridMask;
    CARD32         motionBufferSize;
    CARD16         nbytesVendor;  /* number of bytes in vendor string */
    CARD16         maxRequestSize;
    CARD8          numRoots;          /* number of roots structs to follow */
    CARD8          numFormats;        /* number of pixmap formats */
    CARD8          imageByteOrder;        /* LSBFirst, MSBFirst */
    CARD8          bitmapBitOrder;        /* LeastSignificant, MostSign...*/
    CARD8          bitmapScanlineUnit,     /* 8, 16, 32 */
                   bitmapScanlinePad;     /* 8, 16, 32 */
    KEYCODE	   minKeyCode, maxKeyCode;
    CARD32	   pad2;
} xConnSetup;

typedef struct {
    CARD8          depth;
    CARD8          bitsPerPixel;
    CARD8          scanLinePad;
    CARD8          pad1;
    CARD32	   pad2;
} xPixmapFormat;

/* window root */

typedef struct {
    CARD8 	depth;
    CARD8 	pad1;
    CARD16	nVisuals;  /* number of xVisualType structures following */
    CARD32	pad2;
    } xDepth;

typedef struct {
    VISUALID visualID;
    CARD8 class;
    CARD8 bitsPerRGB;
    CARD16 colormapEntries;
    CARD32 redMask, greenMask, blueMask;
    CARD32 pad;
    } xVisualType;

typedef struct {
    WINDOW         windowId;
    COLORMAP       defaultColormap;
    CARD32         whitePixel, blackPixel;
    CARD32         currentInputMask;   
    CARD16         pixWidth, pixHeight;
    CARD16         mmWidth, mmHeight;
    CARD16         minInstalledMaps, maxInstalledMaps;
    VISUALID       rootVisualID;
    CARD8          backingStore;
    BOOL           saveUnders;
    CARD8          rootDepth;
    CARD8          nDepths;  /* number of xDepth structures following */
} xWindowRoot;

/*****************************************************************
 * Structure Defns
 *   Structures needed for replies 
 *****************************************************************/

/* Used in GetMotionEvents */

typedef struct {
    CARD32 time;
    INT16 x, y;
} xTimecoord;

typedef struct {
    CARD8 family;
    BYTE pad;
    CARD16 length;
} xHostEntry;

typedef struct {
    INT16 leftSideBearing,
	  rightSideBearing,
	  characterWidth,
	  ascent,
	  descent;
    CARD16 attributes;
} xCharInfo;

typedef struct {
    ATOM name;
    CARD32 value;
} xFontProp;

/*
 * non-aligned big-endian font ID follows this struct
 */
typedef struct {           /* followed by string */
    CARD8 len;	/* number of *characters* in string, or FontChange (255)
		   for font change, or 0 if just delta given */
    INT8 delta;
} xTextElt;


typedef struct {        
    CARD32 pixel;
    CARD16 red, green, blue;
    CARD8 flags;  /* DoRed, DoGreen, DoBlue booleans */
    CARD8 pad;
} xColorItem;


typedef struct {
    CARD16 red, green, blue, pad;
} xrgb;

/* typedef CARD8 KEYCODE; */

/*****************
 * XRep:
 *    meant to be 32 byte quantity 
 *****************/

/* GenericReply is the common format of all replies.  The "data" items
   are specific to each individual reply type. */

typedef struct {	
    BYTE type;              /* X_Reply */
    BYTE data1;             /* depends on reply type */
    CARD16 sequenceNumber;  /* of last request received by server */
    CARD32 length;      /* 4 byte quantities beyond size of GenericReply */
    CARD32 data00;
    CARD32 data01;
    CARD32 data02;
    CARD32 data03;
    CARD32 data04;
    CARD32 data05;
} xGenericReply;

/* Individual reply formats. */

typedef struct {
    BYTE type;  /* X_Reply */
    CARD8 backingStore;
    CARD16 sequenceNumber;
    CARD32 length;	/* NOT 0; this is an extra-large reply */
    VISUALID visualID;
    CARD16 class;
    CARD8 bitGravity;
    CARD8 winGravity;
    CARD32 backingBitPlanes;
    CARD32 backingPixel;
    BOOL saveUnder;
    BOOL mapInstalled;
    CARD8 mapState;
    BOOL override;
    COLORMAP colormap;
    CARD32 allEventMasks;
    CARD32 yourEventMask;
    CARD16 doNotPropagateMask;
    CARD16 pad;
} xGetWindowAttributesReply;

typedef struct {
    BYTE type;   /* X_Reply */
    CARD8 depth;
    CARD16 sequenceNumber;
    CARD32 length;  /* 0 */
    WINDOW root;
    INT16 x, y;
    CARD16 width, height;
    CARD16 borderWidth;
    CARD16 pad1;
    CARD32 pad2;
    CARD32 pad3;
} xGetGeometryReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;
    WINDOW root, parent;
    CARD16 nChildren;
    CARD16 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
} xQueryTreeReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length; /* 0 */
    ATOM atom;
    CARD32 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
} xInternAtomReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;  /* of additional bytes */
    CARD16 nameLength;  /* # of characters in name */
    CARD16 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xGetAtomNameReply;

typedef struct {
    BYTE type;  /* X_Reply */
    CARD8 format;
    CARD16 sequenceNumber;
    CARD32 length; /* of additional bytes */
    ATOM propertyType;
    CARD32 bytesAfter;
    CARD32 nItems; /* # of 8, 16, or 32-bit entities in reply */
    CARD32 pad1;
    CARD32 pad2;
    CARD32 pad3;
} xGetPropertyReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;
    CARD16 nProperties;
    CARD16 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xListPropertiesReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;  /* 0 */
    WINDOW owner;
    CARD32 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
} xGetSelectionOwnerReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE status;
    CARD16 sequenceNumber;
    CARD32 length;  /* 0 */
    CARD32 pad1;
    CARD32 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
} xGrabPointerReply;

typedef xGrabPointerReply xGrabKeyboardReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BOOL sameScreen;
    CARD16 sequenceNumber;
    CARD32 length;  /* 0 */
    WINDOW root, child;
    INT16 rootX, rootY, winX, winY;
    CARD16 mask;
    CARD16 pad1;
    CARD32 pad;
} xQueryPointerReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;
    CARD32 nEvents;
    CARD32 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
} xGetMotionEventsReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BOOL sameScreen;
    CARD16 sequenceNumber;
    CARD32 length; /* 0 */
    WINDOW child;
    INT16 dstX, dstY;
    CARD32 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
} xTranslateCoordsReply;

typedef struct {
    BYTE type;  /* X_Reply */
    CARD8 revertTo;
    CARD16 sequenceNumber;
    CARD32 length;  /* 0 */
    WINDOW focus;
    CARD32 pad1;
    CARD32 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
} xGetInputFocusReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;  /* 2, NOT 0; this is an extra-large reply */
    BYTE map[32];
} xQueryKeymapReply;

/* Warning: this MUST match (up to component renaming) xListFontsWithInfoReply */
typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;  /* definitely > 0, even if "nCharInfos" is 0 */
    xCharInfo minBounds; 
    CARD32 walign1;
    xCharInfo maxBounds; 
    CARD32 walign2;
    CARD16 minCharOrByte2, maxCharOrByte2;
    CARD16 defaultChar;
    CARD16 nFontProps;  /* followed by this many xFontProp structures */
    CARD8 drawDirection;
    CARD8 minByte1, maxByte1;
    BOOL allCharsExist;
    INT16 fontAscent, fontDescent;
    CARD32 nCharInfos; /* followed by this many xCharInfo structures */
} xQueryFontReply;

typedef struct {
    BYTE type;  /* X_Reply */
    CARD8 drawDirection;
    CARD16 sequenceNumber;
    CARD32 length;  /* 0 */
    INT16 fontAscent, fontDescent;
    INT16 overallAscent, overallDescent;
    INT32 overallWidth, overallLeft, overallRight;
    CARD32 pad;
} xQueryTextExtentsReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;
    CARD16 nFonts;
    CARD16 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xListFontsReply;

/* Warning: this MUST match (up to component renaming) xQueryFontReply */
typedef struct {
    BYTE type;  /* X_Reply */
    CARD8 nameLength;  /* 0 indicates end-of-reply-sequence */
    CARD16 sequenceNumber;
    CARD32 length;  /* definitely > 0, even if "nameLength" is 0 */
    xCharInfo minBounds; 
    CARD32 walign1;
    xCharInfo maxBounds; 
    CARD32 walign2;
    CARD16 minCharOrByte2, maxCharOrByte2;
    CARD16 defaultChar;
    CARD16 nFontProps;  /* followed by this many xFontProp structures */
    CARD8 drawDirection;
    CARD8 minByte1, maxByte1;
    BOOL allCharsExist;
    INT16 fontAscent, fontDescent;
    CARD32 nReplies;   /* hint as to how many more replies might be coming */
} xListFontsWithInfoReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;
    CARD16 nPaths;
    CARD16 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xGetFontPathReply;

typedef struct {
    BYTE type;  /* X_Reply */
    CARD8 depth;
    CARD16 sequenceNumber;
    CARD32 length;
    VISUALID visual;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xGetImageReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;
    CARD16 nColormaps;
    CARD16 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xListInstalledColormapsReply;

typedef struct {
    BYTE type; /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;   /* 0 */
    CARD16 red, green, blue;
    CARD16 pad2;
    CARD32 pixel;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
} xAllocColorReply;

typedef struct {
    BYTE type; /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;  /* 0 */
    CARD32 pixel;
    CARD16 exactRed, exactGreen, exactBlue;
    CARD16 screenRed, screenGreen, screenBlue;
    CARD32 pad2;
    CARD32 pad3;
} xAllocNamedColorReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;
    CARD16 nPixels, nMasks;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xAllocColorCellsReply;

typedef struct {
    BYTE type; /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;
    CARD16 nPixels;
    CARD16 pad2;
    CARD32 redMask, greenMask, blueMask;
    CARD32 pad3;
    CARD32 pad4;
} xAllocColorPlanesReply;

typedef struct {
    BYTE type; /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;
    CARD16 nColors;
    CARD16 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xQueryColorsReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;  /* 0 */
    CARD16 exactRed, exactGreen, exactBlue;
    CARD16 screenRed, screenGreen, screenBlue;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
} xLookupColorReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;  /* 0 */
    CARD16 width, height;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xQueryBestSizeReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length; /* 0 */
    BOOL  present;
    CARD8 major_opcode;
    CARD8 first_event;
    CARD8 first_error;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xQueryExtensionReply;

typedef struct {
    BYTE type;  /* X_Reply */
    CARD8 nExtensions;
    CARD16 sequenceNumber;
    CARD32 length;
    CARD32 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xListExtensionsReply;


typedef struct {
    BYTE   type;  /* X_Reply */
    CARD8  success;
    CARD16 sequenceNumber;
    CARD32 length;
    CARD32 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xSetMappingReply;

typedef xSetMappingReply xSetPointerMappingReply;
typedef xSetMappingReply xSetModifierMappingReply;

typedef struct {
    BYTE type;  /* X_Reply */
    CARD8 nElts;  /* how many elements does the map have */
    CARD16 sequenceNumber;
    CARD32 length;
    CARD32 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xGetPointerMappingReply;

typedef struct {
    BYTE type;
    CARD8 keySymsPerKeyCode;
    CARD16 sequenceNumber;
    CARD32 length;
    CARD32 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xGetKeyboardMappingReply;    

typedef struct {
    BYTE type;
    CARD8 numKeyPerModifier;
    CARD16 sequenceNumber;
    CARD32 length;
    CARD32 pad1;
    CARD32 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
} xGetModifierMappingReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BOOL globalAutoRepeat;
    CARD16 sequenceNumber;
    CARD32 length;  /* 5 */
    CARD32 ledMask;
    CARD8 keyClickPercent, bellPercent;
    CARD16 bellPitch, bellDuration;
    CARD16 pad;
    BYTE map[32];  /* bit masks start here */
} xGetKeyboardControlReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;  /* 0 */
    CARD16 accelNumerator, accelDenominator;
    CARD16 threshold;
    CARD16 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
} xGetPointerControlReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length;  /* 0 */
    CARD16 timeout, interval;
    BOOL preferBlanking;
    BOOL allowExposures;
    CARD16 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
} xGetScreenSaverReply;

typedef struct {
    BYTE type;  /* X_Reply */
    BOOL enabled;
    CARD16 sequenceNumber;
    CARD32 length;
    CARD16 nHosts;
    CARD16 pad1;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xListHostsReply;

/*****************************************************************
 * Xerror
 *    All errors  are 32 bytes 
 *****************************************************************/

typedef struct {
    BYTE type;                  /* X_Error */
    BYTE errorCode;
    CARD16 sequenceNumber;       /* the nth request from this client */
    CARD32 resourceID;
    CARD16 minorCode;
    CARD8 majorCode;
    BYTE pad1;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
    CARD32 pad7;
} xError;

/*****************************************************************
 * xEvent
 *    All events are 32 bytes
 *****************************************************************/
#define ELFlagFocus        (1<<0)
#define ELFlagSameScreen   (1<<1)

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    BYTE bytes[28];
} xEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    TIME time;
    WINDOW root, event, child;
    INT16 rootX, rootY, eventX, eventY;
    KEYBUTMASK state;
    BOOL sameScreen;
    BYTE pad1;
} xKeyEvent;

typedef xKeyEvent xKeyPressedEvent;
typedef xKeyEvent xKeyReleasedEvent;
typedef xKeyEvent xButtonPressedEvent;
typedef xKeyEvent xButtonReleasedEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    TIME time;
    WINDOW root, event, child;
    INT16 rootX, rootY, eventX, eventY;
    KEYBUTMASK state;
    BYTE mode; 			/* really XMode */
    BYTE flags;		/* sameScreen and focus booleans, packed together */
} xCrossingEvent;

typedef xCrossingEvent xEnterWindowEvent;
typedef xCrossingEvent xLeaveWindowEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW window;
    BYTE mode; 			/* really XMode */
    BYTE pad1, pad2, pad3;
} xFocusChangeEvent;

typedef xFocusChangeEvent xFocusInEvent;
typedef xFocusChangeEvent xFocusOutEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW window;
    CARD16 x, y, width, height;
    CARD16 count;
    CARD16 pad2;
} xExposeEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    DRAWABLE drawable;
    CARD16 x, y, width, height;
    CARD16 minorEvent;
    CARD16 count;
    BYTE majorEvent;
    BYTE pad1, pad2, pad3;
} xGraphicsExposeEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    DRAWABLE drawable;
    CARD16 minorEvent;
    BYTE majorEvent;
    BYTE bpad;
} xNoExposeEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW window;
    CARD8 state;
    BYTE pad1, pad2, pad3;
} xVisibilityEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW parent, window;
    INT16 x, y;
    CARD16 width, height, borderWidth;
    BOOL override;
    BYTE bpad;
} xCreateWindowEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW event, window;
} xDestroyWindowEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW event, window;
    BOOL fromConfigure;
    BYTE pad1, pad2, pad3;
} xUnmapEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW event, window;
    BOOL override;
    BYTE pad1, pad2, pad3;
} xMapEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW parent, window;
} xMapRequestEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW event, window, parent;
    INT16 x, y;
    BOOL override;
    BYTE pad1, pad2, pad3;
} xReparentEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW event, window, aboveSibling;
    INT16 x, y;
    CARD16 width, height, borderWidth;
    BOOL override;		
    BYTE bpad;
} xConfigureEvent;


typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW parent, window, sibling;
    INT16 x, y;
    CARD16 width, height, borderWidth;
    CARD16 valueMask;
    CARD32 pad1;
} xConfigureRequestEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW event, window;
    INT16 x, y;
    CARD32 pad1, pad2, pad3, pad4;
} xGravityEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW window;
    CARD16 width, height;
} xResizeRequestEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW event, window, parent;
    BYTE place;			/* Top or Bottom */
    BYTE pad1, pad2, pad3;
} xCirculateEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW window;
    ATOM atom;
    TIME time;
    BYTE state;			/* NewValue or Deleted */
    BYTE pad1;
    CARD16 pad2;
} xPropertyEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    TIME time;     
    WINDOW window;
    ATOM atom;
} xSelectionClearEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    TIME time;    
    WINDOW owner, requestor;
    ATOM selection, target, property;
} xSelectionRequestEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    TIME time;   
    WINDOW requestor;
    ATOM selection, target, property;
} xSelectionEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW window;
    COLORMAP colormap;
    BOOL new;

    BYTE state;			/* Installed or UnInstalled */
    BYTE pad1, pad2;
} xColormapEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    CARD8 request;
    KEYCODE firstKeyCode;
    CARD8 count;
    BYTE pad1;
} xMappingEvent;

typedef struct {
    BYTE type;
    BYTE detail;
    CARD16 sequenceNumber;
    WINDOW window;
    ATOM mtype;
    INT8 bytes[20];
} xClientMessageEvent;


/* KeymapNotify events are not included in the above union because they
   are different from all other events: they do not have a "detail"
   or "sequenceNumber", so there is room for a 248-bit key mask. */

typedef struct {
    BYTE type;
    BYTE map[31];
} xKeymapEvent;

/* #define XEventSize (sizeof(xEvent)) */

/* XReply is the union of all the replies above whose "fixed part"
fits in 32 bytes.  It does NOT include GetWindowAttributesReply,
QueryFontReply, QueryKeymapReply, or GetKeyboardControlReply 
ListFontsWithInfoReply */

typedef union {
    xGenericReply generic;
    xGetGeometryReply geom;
    xQueryTreeReply tree;
    xInternAtomReply atom;
    xGetAtomNameReply atomName;
    xGetPropertyReply property;
    xListPropertiesReply listProperties;
    xGetSelectionOwnerReply selection;
    xGrabPointerReply grabPointer;
    xGrabKeyboardReply grabKeyboard;
    xQueryPointerReply pointer;
    xGetMotionEventsReply motionEvents;
    xTranslateCoordsReply coords;
    xGetInputFocusReply inputFocus;
    xQueryTextExtentsReply textExtents;
    xListFontsReply fonts;
    xGetFontPathReply fontPath;
    xGetImageReply image;
    xListInstalledColormapsReply colormaps;
    xAllocColorReply allocColor;
    xAllocNamedColorReply allocNamedColor;
    xAllocColorCellsReply colorCells;
    xAllocColorPlanesReply colorPlanes;
    xQueryColorsReply colors;
    xLookupColorReply lookupColor;
    xQueryBestSizeReply bestSize;
    xQueryExtensionReply extension;
    xListExtensionsReply extensions;
    xSetModifierMappingReply setModifierMapping;
    xGetModifierMappingReply getModifierMapping;
    xSetPointerMappingReply setPointerMapping;
    xGetKeyboardMappingReply getKeyboardMapping;
    xGetPointerMappingReply getPointerMapping;
    xGetPointerControlReply pointerControl;
    xGetScreenSaverReply screenSaver;
    xListHostsReply hosts;
    xError error;
    xEvent event;
} xReply;

/*****************************************************************
 * REQUESTS
 *****************************************************************/


/* Request structure */

typedef struct _xReq {
    CARD8 reqType;
    CARD8 data;            /* meaning depends on request type */
    CARD16 length;         /* length in 4 bytes quantities 
			      of whole request, including this header */
} xReq;

/*****************************************************************
 *  structures that follow request. 
 *****************************************************************/

/* ResourceReq is used for any request which has a resource ID 
   (or Atom or Time) as its one and only argument.  */

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length;  */
    CARD32 id;  /* a Window, Drawable, Font, GContext, Pixmap, etc. */
} xResourceReq;

typedef struct {
/*     CARD8 reqType; */
/*     CARD8 depth; */
/*     CARD16 length; */
    WINDOW wid, parent;
    INT16 x, y;
    CARD16 width, height, borderWidth;  
    CARD16 class;
    VISUALID visual;
    CARD32 mask;
} xCreateWindowReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    WINDOW window;
    CARD32 valueMask; 
} xChangeWindowAttributesReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE mode; */
/*     CARD16 length; */
    WINDOW window;
} xChangeSaveSetReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    WINDOW window, parent;
    INT16 x, y;
} xReparentWindowReq;

typedef struct {
/*     CARD8 reqType; */
/*     CARD8 pad; */
/*     CARD16 length; */
    WINDOW window;
    CARD16 mask;
    CARD16 pad2;
} xConfigureWindowReq;

typedef struct {
/*     CARD8 reqType; */
/*     CARD8 direction; */
/*     CARD16 length; */
    WINDOW window;
} xCirculateWindowReq;

typedef struct {    /* followed by padded string */
/*     CARD8 reqType; */
/*     BOOL onlyIfExists; */
/*     CARD16 length; */
    CARD16 nbytes ;    /* number of bytes in string */
    CARD16 pad;
} xInternAtomReq;

typedef struct {
/*     CARD8 reqType; */
/*     CARD8 mode; */
/*     CARD16 length; */
    WINDOW window;
    ATOM property, type;
    CARD8 format;
    BYTE pad[3];
    CARD32 nUnits;     /* length of stuff following, depends on format */
} xChangePropertyReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    WINDOW window;
    ATOM property;
} xDeletePropertyReq;

typedef struct {
/*     CARD8 reqType; */
/*     BOOL delete; */
/*     CARD16 length; */
    WINDOW window;
    ATOM property, type;
    CARD32 longOffset;
    CARD32 longLength;
} xGetPropertyReq;
 
typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    WINDOW window;
    ATOM selection;
    TIME time;
} xSetSelectionOwnerReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    WINDOW requestor;
    ATOM selection, target, property;
    TIME time;
} xConvertSelectionReq;

typedef struct {
/*     CARD8 reqType; */
/*     BOOL propagate; */
/*     CARD16 length; */
    WINDOW destination;
    CARD32 eventMask;
    xEvent event;
} xSendEventReq;

typedef struct {
/*     CARD8 reqType; */
/*     BOOL ownerEvents; */
/*     CARD16 length; */
    WINDOW grabWindow;
    CARD16 eventMask;
    BYTE pointerMode, keyboardMode;
    WINDOW confineTo;
    CURSOR cursor;
    TIME time;
} xGrabPointerReq;

typedef struct {
/*     CARD8 reqType; */
/*     BOOL ownerEvents; */
/*     CARD16 length; */
    WINDOW grabWindow;
    CARD16 eventMask;
    BYTE pointerMode, keyboardMode;
    WINDOW confineTo;
    CURSOR cursor;
    CARD8 button;
    BYTE pad;
    CARD16 modifiers;
} xGrabButtonReq;

typedef struct {
/*     CARD8 reqType; */
/*     CARD8 button; */
/*     CARD16 length; */
    WINDOW grabWindow;
    CARD16 modifiers;
    CARD16 pad;
} xUngrabButtonReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    CURSOR cursor;
    TIME time;
    CARD16 eventMask;
    CARD16 pad2;
} xChangeActivePointerGrabReq;

typedef struct {
/*     CARD8 reqType; */
/*     BOOL ownerEvents; */
/*     CARD16 length; */
    WINDOW grabWindow;
    TIME time;
    BYTE pointerMode, keyboardMode;  
    CARD16 pad;
} xGrabKeyboardReq;

typedef struct {
/*     CARD8 reqType; */
/*     BOOL ownerEvents; */
/*     CARD16 length; */
    WINDOW grabWindow;
    CARD16 modifiers;
    CARD8 key;
    BYTE pointerMode, keyboardMode;  
    BYTE pad1, pad2, pad3;
} xGrabKeyReq;

typedef struct {
/*     CARD8 reqType; */
/*     CARD8 key; */
/*     CARD16 length; */
    WINDOW grabWindow;
    CARD16 modifiers;
    CARD16 pad;
} xUngrabKeyReq;

typedef struct {
/*     CARD8 reqType; */
/*     CARD8 mode; */
/*     CARD16 length; */
    TIME time;
} xAllowEventsReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    WINDOW window;
    TIME start, stop;
} xGetMotionEventsReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    WINDOW srcWid, dstWid;
    INT16 srcX, srcY;
} xTranslateCoordsReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    WINDOW srcWid, dstWid;
    INT16 srcX, srcY;
    CARD16 srcWidth, srcHeight;
    INT16 dstX, dstY;
} xWarpPointerReq;

typedef struct {
/*     CARD8 reqType; */
/*     CARD8 revertTo; */
/*     CARD16 length; */
    WINDOW focus;
    TIME time;
} xSetInputFocusReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    FONT fid;
    CARD16 nbytes;
    BYTE pad1, pad2;	/* string follows on word boundary */
} xOpenFontReq;

typedef struct {
/*     CARD8 reqType;  */
/*     BOOL oddLength; */
/*     CARD16 length;  */
    FONT fid;
} xQueryTextExtentsReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    CARD16 maxNames;
    CARD16 nbytes;  /* followed immediately by string bytes */
} xListFontsReq;

typedef xListFontsReq xListFontsWithInfoReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    CARD16 nFonts;
    BYTE pad1, pad2;	/* LISTofSTRING8 follows on word boundary */
} xSetFontPathReq;

typedef struct {
/*     CARD8 reqType; */
/*     CARD8 depth; */
/*     CARD16 length; */
    PIXMAP pid;
    DRAWABLE drawable;
    CARD16 width, height;
} xCreatePixmapReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    GCONTEXT gc;
    DRAWABLE drawable;
    CARD32 mask;
} xCreateGCReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    GCONTEXT gc;
    CARD32 mask;
} xChangeGCReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    GCONTEXT srcGC, dstGC;
    CARD32 mask;
} xCopyGCReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    GCONTEXT gc;
    CARD16 dashOffset;
    CARD16 nDashes;        /* length LISTofCARD8 of values following */
} xSetDashesReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE ordering; */
/*     CARD16 length; */
    GCONTEXT gc;
    INT16 xOrigin, yOrigin;
} xSetClipRectanglesReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BOOL exposures; */
/*     CARD16 length; */
    WINDOW window;
    INT16 x, y;
    CARD16 width, height;
} xClearAreaReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    DRAWABLE srcDrawable, dstDrawable;
    GCONTEXT gc;
    INT16 srcX, srcY, dstX, dstY;
    CARD16 width, height;
} xCopyAreaReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    DRAWABLE srcDrawable, dstDrawable;
    GCONTEXT gc;
    INT16 srcX, srcY, dstX, dstY;
    CARD16 width, height;
    CARD32 bitPlane;
} xCopyPlaneReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE coordMode; */
/*     CARD16 length; */
    DRAWABLE drawable;
    GCONTEXT gc;
} xPolyPointReq;    

typedef xPolyPointReq xPolyLineReq;  /* same request structure */

/* The following used for PolySegment, PolyRectangle, PolyArc, PolyFillRectangle, PolyFillArc */

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    DRAWABLE drawable;
    GCONTEXT gc;
} xPolySegmentReq;    

typedef xPolySegmentReq xPolyArcReq;
typedef xPolySegmentReq xPolyRectangleReq;
typedef xPolySegmentReq xPolyFillRectangleReq;
typedef xPolySegmentReq xPolyFillArcReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    DRAWABLE drawable;
    GCONTEXT gc;
    BYTE shape;
    BYTE coordMode;
    CARD16 pad1;
} xFillPolyReq;    

typedef struct {
/*     CARD8 reqType; */
/*     CARD8 format; */
/*     CARD16 length; */
    DRAWABLE drawable;
    GCONTEXT gc;
    CARD16 width, height;
    INT16 dstX, dstY;
    CARD8 leftPad;
    CARD8 depth;
    CARD16 pad;
} xPutImageReq;    

typedef struct {
/*     CARD8 reqType; */
/*     CARD8 format; */
/*     CARD16 length; */
    DRAWABLE drawable;
    INT16 x, y;
    CARD16 width, height;
    CARD32 planeMask;
} xGetImageReq;    

/* the folloiwng used by PolyText8 and PolyText16 */

typedef struct {
/*     CARD8 reqType; */
/*     CARD8 pad; */
/*     CARD16 length; */
    DRAWABLE drawable;
    GCONTEXT gc;
    INT16 x, y;		/* items (xTextElt) start after struct */
} xPolyTextReq;    

typedef xPolyTextReq xPolyText8Req;
typedef xPolyTextReq xPolyText16Req;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE nChars; */
/*     CARD16 length; */
    DRAWABLE drawable;
    GCONTEXT gc;
    INT16 x, y;
} xImageTextReq;    

typedef xImageTextReq xImageText8Req;
typedef xImageTextReq xImageText16Req;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE alloc; */
/*     CARD16 length; */
    COLORMAP mid;
    WINDOW window;
    VISUALID visual;
} xCreateColormapReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    COLORMAP mid;
    COLORMAP srcCmap;
} xCopyColormapAndFreeReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    COLORMAP cmap;
    CARD16 red, green, blue;
    CARD16 pad2;
} xAllocColorReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    COLORMAP cmap;
    CARD16 nbytes;  /* followed by structure */
    BYTE pad1, pad2;
} xAllocNamedColorReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BOOL contiguous; */
/*     CARD16 length; */
    COLORMAP cmap;
    CARD16 colors, planes;
} xAllocColorCellsReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BOOL contiguous; */
/*     CARD16 length; */
    COLORMAP cmap;
    CARD16 colors, red, green, blue;
} xAllocColorPlanesReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    COLORMAP cmap;
    CARD32 planeMask;
} xFreeColorsReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    COLORMAP cmap;
} xStoreColorsReq;    

typedef struct {
/*     CARD8 reqType; */
/*     CARD8 flags;   */ /* DoRed, DoGreen, DoBlue, as in xColorItem */
/*     CARD16 length; */
    COLORMAP cmap;
    CARD32 pixel;
    CARD16 nbytes;  /* number of name string bytes following structure */
    BYTE pad1, pad2;
} xStoreNamedColorReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    COLORMAP cmap;
} xQueryColorsReq;    

typedef struct {    /* followed  by string of length len */
/*  CARD8 reqType; */
/*  BYTE pad;      */
/*  CARD16 length; */
    COLORMAP cmap;
    CARD16 nbytes;  /* number of string bytes following structure*/
    BYTE pad1, pad2;
} xLookupColorReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    CURSOR cid;
    PIXMAP source, mask;
    CARD16 foreRed, foreGreen, foreBlue;
    CARD16 backRed, backGreen, backBlue;
    CARD16 x, y;
} xCreateCursorReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    CURSOR cid;
    FONT source, mask;
    CARD16 sourceChar, maskChar;
    CARD16 foreRed, foreGreen, foreBlue;
    CARD16 backRed, backGreen, backBlue;
} xCreateGlyphCursorReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    CURSOR cursor;
    CARD16 foreRed, foreGreen, foreBlue;
    CARD16 backRed, backGreen, backBlue;
} xRecolorCursorReq;    

typedef struct {
/*     CARD8 reqType; */
/*     CARD8 class; */
/*     CARD16 length; */
    DRAWABLE drawable;
    CARD16 width, height;
} xQueryBestSizeReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    CARD16 nbytes;  /* number of string bytes following structure */
    BYTE pad1, pad2;
} xQueryExtensionReq;

typedef struct {
/*     CARD8   reqType; */
/*     CARD8   numKeyPerModifier; */
/*     CARD16  length; */
} xSetModifierMappingReq;

typedef struct {
    CARD8 reqType;
    CARD8 nElts;  /* how many elements in the map */
    CARD16 length;
} xSetPointerMappingReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    KEYCODE firstKeyCode;
    CARD8 count;
    CARD16 pad1;
} xGetKeyboardMappingReq;    

typedef struct {
/*     CARD8 reqType; */
/*     CARD8 keyCodes; */
/*     CARD16 length; */
    KEYCODE firstKeyCode;
    CARD8 keySymsPerKeyCode;
    CARD16 pad1;
} xChangeKeyboardMappingReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    CARD32 mask;
} xChangeKeyboardControlReq;    

typedef struct {
/*     CARD8 reqType; */
/*     INT8 percent;  */ /* -100 to 100 */
/*     CARD16 length; */
} xBellReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    INT16 accelNum, accelDenum;
    INT16 threshold;             
    BOOL doAccel, doThresh;
} xChangePointerControlReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    INT16 timeout, interval;
    BYTE preferBlank, allowExpose;  
    CARD16 pad2;
} xSetScreenSaverReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE mode; */
/*     CARD16 length; */
    CARD8 hostFamily;
    BYTE pad;
    CARD16 hostLength;
} xChangeHostsReq;    

typedef struct {
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
} xListHostsReq;

typedef struct {
/*     CARD8 reqType; */
/*     BYTE mode; */
/*     CARD16 length; */
} xChangeModeReq;

typedef xChangeModeReq xSetAccessControlReq;
typedef xChangeModeReq xSetCloseDownModeReq;
typedef xChangeModeReq xForceScreenSaverReq;

typedef struct { /* followed by LIST of ATOM */
/*     CARD8 reqType; */
/*     BYTE pad; */
/*     CARD16 length; */
    WINDOW window;
    CARD16 nAtoms;
    INT16 nPositions;
} xRotatePropertiesReq;
    
/* Reply codes */

#define X_Reply		1		/* Normal reply */
#define X_Error		0		/* Error */

/* Request codes */

#define X_CreateWindow                  1              
#define X_ChangeWindowAttributes        2        
#define X_GetWindowAttributes           3     
#define X_DestroyWindow                 4
#define X_DestroySubwindows             5   
#define X_ChangeSaveSet                 6
#define X_ReparentWindow                7
#define X_MapWindow                     8
#define X_MapSubwindows                 9
#define X_UnmapWindow                  10
#define X_UnmapSubwindows              11  
#define X_ConfigureWindow              12  
#define X_CirculateWindow              13  
#define X_GetGeometry                  14
#define X_QueryTree                    15
#define X_InternAtom                   16
#define X_GetAtomName                  17
#define X_ChangeProperty               18 
#define X_DeleteProperty               19 
#define X_GetProperty                  20
#define X_ListProperties               21 
#define X_SetSelectionOwner            22    
#define X_GetSelectionOwner            23    
#define X_ConvertSelection             24   
#define X_SendEvent                    25
#define X_GrabPointer                  26
#define X_UngrabPointer                27
#define X_GrabButton                   28
#define X_UngrabButton                 29
#define X_ChangeActivePointerGrab      30          
#define X_GrabKeyboard                 31
#define X_UngrabKeyboard               32 
#define X_GrabKey                      33
#define X_UngrabKey                    34
#define X_AllowEvents                  35       
#define X_GrabServer                   36      
#define X_UngrabServer                 37        
#define X_QueryPointer                 38        
#define X_GetMotionEvents              39           
#define X_TranslateCoords              40                
#define X_WarpPointer                  41       
#define X_SetInputFocus                42         
#define X_GetInputFocus                43         
#define X_QueryKeymap                  44       
#define X_OpenFont                     45    
#define X_CloseFont                    46     
#define X_QueryFont                    47
#define X_QueryTextExtents             48     
#define X_ListFonts                    49  
#define X_ListFontsWithInfo    	       50 
#define X_SetFontPath                  51 
#define X_GetFontPath                  52 
#define X_CreatePixmap                 53        
#define X_FreePixmap                   54      
#define X_CreateGC                     55    
#define X_ChangeGC                     56    
#define X_CopyGC                       57  
#define X_SetDashes                    58     
#define X_SetClipRectangles            59             
#define X_FreeGC                       60  
#define X_ClearArea                    61             
#define X_CopyArea                     62    
#define X_CopyPlane                    63     
#define X_PolyPoint                    64     
#define X_PolyLine                     65    
#define X_PolySegment                  66       
#define X_PolyRectangle                67         
#define X_PolyArc                      68   
#define X_FillPoly                     69    
#define X_PolyFillRectangle            70             
#define X_PolyFillArc                  71       
#define X_PutImage                     72    
#define X_GetImage                     73 
#define X_PolyText8                    74     
#define X_PolyText16                   75      
#define X_ImageText8                   76      
#define X_ImageText16                  77       
#define X_CreateColormap               78          
#define X_FreeColormap                 79        
#define X_CopyColormapAndFree          80               
#define X_InstallColormap              81           
#define X_UninstallColormap            82             
#define X_ListInstalledColormaps       83                  
#define X_AllocColor                   84      
#define X_AllocNamedColor              85           
#define X_AllocColorCells              86           
#define X_AllocColorPlanes             87            
#define X_FreeColors                   88      
#define X_StoreColors                  89       
#define X_StoreNamedColor              90           
#define X_QueryColors                  91       
#define X_LookupColor                  92       
#define X_CreateCursor                 93        
#define X_CreateGlyphCursor            94             
#define X_FreeCursor                   95      
#define X_RecolorCursor                96         
#define X_QueryBestSize                97         
#define X_QueryExtension               98          
#define X_ListExtensions               99          
#define X_ChangeKeyboardMapping        100
#define X_GetKeyboardMapping           101
#define X_ChangeKeyboardControl        102                
#define X_GetKeyboardControl           103             
#define X_Bell                         104
#define X_ChangePointerControl         105
#define X_GetPointerControl            106
#define X_SetScreenSaver               107          
#define X_GetScreenSaver               108          
#define X_ChangeHosts                  109       
#define X_ListHosts                    110     
#define X_SetAccessControl             111               
#define X_SetCloseDownMode             112
#define X_KillClient                   113 
#define X_RotateProperties	       114
#define X_ForceScreenSaver	       115
#define X_SetPointerMapping            116
#define X_GetPointerMapping            117
#define X_SetModifierMapping	       118
#define X_GetModifierMapping	       119
#define X_NoOperation                  127
