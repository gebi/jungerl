
-module(gd1_test).
-define(DRV, gd1_drv).

-include("gd1_drv.hrl").

-export([regression/0]).

regression() ->
    {ok, Port1} = ?DRV:start(),
    {ok, 1} = ?DRV:debug(Port1, 1),
    ok = short_example(Port1),
    ok = r1(Port1),
    ok = r2(Port1),
    io:format("Regression test via linked-in method succeeded.\n"),
    %timer:sleep(3000),
    %
    %{ok, Port2} = ?DRV:start_pipe(),
    %{ok, 1} = ?DRV:debug(Port2, 1),
    %ok = short_example(Port2),
    %ok = r1(Port2),
    %ok = r2(Port2),
    %io:format("Regression test via pipe method succeeded.\n"),
    %timer:sleep(3000),

    io:format("All regression tests PASSED.\n"),
    ok.

% Not really a regression test, but rather a direct translation of
% "Here is a short example program" found in "readme.txt" in GD's 
% source distribution.
short_example(Port) ->
    %% Doesn't work, no error checking done by gdImageCreate()!
    %% {error, 0} = ?DRV:gdImageCreate(Port, -1, -2),
    %% Allocate the image: 64 pixels across by 64 pixels tall
    {ok, IM} = ?DRV:gdImageCreate(Port, 64, 64),

    %% Allocate the color black (red, green and blue all minimum)
    %% Since this is the first color in a new image, it will
    %% be the background color.
    {ok, Black} = ?DRV:gdImageColorAllocate(Port, IM, 0, 0, 0),
    %% Allocate the color white (red, green and blue all maximum).
    {ok, White} = ?DRV:gdImageColorAllocate(Port, IM, 255, 255, 255),

    %% Draw a line from the upper left to the lower right,
    %% using white color index.
    ok = ?DRV:gdImageLine(Port, IM, 0, 0, 63, 63, White),
    
    %% Encapsulation-style
    %% Output the image to the disk file in PNG format.
    {ok, 0} = ?DRV:gdImagePng(Port, IM, "test.png"),
    %% Output the same image in JPEG format, using the default
    %% JPEG quality setting.
    {ok, 0} = ?DRV:gdImageJpeg(Port, IM, "test.jpg", -1),

    %% Explicit-style ... this time use the fopen- & fclose-style.
    %% Open a file for writing. "wb" means "write binary", important
    %% under MSDOS, harmless under Unix.
    {ok, PngOut} = ?DRV:fopen(Port, "test2.png", "wb"),
    %% Do the same for a JPEG-format file.
    {ok, JpegOut} = ?DRV:fopen(Port, "test2.jpg", "wb"),
    %% Output the image to the disk file in PNG format.
    ok = ?DRV:gdImagePngFILE(Port, IM, PngOut),
    %% Output the same image in JPEG format, using the default
    %% JPEG quality setting.
    ok = ?DRV:gdImageJpegFILE(Port, IM, JpegOut, -1),
    %% Close the files.
    {ok, 0} = ?DRV:fclose(Port, PngOut),
    {ok, 0} = ?DRV:fclose(Port, JpegOut),

    %% This isn't part of the "short example", but we need the White color idx
    {ok, 0} = ?DRV:gdImageWBMP(Port, IM, White, "test.wbmp"),
    {ok, B3} = ?DRV:gdImageWBMPPtr(Port, IM, White),

    %% Destroy the image in memory.
    ok = ?DRV:gdImageDestroy(Port, IM),
    
    ok.

%% We can be a bit more thorough here
r1(Port) ->
    io:format("\n\nQQQ BEFORE gdImageCreateFromJpeg succeeded!\n\n"),
    {ok, IM1} = ?DRV:gdImageCreateFromJpeg(Port, "test.jpg"),
    {error, _} = ?DRV:gdImageCreateFromJpeg(Port, "NOEXIST.jpg"),
    {ok, IM2} = ?DRV:gdImageCreateFromPng(Port, "test.png"),
    {error, _} = ?DRV:gdImageCreateFromPng(Port, "NOEXIST.png"),
    ok = ?DRV:gdImageDestroy(Port, IM1),
    ok = ?DRV:gdImageDestroy(Port, IM2),
    
    {ok, IM3} = ?DRV:gdImageCreateFromGd(Port, "Test-Files/test.gd"),
    {ok, IM4} = ?DRV:gdImageCreateFromGd2(Port, "Test-Files/test.gd2"),
    %% We'll let these two images "leak".
    %% Besides, we may use them later.

    %% The "hosed.gd2" is corrupt.  Make certain we get a -1 error back.
    {error, -1} = ?DRV:gdImageCreateFromGd2(Port, "Test-Files/hosed.gd2"),

    {ok, IM5} = ?DRV:gdImageCreateFromGd2Part(Port, "Test-Files/test.gd2",
					      0, 0, 32, 32),
    ok = ?DRV:gdImageDestroy(Port, IM5),

    %% This function will only work if libgd.so was built with XPM support.
    %% {ok, IM6} = ?DRV:gdImageCreateFromXpm(Port, "Test-Files/test.xpm"),
    %% ok = ?DRV:gdImageDestroy(Port, IM6),

    %% IM3 is based on slurping in "test.gd"
    {ok, B1} = ?DRV:gdImagePngPtr(Port, IM3),
    io:format("FWIW, B1 = gdImagePngPtr(IM3) = ~w\n", [B1]),
    %% Pattern match: B1 should exactly equal "test.png"'s contents.
    {ok, F1} = file:open("test.png", [read, raw, binary]),
    {ok, B1} = file:read(F1, size(B1) * 10),
    file:close(F1),

    %% IM3 is based on slurping in "test.gd"
    {ok, B2} = ?DRV:gdImageJpegPtr(Port, IM3, -1),
    %% Pattern match: B2 should exactly equal "test.jpg"'s contents.
    {ok, F2} = file:open("test.jpg", [read, raw, binary]),
    {ok, B2} = file:read(F2, size(B2) * 10),
    file:close(F2),

    %% IM3 is based on slurping in "test.gd"
    {ok, 0} = ?DRV:gdImageGd(Port, IM3, "test-copy.gd"),
    %% Make certain the file exists and is the same as our reference copy
    {ok, F3a} = file:open("test-copy.gd", [read, raw, binary]),
    {ok, B3} = file:read(F3a, 8192),
    file:close(F3a),
    {ok, F3b} = file:open("Test-Files/test.gd", [read, raw, binary]),
    {ok, B3} = file:read(F3b, 8192),
    file:close(F3b),
    %% On-the-fly binary should be same as ref copy, too
    {ok, B3} = ?DRV:gdImageGdPtr(Port, IM3),

    %% IM3 is based on slurping in "test.gd"
    {ok, 0} = ?DRV:gdImageGd2(Port, IM3, "test-copy.gd2", 1024, ?GD2_FMT_COMPRESSED),
    %% Make certain the file exists and is the same as our reference copy
    {ok, F4a} = file:open("test-copy.gd2", [read, raw, binary]),
    {ok, B4} = file:read(F4a, 8192),
    file:close(F4a),
    {ok, F4b} = file:open("Test-Files/test.gd2", [read, raw, binary]),
    {ok, B4} = file:read(F4b, 8192),
    file:close(F4b),
    %% On-the-fly binary should be same as ref copy, too
    {ok, B4} = ?DRV:gdImageGd2Ptr(Port, IM3, 1024, ?GD2_FMT_COMPRESSED),

    io:format("Regression test 'r1' finished successfully\n"),
    
    ok.

%%%
%%% r2: Test more drawing primitives.
%%%
r2(Port) ->
    {ok, IM} = ?DRV:gdImageCreate(Port, 256, 256),
    %% First color = background color.
    {ok, Black} = ?DRV:gdImageColorAllocate(Port, IM, 0, 0, 0),
    {ok, White} = ?DRV:gdImageColorAllocate(Port, IM, 255, 255, 255),
    {ok, Cyan} = ?DRV:gdImageColorAllocate(Port, IM, 0, 255, 255),
    {ok, DGrey} = ?DRV:gdImageColorAllocate(Port, IM, 75, 75, 75),
    {ok, Red} = ?DRV:gdImageColorAllocate(Port, IM, 255, 0, 0),
    {ok, Purple} = ?DRV:gdImageColorAllocate(Port, IM, 255, 0, 255),

    ok = ?DRV:gdImageSetPixel(Port, IM, 5, 20, White),
    ok = ?DRV:gdImageLine(Port, IM, 0, 0, 10, 25, White),

    {NVert1, Vert1} = ?DRV:make_vertex_array([0, 0, 30, 30, 30, 0]),
    {ok, 0} = ?DRV:gdImagePolygon(Port, IM, Vert1, NVert1, White),
    {error, badarg} = ?DRV:gdImagePolygon(Port, IM, Vert1, NVert1 + 3, White),
    {NVert2, Vert2} = ?DRV:make_vertex_array([{0, 30}, {30, 30},
					      {30, 60}, {0, 60}]),
    {ok, 0} = ?DRV:gdImageFilledPolygon(Port, IM, Vert2, NVert2, Cyan),

    ok = ?DRV:gdImageRectangle(Port, IM, 0, 65, 200, 75, Cyan),
    ok = ?DRV:gdImageFilledRectangle(Port, IM, 0, 85, 200, 95, White),
    ok = ?DRV:gdImageArc(Port, IM, 70, 30, 60, 40, 0, 360, White),
    ok = ?DRV:gdImageArc(Port, IM, 70, 40, 60, 40, 0, 90, Cyan),

    ok = ?DRV:gdImageRectangle(Port, IM, 0, 105, 200, 115, Cyan),
    ok = ?DRV:gdImageFillToBorder(Port, IM, 5, 110, Cyan, DGrey),

    ok = ?DRV:gdImageRectangle(Port, IM, 0, 125, 200, 135, Cyan),
    ok = ?DRV:gdImageLine(Port, IM, 100, 120, 100, 140, Red),
    ok = ?DRV:gdImageFill(Port, IM, 5, 130, DGrey),

    {ok, Brush1IM} = ?DRV:gdImageCreateFromPng(Port, "Test-Files/brush.png"),
    ok = ?DRV:gdImageSetBrush(Port, IM, Brush1IM),
    ok = ?DRV:gdImageLine(Port, IM, 63, 30, 77, 30, ?GD2_gdBrushed),

    ok = ?DRV:gdImageSetTile(Port, IM, Brush1IM),
    ok = ?DRV:gdImageFilledRectangle(Port, IM, 110, 0, 170, 63, ?GD2_gdTiled),

    Style1 = ?DRV:make_style_array([White, White, White, Red, Red, Red,
				    Cyan, Cyan, Cyan, Black, Black, Black]),
    {ok, 0} = ?DRV:gdImageSetStyle(Port, IM, Style1, length(Style1)),
    ok = ?DRV:gdImageLine(Port, IM, 40, 5, 100, 5, ?GD2_gdStyled),

    {ok, White} = ?DRV:gdImageGetPixel(Port, IM, 0, 0),
    {ok, Cyan} = ?DRV:gdImageGetPixel(Port, IM, 0, 65),
    {ok, 255} = ?DRV:gdImageRed(Port, IM, White),
    {ok, 255} = ?DRV:gdImageGreen(Port, IM, White),
    {ok, 255} = ?DRV:gdImageBlue(Port, IM, White),
    {ok, 1} = ?DRV:gdImageBoundsSafe(Port, IM, 0, 65),
    {ok, 0} = ?DRV:gdImageBoundsSafe(Port, IM, 0, 465),
    {ok, 0} = ?DRV:gdImageBoundsSafe(Port, IM, -5, 65),
    {ok, 256} = ?DRV:gdImageSX(Port, IM),
    {ok, 256} = ?DRV:gdImageSY(Port, IM),

    {ok, FTiny} = ?DRV:getFontPtr(Port, ?_FONT_TINY),
    {ok, FSmall} = ?DRV:getFontPtr(Port, ?_FONT_SMALL),
    {ok, FMediumBold} = ?DRV:getFontPtr(Port, ?_FONT_MEDIUMBOLD),
    {ok, FLarge} = ?DRV:getFontPtr(Port, ?_FONT_LARGE),
    {ok, FGiant} = ?DRV:getFontPtr(Port, ?_FONT_GIANT),

    ok = ?DRV:gdImageChar(Port, IM, FTiny, 5, 140, $C, White),
    ok = ?DRV:gdImageChar(Port, IM, FSmall, 10, 140, $C, White),
    ok = ?DRV:gdImageChar(Port, IM, FMediumBold, 16, 140, $C, White),
    ok = ?DRV:gdImageChar(Port, IM, FLarge, 22, 140, $C, White),
    ok = ?DRV:gdImageChar(Port, IM, FGiant, 30, 140, $C, White),
    ok = ?DRV:gdImageCharUp(Port, IM, FGiant, 37, 152, $C, White),
    ok = ?DRV:gdImageString(Port, IM, FMediumBold, 55, 140, "Hello, world!", Red),
    %% XXX gdImageString16() and gdImageStringUp16() unsupported at this time.
    ok = ?DRV:gdImageStringUp(Port, IM, FMediumBold, 170, 60, "Hello, world!", Cyan),

    {ok, White} = ?DRV:gdImageColorClosest(Port, IM, 254, 254, 254),
    {ok, White} = ?DRV:gdImageColorExact(Port, IM, 255, 255, 255),
    {error, -1} = ?DRV:gdImageColorExact(Port, IM, 4, 4, 4),
    {ok, Blue} = ?DRV:gdImageColorResolve(Port, IM, 0, 0, 255),
    
    {ok, NumColors} = ?DRV:gdImageColorsTotal(Port, IM),
    io:format("\nNumber of colors used so far = ~w\n\n", [NumColors]),
    {ok, 0} = ?DRV:gdImageGetInterlaced(Port, IM),
    {error, -1} = ?DRV:gdImageGetTransparent(Port, IM),

    %% Write the string "Green!" using the color purple.
    ok = ?DRV:gdImageString(Port, IM, FMediumBold, 5, 155, "Green!", Purple),
    ok = ?DRV:gdImageColorDeallocate(Port, IM, Purple),
    {ok, Green} = ?DRV:gdImageColorAllocate(Port, IM, 0, 255, 0),
    %% Now the string "Green!" should be green.

    ok = ?DRV:gdImageColorTransparent(Port, IM, Black),
    {ok, Black} = ?DRV:gdImageGetTransparent(Port, IM),

    {ok, IM2} = ?DRV:gdImageCreate(Port, 300, 256),
    {ok, DGrey2} = ?DRV:gdImageColorAllocate(Port, IM2, 75, 75, 75),
    ok = ?DRV:gdImageCopy(Port, IM2, IM, 0, 0, 0, 0, 50, 50),
    ok = ?DRV:gdImageCopyResized(Port, IM2, IM, 50, 0, 50, 0, 50, 50, 100, 100),
    ok = ?DRV:gdImageCopyMerge(Port, IM2, IM, 0, 50, 0, 0, 50, 50, 50),
    ok = ?DRV:gdImageCopyMergeGray(Port, IM2, IM, 50, 50, 0, 0, 50, 50, 10),

    {ok, IM3} = ?DRV:gdImageCreate(Port, 256, 256),
    ok = ?DRV:gdImagePaletteCopy(Port, IM3, IM),
    Mask = ?GD_CMP_IMAGE bor ?GD_CMP_NUM_COLORS bor ?GD_CMP_COLOR bor
	?GD_CMP_SIZE_X bor ?GD_CMP_TRANSPARENT,
    {ok, Mask} = ?DRV:gdImageCompare(Port, IM, IM2),
    Mask2 = Mask bor ?GD_CMP_INTERLACE,
    ok = ?DRV:gdImageInterlace(Port, IM2, 1),
    {ok, Mask2} = ?DRV:gdImageCompare(Port, IM, IM2),

    {ok, 0} = ?DRV:gdImagePng(Port, IM, "test_r2.png"),
    {ok, 0} = ?DRV:gdImagePng(Port, IM2, "test_r2-2.png"),
    %%
    %% I suspect difference is the version of zlib used with libpng causes
    %% different sized files but the images really are identical.  If
    %% your system creates something that matches neither file in
    %% "Test-Files", then inspect the images manually to see if they
    %% really are the same.  (Exporting both to another graphics format,
    %% preferably uncompressed, is an even better comparison method!)
    %%
    cmp_file("test_r2.png",
	     "Test-Files/test_r2.png", "Test-Files/test_r2-b.png"),
    cmp_file("test_r2-2.png", "Test-Files/test_r2-2.png"),
    %% All done!

    io:format("Regression test 'r2' finished successfully\n"),

    ok.

%%% Brute force.
cmp_file(File1, File2) ->
    cmp_file(File1, File2, File2).
cmp_file(File1, File2, File2b) ->
    {ok, F1} = file:open(File1, [read, raw, binary]),
    {ok, B1} = file:read(F1, 128*1024),
    {ok, F2} = file:open(File2, [read, raw, binary]),
    {ok, B2} = file:read(F2, 128*1024),
    case B1 of
	B2 ->
	    ok;
	_ ->
	    {ok, F2b} = file:open(File2b, [read, raw, binary]),
	    {ok, B2b} = file:read(F2b, 128*1024),
	    file:close(F2b),
	    B1 = B2b
    end,
    file:close(F1),
    file:close(F2),
    ok.
