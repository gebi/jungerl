%%
%% image format A  R  G  B 
%% pixel data   16 16 16 16
%%
-ifndef(__ERL_IMG_HRL__).
-define(__ERL_IMG_HRL__, true).

-define(IMAGE_JPEG,      image_jpeg).
-define(IMAGE_TIFF,      image_tiff).
-define(IMAGE_GIF,       image_gif).
-define(IMAGE_PNG,       image_png).
-define(IMAGE_BMP,       image_bmp).
-define(IMAGE_X_XPIXMAP, image_x_xpixmap).
-define(IMAGE_UNDEF,     image_undef).
-define(VIDEO_MPEG,      video_mpeg).

-define(PAD_Len(L,A), (((A)-((L) rem (A))) rem (A))).

-define(PAD_Len8(L), ((8 - ((L) band 7)) band 7)).

-define(PAD(L,A),
        case ?PAD4_Len(L,A) of
            0 -> <<>>;
            1 -> <<0>>;
            2 -> <<0,0>>;
            3 -> <<0,0,0>>;
            4 -> <<0,0,0,0>>;
            5 -> <<0,0,0,0,0>>;
            6 -> <<0,0,0,0,0,0>>;
            7 -> <<0,0,0,0,0,0,0>>;
	    N -> list_to_binary(lists:duplicate(N,0))
	end).

-define(IMAGE_TYPES, [?IMAGE_JPEG,
		      ?IMAGE_TIFF,
                      ?IMAGE_GIF,
                      ?IMAGE_PNG,
                      ?IMAGE_BMP,
                      ?IMAGE_X_XPIXMAP,
                      ?VIDEO_MPEG]).

-record(erl_pixmap,
	{
	  top      = 0,
	  left     = 0,
	  width    = 0,
	  height   = 0,
	  palette,          %% list [{R,G,B}]
	  format,           %% pixmap format
	  attributes = [],  %% extension codes
	  pixels   = []     %% [ {Ri,binary(Row)} ]
	 }).


-record(erl_image,
	{
	  type,         %% module name of image handler
	  name,         %% Image name (no path)
	  filename,     %% Full filename
	  size,         %% File size
	  extension,    %% extension used
	  mtime,        %% file creation date {{YYYY,MM,DD},{HH,MM,SS}}
          itime,        %% image creation date {{YYYY,MM,DD},{HH,MM,SS}}
          comment = "", %% image comment (if present)
	  format,       %% pixel format:
	                %%  gray4, gray8,
	                %%  palette4, palette8
	                %%  b8g8r8 r8g8b8 r8g8b8a8
	  width,        %% Image width
	  height,       %% Image height
	  depth,        %% Image depth
	  bytes_pp = 3, %% bytes per pixel
	  alignment = 1,
          attributes = [], %% list of attributes [{atom(Key),term(Value)}]
	  order,        %% sample order left_to_right or right_to_left
	  palette,      %% list [{R,G,B}]
	  pixmaps = []  %% [#erl_pixmap]
	 }).

-endif.

