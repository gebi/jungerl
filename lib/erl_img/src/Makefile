
EBIN = ../ebin
ERLC = erlc
ERLC_FLAGS = -I../include -pa /usr/local/lib/erlang/lib/erlgtk-0.9.7/ebin

OBJS = 	$(EBIN)/image_bmp.beam \
	$(EBIN)/image_x_xpixmap.beam \
	$(EBIN)/image_png.beam \
	$(EBIN)/image_gif.beam \
	$(EBIN)/image_jpeg.beam \
	$(EBIN)/image_tiff.beam \
	$(EBIN)/image_undef.beam \
	$(EBIN)/video_mpg.beam \
	$(EBIN)/exif.beam \
	$(EBIN)/lzw.beam \
	$(EBIN)/zlib.beam \
	$(EBIN)/erl_img.beam \
	$(EBIN)/erl_img_show.beam


# $(EBIN)/photo.beam 

all:	$(OBJS)

$(OBJS): ../include/erl_img.hrl

$(EBIN)/image_tiff.beam:	tiff.hrl
$(EBIN)/image_jpeg.beam:	jpeg.hrl exif.hrl
$(EBIN)/exif.beam:		exif.hrl

$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN) $<

