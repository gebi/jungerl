include ../../../support/include.mk

ESP_CAT=../priv/bin/esp_cat

all:	$(ESP_CAT)

clean:
	rm esp_cat.o

$(ESP_CAT):	esp_cat.o
	$(CC) -o$@ esp_cat.o $(SOCKET_LIBS)


