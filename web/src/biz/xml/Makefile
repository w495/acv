ERL=erl


all:
	$(MAKE)  --quiet  crc

crc: compile run clean

run:
	$(ERL) -eval "example:main(), halt()."

compile:
	$(ERL) -make

clean:
	rm -rf *.beam
	rm -rf *.dump
	rm -rf *~