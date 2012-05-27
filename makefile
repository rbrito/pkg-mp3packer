ifdef ComSpec
	#Windows
	O=.obj
	EXE=.exe
	EXLIBRARIES=Shell32.lib
else
	#Other
	O=.o
	EXE=
	EXLIBRARIES=
endif


common=unix.cmxa str.cmxa $(EXLIBRARIES)

ocaml=ocamlopt.opt $(common)


all: mp3packer mp3reader


.SUFFIXES: .ml .mli .cmo .cmi .cmx $O

.ml.cmx:
	$(ocaml) -c $<

.mli.cmi:
	$(ocaml) -c $<

ifdef ComSpec
.c$O:
	$(ocaml) -ccopt "/Fa$@.asm" -c $<
else
.c$O:
	$(ocaml) -c $<
endif


ALLREQS=ptr.cmx ptr-c$O crc.cmx list2.cmx expandarray.cmx c_part$O unicode.cmx unicode-c$O mp3types.cmx pack.cmx mp3read.cmx mp3write.cmx mp3info.cmx mp3framehuffman.cmx mp3frameutils.cmx mp3queue.cmx multiproc.cmx



MP3PACKER=$(ALLREQS) mp3packer.cmx

mp3packer: depend $(MP3PACKER)
	$(ocaml) -o $@$(EXE) $(MP3PACKER)


MP3READER=$(ALLREQS) mp3reader.cmx

mp3reader: depend $(MP3READER)
	$(ocaml) -o $@$(EXE) $(MP3READER)



ptr_test: depend $(ALLREQS) ptr_test.cmx
	$(ocaml) -o $@$(EXE) $(ALLREQS) ptr_test.cmx
	./ptr_test$(EXE)



clean:
	rm -f ./mp3packer$(EXE) ./mp3reader$(EXE) ./*$O ./*.cmi ./*.cmx ./*.cmo ./*.cma ./*.cmxa ./*.asm

depend:
	ocamldep *.mli *.ml > .depend

include .depend

#OBJ_EXT=.obj
#EXE_EXT=.exe
#
#ocaml=ocamlopt
#
#all: mp3packer mp3reader
#
#mp3packer: ptr.cmx ptr-c$(OBJ_EXT) crc.cmx list2.cmx expandarray.cmx c_part$(OBJ_EXT) mp3types.cmx pack.cmx mp3read.cmx mp3write.cmx mp3info.cmx mp3framehuffman.cmx mp3frameutils.cmx mp3queue.cmx multiproc.cmx mp3packer.ml
#	ocamlopt -o mp3packer$(EXE_EXT) unix.cmxa str.cmxa crc.cmx list2.cmx expandarray.cmx c_part$(OBJ_EXT) mp3types.cmx pack.cmx mp3read.cmx mp3write.cmx mp3info.cmx mp3framehuffman.cmx mp3frameutils.cmx mp3queue.cmx multiproc.cmx mp3packer.ml
## mp3packer: crc.cmx expandarray.cmx mp3types.cmx pack.cmx mp3read.cmx mp3info.cmx mp3framehuffman.cmx mp3queue.cmx mp3packer.ml
##	ocamlopt -o mp3packer unix.cmxa str.cmxa crc.cmx expandarray.cmx mp3types.cmx pack.cmx mp3read.cmx mp3info.cmx mp3framehuffman.cmx mp3queue.cmx mp3packer.ml
#
#mp3reader: crc.cmx list2.cmx c_part$(OBJ_EXT) mp3types.cmx pack.cmx mp3read.cmx mp3reader.ml
#	ocamlopt -o mp3reader$(EXE_EXT) unix.cmxa str.cmxa crc.cmx list2.cmx c_part$(OBJ_EXT) mp3types.cmx pack.cmx mp3read.cmx mp3reader.ml
#
#
#mp3framehuffman.cmx: mp3types.cmx pack.cmx mp3framehuffman.ml
#	ocamlopt -c mp3types.cmx pack.cmx mp3framehuffman.ml
#
#
#
#mp3frameutils.cmx: mp3types.cmx pack.cmx mp3framehuffman.cmx mp3frameutils.ml
#	ocamlopt -c mp3types.cmx pack.cmx mp3framehuffman.cmx mp3frameutils.ml
#
#mp3framehuffmantest: mp3types.cmx pack.cmx mp3framehuffman.cmx mp3framehuffmantest.ml
#	ocamlopt -o mp3framehuffmantest$(EXE_EXT) unix.cmxa str.cmxa mp3types.cmx pack.cmx mp3framehuffman.cmx mp3framehuffmantest.ml
#
#
#
#%.cmx: %.ml
#	$(ocaml) -c $<
#
#multiproc.cmx: mp3types.cmx mp3read.cmx mp3write.cmx mp3queue.cmx multiproc.ml
#mp3read.cmx: mp3types.cmx pack.cmx mp3read.ml
#mp3write.cmx: mp3types.cmx pack.cmx mp3write.ml
#mp3queue.cmx: list2.cmx mp3types.cmx pack.cmx mp3read.cmx mp3write.cmx mp3framehuffman.cmx mp3frameutils.cmx mp3queue.ml
#mp3info.cmx: expandarray.cmx mp3types.cmx pack.cmx mp3read.cmx mp3write.cmx mp3info.ml
#crc.cmx: crc.ml
#pack.cmx: pack.ml
#mp3types.cmx: mp3types.ml
#
##mp3read.cmx: mp3types.cmx pack.cmx mp3read.ml
##	ocamlopt -c mp3types.cmx pack.cmx mp3read.ml
##
##mp3write.cmx: mp3types.cmx pack.cmx mp3write.ml
##	ocamlopt -c mp3types.cmx pack.cmx mp3write.ml
##
##mp3queue.cmx: list2.cmx mp3types.cmx pack.cmx mp3read.cmx mp3write.cmx mp3framehuffman.cmx mp3frameutils.cmx mp3queue.ml
##	ocamlopt -c list2.cmx mp3types.cmx pack.cmx mp3read.cmx mp3write.cmx mp3framehuffman.cmx mp3frameutils.cmx mp3queue.ml
##
##mp3info.cmx: expandarray.cmx mp3types.cmx pack.cmx mp3read.cmx mp3write.cmx mp3info.ml
##	ocamlopt -c expandarray.cmx mp3types.cmx pack.cmx mp3read.cmx mp3write.cmx mp3info.ml
##
##crc.cmx: crc.ml
##	ocamlopt -c crc.ml
##
##pack.cmx: pack.ml
##	ocamlopt -c pack.ml
##
##mp3types.cmx: mp3types.ml
##	ocamlopt -c mp3types.ml
#
#
#
#mp3read_test: mp3types.cmx crc.cmx pack.cmx mp3read.cmx mp3read_test.ml
#	ocamlopt -o mp3read_test$(EXE_EXT) str.cmxa mp3types.cmx crc.cmx pack.cmx mp3read.cmx mp3read_test.ml
#	./mp3read_test$(EXE_EXT)
#
#mp3queue_test: mp3types.cmx crc.cmx pack.cmx mp3read.cmx mp3framehuffman.cmx mp3frameutils.cmx mp3queue.cmx mp3queue_test.ml
#	ocamlopt -o mp3queue_test$(EXE_EXT) mp3types.cmx crc.cmx pack.cmx mp3read.cmx mp3framehuffman.cmx mp3frameutils.cmx mp3queue.cmx mp3queue_test.ml
#	./mp3queue_test$(EXE_EXT)
#
#mp3info_test: crc.cmx mp3types.cmx pack.cmx mp3read.cmx mp3info.cmx mp3info_test.ml
#	ocamlopt -o mp3info_test$(EXE_EXT) crc.cmx mp3types.cmx pack.cmx mp3read.cmx mp3info.cmx mp3info_test.ml
#	./mp3info_test$(EXE_EXT)
#
#mp3queue_concept: mp3queue_concept.ml
#	ocamlopt -o mp3queue_concept$(EXE_EXT) mp3queue_concept.ml
#	./mp3queue_concept$(EXE_EXT)
#
#ptr_test: ptr-c$(OBJ_EXT) ptr.cmx ptr_test.ml
#	ocamlopt -o ptr_test$(EXE_EXT) unix.cmxa ptr-c$(OBJ_EXT) ptr.cmx ptr_test.ml
#	./ptr_test$(EXE_EXT)
#
#
#expandarray.cmx: expandarray.ml expandarray.mli
#	ocamlopt expandarray.mli
#	ocamlopt -c expandarray.ml
#
#
### C STUFF ##
#c_part$(OBJ_EXT): c_part.c
#	ocamlopt c_part.c
#
#ptr-c$(OBJ_EXT): ptr-c.c
#	ocamlopt ptr-c.c
#
##c_module.cmx: c_part.obj c_module.ml
##	ocamlopt -c c_part.obj c_module.ml
#
#
#
#
#
### Lists ##
#list2.cmx: list2.ml list2.mli
#	ocamlopt list2.mli
#	ocamlopt -c list2.ml
#
##list2_test: list2.cmx list2_test.ml
##	ocamlopt -o list2_test list2.cmx list2_test.ml
##	list2_test
#
#
#clean:
#	rm -rf ./mp3packer.exe
#	rm -rf ./*.obj
#	rm -rf ./*.cmi
#	rm -rf ./*.cmx
#	rm -rf ./*.cmo
#	rm -rf ./*.cma
#	rm -rf ./*.cmxa
#	rm -rf ./*.lib
#	rm -rf ./*.asm
#
#cleanw:
#	del /s /q /f .\mp3packer.exe
#	del /s /q /f .\*.obj
#	del /s /q /f .\*.cmi
#	del /s /q /f .\*.cmx
#	del /s /q /f .\*.cmo
#	del /s /q /f .\*.cma
#	del /s /q /f .\*.cmxa
#	del /s /q /f .\*.lib
#	del /s /q /f .\*.asm
#
#remake: clean mp3packer
#
