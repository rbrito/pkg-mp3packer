# Change this if compiling on Unixy OS
OBJ_EXT=.obj
EXE_EXT=.exe

all: mp3packer mp3reader

mp3packer: crc.cmx list2.cmx expandarray.cmx c_part$(OBJ_EXT) mp3types.cmx pack.cmx mp3read.cmx mp3info.cmx mp3framehuffman.cmx mp3frameutils.cmx mp3queue.cmx mp3packer.ml
	ocamlopt -o mp3packer$(EXE_EXT) unix.cmxa str.cmxa crc.cmx list2.cmx expandarray.cmx c_part$(OBJ_EXT) mp3types.cmx pack.cmx mp3read.cmx mp3info.cmx mp3framehuffman.cmx mp3frameutils.cmx mp3queue.cmx mp3packer.ml
# mp3packer: crc.cmx expandarray.cmx mp3types.cmx pack.cmx mp3read.cmx mp3info.cmx mp3framehuffman.cmx mp3queue.cmx mp3packer.ml
#	ocamlopt -o mp3packer unix.cmxa str.cmxa crc.cmx expandarray.cmx mp3types.cmx pack.cmx mp3read.cmx mp3info.cmx mp3framehuffman.cmx mp3queue.cmx mp3packer.ml

mp3reader: crc.cmx list2.cmx c_part$(OBJ_EXT) mp3types.cmx pack.cmx mp3read.cmx mp3reader.ml
	ocamlopt -o mp3reader$(EXE_EXT) unix.cmxa crc.cmx list2.cmx c_part$(OBJ_EXT) mp3types.cmx pack.cmx mp3read.cmx mp3reader.ml


mp3framehuffman.cmx: mp3types.cmx pack.cmx mp3framehuffman.ml
	ocamlopt -c mp3types.cmx pack.cmx mp3framehuffman.ml



mp3frameutils.cmx: mp3types.cmx pack.cmx mp3framehuffman.cmx mp3frameutils.ml
	ocamlopt -c mp3types.cmx pack.cmx mp3framehuffman.cmx mp3frameutils.ml

mp3framehuffmantest: mp3types.cmx pack.cmx mp3framehuffman.cmx mp3framehuffmantest.ml
	ocamlopt -o mp3framehuffmantest$(EXE_EXT) unix.cmxa str.cmxa mp3types.cmx pack.cmx mp3framehuffman.cmx mp3framehuffmantest.ml



mp3read.cmx: mp3types.cmx pack.cmx mp3read.ml
	ocamlopt -c mp3types.cmx pack.cmx mp3read.ml

mp3queue.cmx: list2.cmx mp3types.cmx pack.cmx mp3read.cmx mp3framehuffman.cmx mp3frameutils.cmx mp3queue.ml
	ocamlopt -c list2.cmx mp3types.cmx pack.cmx mp3read.cmx mp3framehuffman.cmx mp3frameutils.cmx mp3queue.ml

mp3info.cmx: expandarray.cmx mp3types.cmx pack.cmx mp3read.cmx mp3info.ml
	ocamlopt -c expandarray.cmx mp3types.cmx pack.cmx mp3read.cmx mp3info.ml

crc.cmx: crc.ml
	ocamlopt -c crc.ml

pack.cmx: pack.ml
	ocamlopt -c pack.ml

mp3types.cmx: mp3types.ml
	ocamlopt -c mp3types.ml



mp3read_test: mp3types.cmx crc.cmx pack.cmx mp3read.cmx mp3read_test.ml
	ocamlopt -o mp3read_test$(EXE_EXT) str.cmxa mp3types.cmx crc.cmx pack.cmx mp3read.cmx mp3read_test.ml
	mp3read_test

mp3queue_test: mp3types.cmx crc.cmx pack.cmx mp3read.cmx mp3framehuffman.cmx mp3frameutils.cmx mp3queue.cmx mp3queue_test.ml
	ocamlopt -o mp3queue_test$(EXE_EXT) mp3types.cmx crc.cmx pack.cmx mp3read.cmx mp3framehuffman.cmx mp3frameutils.cmx mp3queue.cmx mp3queue_test.ml
	mp3queue_test

mp3info_test: crc.cmx mp3types.cmx pack.cmx mp3read.cmx mp3info.cmx mp3info_test.ml
	ocamlopt -o mp3info_test$(EXE_EXT) crc.cmx mp3types.cmx pack.cmx mp3read.cmx mp3info.cmx mp3info_test.ml
	mp3info_test

mp3queue_concept: mp3queue_concept.ml
	ocamlopt -o mp3queue_concept$(EXE_EXT) mp3queue_concept.ml
	mp3queue_concept


expandarray.cmx: expandarray.ml expandarray.mli
	ocamlopt expandarray.mli
	ocamlopt -c expandarray.ml


## C STUFF ##
c_part$(OBJ_EXT): c_part.c
	ocamlopt c_part.c

#c_module.cmx: c_part.obj c_module.ml
#	ocamlopt -c c_part.obj c_module.ml





## Lists ##
list2.cmx: list2.ml list2.mli
	ocamlopt list2.mli
	ocamlopt -c list2.ml

#list2_test: list2.cmx list2_test.ml
#	ocamlopt -o list2_test list2.cmx list2_test.ml
#	list2_test


clean:
	rm -rf ./mp3packer.exe
	rm -rf ./*.obj
	rm -rf ./*.cmi
	rm -rf ./*.cmx
	rm -rf ./*.cmo
	rm -rf ./*.cma
	rm -rf ./*.cmxa
	rm -rf ./*.lib
	rm -rf ./*.asm

cleanw:
	del /s /q /f .\mp3packer.exe
	del /s /q /f .\*.obj
	del /s /q /f .\*.cmi
	del /s /q /f .\*.cmx
	del /s /q /f .\*.cmo
	del /s /q /f .\*.cma
	del /s /q /f .\*.cmxa
	del /s /q /f .\*.lib
	del /s /q /f .\*.asm

remake: clean mp3packer

