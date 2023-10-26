build_dir = build
exe = instant
bnfc_path = /home/students/inf/PUBLIC/MRJP/bin/bnfc

$(exe):
	rm -rf $(build_dir)
	mkdir -p $(build_dir)
	cp src/Instant.cf $(build_dir)/
	cd $(build_dir); $(bnfc_path) -m Instant.cf; make
	cd $(build_dir); cp ../src/*.hs .
	cd $(build_dir); ghc --make -package mtl InstantLlvm.hs -o ../insc_llvm
	cd $(build_dir); ghc --make -package mtl InstantJvm.hs -o ../insc_jvm

clean:
	rm -rf $(build_dir) $(exe) insc_llvm insc_jvm examples/*.ll examples/*.bc examples/*.j examples/*.class

.PHONY: clean