all: 20170717_latest.o 20170717_latest_raw.o

copy: 20170717_latest.o
	cp 20170717_latest.s ../../../../openssl/crypto/evp/e_aes_cbc_hmac_sha1_decrypt.s
	cp 20170717_latest.h ../../../../openssl/crypto/evp/fact_aes_cbc_hmac_sha1_decrypt.h

.PHONY: all copy

.PRECIOUS: 20170717_latest.h 20170717_latest_raw.h

20170717_latest.o: 20170717_latest.fact Makefile ../../../fact.byte
	../../../fact.byte -pseudocode -generate-header -ast-out -core-ir-out -llvm-out -debug -opt O2 20170717_latest.fact

20170717_latest_raw.o: 20170717_latest_raw.fact Makefile ../../../fact.byte
	../../../fact.byte -pseudocode -generate-header -ast-out -core-ir-out -llvm-out -debug -opt O2 20170717_latest_raw.fact


clean:
	rm -f *.s *.o *.bc 20170717_latest_raw.h *.pseudo.fact *.ll *.bin *.res *.ml
