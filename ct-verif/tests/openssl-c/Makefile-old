export WD =${PWD}

verify:
	ENTRYPOINTS=ssl3_cbc_remove_padding_wrapper EXAMPLE=ssl3_cbc_remove_padding_wrapper.c make -C ../../bin verify

verify_bug:
	ENTRYPOINTS=ssl3_cbc_remove_padding_wrapper EXAMPLE=ssl3_cbc_remove_padding_wrapper_error.c make -C ../../bin verify

verify2:
	ENTRYPOINTS=tls1_cbc_remove_padding_wrapper EXAMPLE=tls1_cbc_remove_padding_wrapper.c make -C ../../bin verify

verify3:
	ENTRYPOINTS=ssl3_cbc_copy_mac_wrapper EXAMPLE=ssl3_cbc_copy_mac_wrapper.c make -C ../../bin verify

verify4:
	ENTRYPOINTS=ssl3_cbc_digest_record_wrapper EXAMPLE=ssl3_cbc_digest_record_wrapper.c make -C ../../bin verify

clean:
	rm -f ssl3_cbc_remove_padding_wrapper.bpl ssl3_cbc_remove_padding_wrapper_error.bpl tls1_cbc_remove_padding_wrapper.bpl ssl3_cbc_digest_record_wrapper.bpl
