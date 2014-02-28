TOPFILES=../sandbox

test-compile:
	@bash ./test-compile.sh
	@rm build/*.o

clean:
	@rm -rf build
	@for i in `find . -iname *.lusi`; do grep generated $$i > /dev/null; if [ $$? -eq 0 ]; then rm $$i; fi; done

distclean: clean
	@rm -rf report*

