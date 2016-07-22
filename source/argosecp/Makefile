#
# General makefile to build the various pieces of the argos modules argos1e and cnvrt1e
#

include ../../makefile.h

MODULES = argos1e cnvrt1e

all : portfirst
	@for dir in $(MODULES); do \
		$(MAKE) -C $$dir ; \
	done

portfirst : already_ported
	test -f ported.$(TARGET) || \
	((touch ported.$(TARGET) ; \
	cp cmdc/binaries/cmdc.$(ARGOSMAC).x cmdc.x ; \
	./port $(ARGOSMAC) normal colib/colib/*.f ; \
	./port $(ARGOSMAC) normal argos1e/*.f ; \
	./port $(ARGOSMAC) normal cnvrt1e/*.f) || exit 1)

already_ported :
	test -f ported.$(TARGET) || rm -f ported.* *.x

clean :
	rm -f ported.* cmdc.x *TMP bummer
