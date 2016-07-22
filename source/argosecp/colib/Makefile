#
# General makefile to build the libraries colib and special used
# by the argos modules argos1e and cnvrt1e
#

include ../../../makefile.h

MODULES = colib special  

all : 
	@for dir in $(MODULES); do \
		$(MAKE) -C $$dir ; \
	done

