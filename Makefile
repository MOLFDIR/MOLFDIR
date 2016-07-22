########################################################################
#                                                                      #
#  Main makefile for the MOLFDIR program package                       #
#                                                                      #
#  Written by:                                                         #
#                                                                      #
#  P.J.C. Aerts                                                        #
#  O. Visser                                                           #
#  L. Visscher                                                         #
#  H. Merenga                                                          #
#  W.A. de Jong                                                        #
#  M. Pernpointer                                                      #
#                                                                      #
#  University of Groningen                                             #
#  Department of Chemistry                                             #
#  Chemical Physics (Theoretical Chemistry Group)                      #
#  Nijenborg 4                                                         #
#  9747 AG   GRONINGEN                                                 #
#  The Netherlands                                                     #
#                                                                      #
#  Current e-mail address for information: bert.dejong@pnl.gov         #
#                                                                      #
########################################################################

include makefile.h

MODULES_PAR = molfdir relonel reltwel mfdscf rotran tmoone relccsd

MODULES_SER = molfdir relonel reltwel mfdscf rotran tmoone goscip dirrci relccsd propan caldens genbas prtran argosecp

ifdef PARALLEL
  building = parallel
else
  building = serial
endif

all : directories $(building)

serial :
	@for dir in $(MODULES_SER); do \
		rm -f source/$$dir/*.o ; \
		$(MAKE) -C source/$$dir ; \
	done

parallel :
	@for dir in $(MODULES_PAR); do \
		rm -f source/$$dir/*.o ; \
		$(MAKE) -C source/$$dir ; \
	done

directories:
	test -d lib/$(TARGET) || mkdir -p lib/$(TARGET)
	test -d bin/$(TARGET) || mkdir -p bin/$(TARGET)

clean    :
	rm -f lib/$(TARGET)/*.a bin/$(TARGET)/* 

realclean :
	rm -rf lib/$(TARGET) bin/$(TARGET)
	$(MAKE) clean -C source/argosecp

