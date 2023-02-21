#======================================================================#

CC     = gcc -Df2cFortran
F77    = gfortran -fPIC
LINKER = gfortran -fPIC

#======================================================================#

DESTINC = /usr/local/include
DESTLIB = /usr/local/lib
DESTBIN = /usr/local/bin

#======================================================================#

STEST    = stest
STOPPING = stopping

ENEWZ    = enewz
RANGEZ   = rangez
RANGE2EZ = range2ez
EOLDZ    = eoldz

HEADER   = enewzlib.h

EXES     = $(STEST) $(STOPPING) \
		$(ENEWZ) $(RANGEZ) $(RANGE2EZ) $(EOLDZ)


ARCHIVE  = libenewzlib.a
SHAREDLIBRARY = libenewzlib.sl
OBJS =	stop.o scoef.o \
		table.o \
		range_z.o \
		eloss_z.o \
		snke_matter.o srho_gas.o string.o \
		enewzsub.o eoldzsub.o rangezsub.o range2ezsub.o \
		enewzsub_etot.o eoldzsub_etot.o rangezsub_etot.o range2ezsub_etot.o
#======================================================================#

all : $(ARCHIVE) $(EXES) 

$(ARCHIVE) : $(OBJS) 
	ar r $(ARCHIVE) $(OBJS)
	ranlib $(ARCHIVE)

$(STEST) : $(STEST).o $(ARCHIVE)
	$(LINKER) -o $(STEST) $(STEST).o $(ARCHIVE)

$(STOPPING) : $(STOPPING).o $(ARCHIVE)
	$(LINKER) -o $(STOPPING) $(STOPPING).o $(ARCHIVE)

$(ENEWZ) : $(ENEWZ).o $(ARCHIVE)
	$(LINKER) -o $(ENEWZ) $(ENEWZ).o $(ARCHIVE)

$(RANGEZ) : $(RANGEZ).o $(ARCHIVE)
	$(LINKER) -o $(RANGEZ) $(RANGEZ).o $(ARCHIVE)

$(RANGE2EZ) : $(RANGE2EZ).o $(ARCHIVE)
	$(LINKER) -o $(RANGE2EZ) $(RANGE2EZ).o $(ARCHIVE)

$(EOLDZ) : $(EOLDZ).o $(ARCHIVE)
	$(LINKER) -o $(EOLDZ) $(EOLDZ).o $(ARCHIVE)

.f.o :
	$(F77) -c -O3 -o $@ $<

install : $(EXES) $(HEADER) $(ARCHIVE)
	- cp -f $(EXES)     $(DESTBIN)	
	- cp -f $(ARCHIVE)  $(DESTLIB)
	- cp -f $(HEADER)   $(DESTINC)

uninstall :
	- rm -f $(DESTBIN)/$(STEST)
	- rm -f $(DESTBIN)/$(STOPPING)
	- rm -f $(DESTBIN)/$(ENEWZ)
	- rm -f $(DESTBIN)/$(ENEWZN)
	- rm -f $(DESTBIN)/$(RANGEZ)
	- rm -f $(DESTBIN)/$(RANGE2EZ)
	- rm -f $(DESTBIN)/$(EOLDZ)
	- rm -f $(DESTLIB)/$(ARCHIVE)
	- rm -f $(DESTINC)/$(HEADER)

clean :
	- rm -f $(OBJS)
	- rm -f $(STEST).o
	- rm -f $(STOPPING).o
	- rm -f $(ENEWZ).o
	- rm -f $(ENEWZN).o
	- rm -f $(RANGEZ).o
	- rm -f $(RANGE2EZ).o
	- rm -f $(EOLDZ).o
	- rm -f $(ARCHIVE).o
	- rm -f $(HEADER).o
	- rm -f $(EXES)
	- rm -f $(ARCHIVE)
	- rm -f $(SHAREDLIBRARY)

#======================================================================#
