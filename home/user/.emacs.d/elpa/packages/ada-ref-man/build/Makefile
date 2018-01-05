# Compilation of the Scribe formatter written for the Ada Reference
# Manual, translating .ms[ms] files into various formats. Also build
# tar.gz for the web page.

# Copyright (c) 2010, 2013 Stephen Leake <stephen_leake@stephe-leake.org>
# Copyright (c) 2013       Nicolas Boulenguez <nicolas@debian.org>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# Specific targets generate every $document$year.$format combination.
# Ex: make arm2012.pdf
AVAILABLE_DOCUMENTS := aarm arm
AVAILABLE_YEARS     := 2005 2012
AVAILABLE_FORMATS   := html info texinfo txt pdf

# 'all' builds all versions except pdf; to build a subset, override these on the command line.
DOCUMENTS := $(AVAILABLE_DOCUMENTS)
YEARS     := $(AVAILABLE_YEARS)
FORMATS   := $(filter-out pdf texinfo, $(AVAILABLE_FORMATS))
# Texinfo is an intermediate, and is built as necessary.
# Pdf requires installation of many huge formatting tools; we assume they are not installed.

# see ../README.txt for list of things to change when Scribe or Ada source changes.
#
# Cygwin texi2dvi (GNU Texinfo 4.13) 1.135 fails silently!

# Debian standard build flags for Ada projects
BUILDER_OPTIONS := -k
ADAFLAGS        :=
LDFLAGS         :=
# arm_info.gpr sets its own flags, then appends these variables so
# that they take precedence.  The intent is that you may configure the
# build with a simple variable override.

.PHONY: all clean publish

all: $(foreach d,$(DOCUMENTS),\
     $(foreach y,$(YEARS),\
     $(foreach f,$(FORMATS),\
       $(d)$(y).$(f))))

# We have been unable to get rid of all the section ref violations, so we
# specify --no-validate. Delete that to see the errors.
TEXI_INFO_OPTS := --no-split --no-number-sections --no-validate
TEXI_PDF_OPTS := --quiet

SOURCE_DIR_2005 := ../source_2005
SOURCE_DIR_2012 := ../source_2012

# See progs/command.txt.
CHANGES      := New-Only
# See arm_form.exe help for possible values
VERSION_2005 := 2
VERSION_2012 := 4

# Mapping from document acronyms to Randy's file names
MASTER_arm  := rm.msm
MASTER_aarm := aarm.msm

# Rough dependency, but Scribe files should not change often.
SOURCES_2005 := $(wildcard $(SOURCE_DIR_2005)/*.ms[ms])
SOURCES_2012 := $(wildcard $(SOURCE_DIR_2012)/*.ms[ms])
define doc_year_build

  # Non-unique intermediate paths would create a race condition, so we
  # store the temporary result into the versioned source directory.
  $(d)$(y).texinfo: arm_form.exe $(SOURCES_$(y))
	cd $(SOURCE_DIR_$(y)); \
        $(CURDIR)/$$< $(MASTER_$(d)) info $(CHANGES) $(VERSION_$(y)) $(VERSION_$(y)) ./; \
        mv `sed -ne '/^@FilePrefix{\(.*\)}/{s//\L\1/p;q}' $(MASTER_$(d))`.texinfo $(CURDIR)/$$@
  clean::
	rm -f $(d)$(y).texinfo

  $(d)$(y).html: arm_form.exe $(SOURCES_$(y))
	rm -f -r $$@
	mkdir --parents $$@
	cd $(SOURCE_DIR_$(y)); \
        $(CURDIR)/$$< $(MASTER_$(d)) HTML $(CHANGES) $(VERSION_$(y)) $(VERSION_$(y)) $(CURDIR)/$$@/
  clean::
	rm -f -r $(d)$(y).html

  $(d)$(y).txt: arm_form.exe $(SOURCES_$(y))
	rm -f -r $$@
	mkdir --parents $$@
	cd $(SOURCE_DIR_$(y)); \
        $(CURDIR)/$$< $(MASTER_$(d)) Text $(CHANGES) $(VERSION_$(y)) $(VERSION_$(y)) $(CURDIR)/$$@/
  clean::
	rm -f -r $(d)$(y).txt

# texi2any is from texinfo 5
  $(d)$(y).info: $(d)$(y).texinfo
	texi2any --info $(TEXI_INFO_OPTS) $$< -o $$@
  clean::
	rm -f $(d)$(y).info

  $(d)$(y).pdf: $(d)$(y).texinfo
	texi2any --pdf $(TEXI_PDF_OPTS) $$< -o $$@
  clean::
	rm -f $(d)$(y).log $(d)$(y).pdf $(d)$(y).toc

endef
$(foreach d,$(AVAILABLE_DOCUMENTS),\
$(foreach y,$(AVAILABLE_YEARS),\
$(eval $(doc_year_build))))

arm_form.exe : force
	gnatmake -p $(BUILDER_OPTIONS) -P arm_info.gpr $(foreach var,ADAFLAGS LDFLAGS,"-X$(var)=$($(var))")
clean::
	rm -f arm_form.exe
	rm -f -r objects

trace :
	addr2line -e arm_form.exe 0x5c12e7 0x5c598e 0x43e05b 0x446c5b 0x448369 0x545205 0x4e9221 0x4e3b1a 0x5831a1 0x5ad295 0x5adee9 0x4cb3f5 0x401850 0x401235 0x401286 0x7c817075

# year is latest standard; bump trailing digit for any change.
ZIP_VERSION := $(lastword $(AVAILABLE_YEARS)).3

INFO_ARCHIVE := arm_info-$(ZIP_VERSION).tar.gz
SOURCE_ARCHIVE  := arm_info-$(ZIP_VERSION)-src.tar.gz
publish: $(INFO_ARCHIVE) $(SOURCE_ARCHIVE)

# --no-name eliminates time stamps, which helps make the tarball reproducible
# --best gives higher compression than the default; we can afford the time here.
GZIP := --no-name --best

$(INFO_ARCHIVE): $(foreach d,$(DOCUMENTS),\
                 $(foreach y,$(YEARS),\
                 $(d)$(y).info))
	tar cf - $^ | gzip $(GZIP) -c > $@
clean::
	rm -f $(INFO_ARCHIVE)

ZIP_EXCLUDES := *~ *.dvi *.gz *.info *.log *.pdf *.ps *.texinfo *.toc \
  *.stamp *.zip arm_form.exe _MTN .mtn-ignore .dvc-exclude aarm2005.html \
  aarm2005.txt aarm2012.html aarm2012.txt arm2005.html arm2005.txt arm2012.html \
  arm2012.txt objects
$(SOURCE_ARCHIVE):
	tar cf - -C ../.. $(foreach e,$(ZIP_EXCLUDES),--exclude="$(e)") org.adaic.arm_form | gzip $(GZIP) -c > $@
clean::
	rm -f $(SOURCE_ARCHIVE)

update_upstream : source_ada.clean         source_ada.stamp
update_upstream : source_scribe_2005.clean source_scribe_2005.stamp
update_upstream : source_scribe_2012.clean source_scribe_2012.stamp

# download.py does not convert to unix line endings here to minimize
# the diff with upstream; the Ada compiler can handle DOS line endings
# on Unix. download.py does convert file names to lower case here, for
# consistency.
source_ada.stamp :
	rm -rf ../../org.adaic.arm_form.upstream/progs
	cd ../../org.adaic.arm_form.upstream; python3 ../org.adaic.arm_form/build/download.py progs
	touch source_ada.stamp

source_ada.clean :
	rm -f source_ada.stamp

# This retrieves the CVS tag for the year given on the command line
# commit to mtn: (dvc-status "../../org.adaic.arm_form.upstream")
#
# download.py converts to unix line endings here, because the scribe
# processor insists on uniform formatting; this works for both Windows
# and Debian. download.py also converts all file names to lowercase,
# and arm_form does as well; upstream only runs on Windows, which
# ignores case in filenames, so the file names in the sources are not
# consistently cased.
source_scribe_%.stamp :
	rm -rf ../../org.adaic.arm_form.upstream/source_$*
	cd ../../org.adaic.arm_form.upstream; python3 ../org.adaic.arm_form/build/download.py $*
	mv ../../org.adaic.arm_form.upstream/source ../../org.adaic.arm_form.upstream/source_$*
	touch source_scribe_$*.stamp

source_scribe_%.clean :
	rm -f source_scribe_$*.stamp

# These sources do not match the correpsonding CVS tag, although they are close.
ARM_SRC.ZIP ARM_FORM.ZIP ARM_FSRC.ZIP 2005-SRC.zip 2012-SRC.zip :
	wget http://www.ada-auth.org/arm-files/$@

# delete everything back to mtn checkout
#
# Note that additional 'clean' targets are specified after every
# target that builds something.
clean ::
	rm -f *.stamp *.zip

info-clean :
	rm -f *.info*

.PHONY : force

VPATH = ../source_2005 ../source_2012 ../progs

#Local Variables:
#eval: (delete '("\\.mss\\'" . scribe-mode) auto-mode-alist)
#eval: (ada-parse-prj-file "arm_info.prj")
#eval: (ada-select-prj-file "arm_info.prj")
#End:
# end of file
