#!/bin/sh
# TODO: Author
# TODO: License
##
# Usage: org-sync.sh DOWNLOAD-DIR ADMIN-DIR
#
# This script first determines the latest org-mode tarball
# by screen-scraping <http://orgmode.org/elpa/>.  Next, it
# changes directory to DOWNLOAD-DIR and fetches the tarball.
# If successful, it passes control to the Emacs Lisp program
# ADMIN-DIR/org-sync.el (func ‘org-sync’) to finish the job.
#
# Preconditions:
# - Installed software: perl, wget, emacs.
# - Internet connection (i.e., can access orgmode.org over HTTP).
# - DOWNLOAD-DIR exists and rw.
# - ADMIN-DIR exists and readable.
##
# [NB: I inferred these from VCS logs.  Corrections welcome!  --ttn]
#        0.x  -- release from the previous VCS
#        1.0  -- initial release from this VCS (Git)
#        1.1  -- support ‘--help’, ‘--version’
#        1.2  -- no longer require curl; performance tweak
version='1.2'
# If $0 is a symlink, `dirname $0`/hv.sh might not be available,
# and even if it IS available, how can we be sure it's bonafide?
test -L $0 || { hv=`dirname $0`/hv.sh ; test -r $hv && . $hv ; }

# TODO: (here) Validate args.

PATH="/bin:/usr/bin:/usr/local/bin:${PATH}"

pkgname=`wget -q http://orgmode.org/elpa/ -O-|perl -ne '$n = $1 if (m/(org-\d{8}\.tar)/ && $1 gt $n); END { print "$n" }'`

cd $1
wget -q http://orgmode.org/elpa/${pkgname} -O ${pkgname}-tmp
if [ -f ${pkgname}-tmp ]; then
    rm -f org*.tar
    mv ${pkgname}-tmp ${pkgname} && \
    emacs -batch -l $2/org-synch.el --eval "(org-synch \"${pkgname}\")"
fi

# org-synch.sh ends here
