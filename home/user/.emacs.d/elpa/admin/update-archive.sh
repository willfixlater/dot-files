#!/bin/sh
# TODO: Author
# TODO: License
##
# Usage: update-archive.sh [options]
#
# Update the archive.  This involves several steps,
# some performed in the "buildir" (cwd at invocation),
# which should be a sibling of the elpa/ dir.
#
# Options:
#  --announce EMAIL   -- also send announcement to EMAIL address
#  --batch            -- write std{out,err} to make.log (in buildir)
#
# Preconditions:
# - Installed software: /usr/sbin/sendmail, git, rsync, make, emacs.
# - Internet connection (for ‘git pull’, sending mail).
# - There should be a sibling directory of elpa/: staging/.
#
# Gory operation details follow (for maintainers).
#
# * Flow (see Cahoots for ‘[N]’)
#
# First, in sibling dir ../elpa, fetch changes (via ‘git pull’),
# set up and update external packages[1], and check copyrights[2].
# Signal error if any sub-step fails.
#
# Back in $buildir, snapshot ../elpa/packages/* as packages/*,
# excluding some files such as ChangeLog, .git/, *.elc, and so on;
# refresh the ChangeLog files[3]; wipe and recreate dir archive/[4].
# Some of these sub-steps signal error on failure.
#
# In $buildir/archive/, make emacs-packages-latest.tgz from subdir
# $buildir/archive/packages/ (unpacking creates ./package/*).
#
# In parent dir of $buildir, ensure existence of directories
# staging/packages/ and staging-old/ -- that is, $buildir has
# two sibling dirs staging/ and staging-old/ -- and then snapshot
# staging/* to staging-old/* [which all kind of implies that
# staging/ is persistent (is not a temporary dir), right?  --ttn].
# To populate staging/packages/ (here, called ‘dst’), iterate over
# $buildir/archive/packages/* (here, called ‘src’) and do one of:
#  (a) for */archive-contents, *-readme.txt, mv directly
#  (b) if $dst/PV already exists, delete $src/PV
#  (c) mv $src/PV $dst/PV and announce it (if ‘--announce’)
# Afterwards, mv $buildir/archive/emacs-packages-latest.tgz to staging/
# and delete $buildir/archive/ (and all its subdirs).
#
# Lastly, in ../staging/packages/, make the HTML and readme.txt files[5].
#
# * Cahoots
#
# These programs are in cahoots w/ update-archive.sh -- here,
# "lisp" means Emacs Lisp function found in archive-contents.el,
# and "make" means makefile target found in ../GNUmakefile.
#  [1] lisp ‘archive-add/remove/update-externals’
#  [2] make ‘check_copyrights’
#  [3] lisp ‘archive-prepare-packages’
#  [4] make ‘archive-full’
#  [5] lisp ‘batch-html-make-index’
#
# * Miscellaneous
#
# "Signal error" means report an error and exit w/ status 1.
# If invoked w/ ‘--batch’, reporting means mailing the log file
# to emacs-elpa-diffs (a gnu dot org mailing list) using the
# error message as title.  Otherwise, reporting means displaying
# the error message to stdout.
#
# Mail sender (From) is "ELPA update" w/ bogus address.
#
# "Snapshot" means use ‘rsync -av’ (plus other options).
##
# [NB: I inferred these from VCS logs.  Corrections welcome!  --ttn]
#        0.x  -- release from the previous VCS
#        1.0  -- initial release from this VCS (Git)
#        1.1  -- add ‘--announce EMAIL’ support
#        1.2  -- fix externals maintenance
#        1.3  -- fix ‘--announce EMAIL’ support
#        1.4  -- use sendmail(8) and rsync(1)
#        1.5  -- make staging operations less brittle
#        1.6  -- support ‘--help’, ‘--version’
#        1.7  -- fix DANGEROUS bug; make less noisy; name bash explicitly
#        1.8  -- revert "name bash explicitly"
version='1.8'
# If $0 is a symlink, `dirname $0`/hv.sh might not be available,
# and even if it IS available, how can we be sure it's bonafide?
test -L $0 || { hv=`dirname $0`/hv.sh ; test -r $hv && . $hv ; }

# TODO: (here) Validate args.

set -x

makelog=""
buildir="$(pwd)"

announce=no
a_email="" #info-gnu-emacs@gnu.org

export LANG=C
while [ $# -gt 0 ]; do
    case "$1" in
        "--announce") announce=yes; a_email="$2"; shift ;;
        "--batch")
            makelog="$(pwd)/make.log"
            exec >"$makelog" 2>&1
            ;;
    esac
    shift
done

send_mail () {
    to="$1"; shift
    title="$*"
    (cat <<ENDDOC
From: ELPA update <do.not.reply@elpa.gnu.org>
To: $to
Subject: $title

ENDDOC
     cat -) | /usr/sbin/sendmail "$to"
}

# Send an email to warn about a problem.
signal_error () {
    title="$*"
    if [ "" = "$makelog" ]; then
        echo "Error: $title"
    else
        send_mail "emacs-elpa-diffs@gnu.org" "$title" <"$makelog"
    fi
    exit 1
}

announce_new () {
    if [ "yes" != "$announce" ]; then return; fi
    pv="$1"
    pkg="$(echo "$pv" | sed -e 's/^\(.*\)-\([^-]*\)\.[^-.]*$/\1/')"
    ver="$(echo "$pv" | sed -e 's/^\(.*\)-\([^-]*\)\.[^-.]*$/\2/')"
    if [ -z "$pkg" ] || [ -z "$ver" ]; then signal_error "bad PKG-VER: $pv"; fi
    send_mail "$a_email" "[GNU ELPA] $pkg version $ver" <<ENDDOC
Version $ver of GNU ELPA package $pkg has just been released.
You can now find it in M-x package-list RET.

More at http://elpa.gnu.org/packages/$pkg.html
ENDDOC
}

cd ../elpa

# Fetch changes.
git pull || signal_error "git pull failed"

# Remember we're inside the "elpa" branch which we don't want to trust,
# So always refer to the makefile and admins files from $builddir".

# Setup and update externals.
emacs --batch -l "$buildir/admin/archive-contents.el" \
      -f archive-add/remove/update-externals

make -f "$buildir/GNUmakefile" check_copyrights ||
    signal_error "check_copyright failed"

cd "$buildir"

rsync -av --delete                    \
      --exclude=ChangeLog             \
      --exclude=.git                  \
      --exclude='*.elc'               \
      --exclude='*~'                  \
      --exclude='/*/*/*-autoloads.el' \
      ../elpa/packages ./

# Refresh the ChangeLog files.  This needs to be done in
# the source tree, because it needs the VCS data!
emacs -batch -l admin/archive-contents.el \
      -eval '(archive-prepare-packages "../elpa")'


rm -rf archive                  # In case there's one left over!
make archive-full || {
    signal_error "make archive-full failed"
}
latest="emacs-packages-latest.tgz"
(cd archive
 GZIP=--best tar zcf "$latest" packages)
(cd ../
 mkdir -p staging/packages
 # Not sure why we have `staging-old', but let's keep it for now.
 mkdir -p staging-old
 rsync -av --inplace --delete staging/. staging-old/.
 # Move new files into place but don't throw out old package versions.
 for f in $buildir/archive/packages/*; do
     # PKG-VER
     pv=$(basename "$f")
     dst="staging/packages/$pv"
     # Actually, let's never overwrite an existing version.  So changes can
     # be installed without causing a new package to be built until the
     # version field is changed.  Some files need to be excluded from the
     # "immutable" policy, most importantly "archive-contents"
     # and "*-readme.txt".
     case $dst in
         */archive-contents | *-readme.txt ) mv "$f" "$dst" ;;
         * ) if [ -r "$dst" ]
             then rm "$f"
             else
                 mv "$f" "$dst"
                 # FIXME: Add a tag to remember the precise code used.
                 announce_new "$pv"
             fi ;;
     esac
 done
 mv $buildir/archive/"$latest" staging/
 rm -rf $buildir/archive)

# Make the HTML and readme.txt files.
(cd ../staging/packages
 emacs --batch -l $buildir/admin/archive-contents.el \
       --eval '(batch-html-make-index)')

# update-archive.sh ends here
