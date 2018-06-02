;;; gnome-c-tests.el --- tests for gnome-c-style -*- lexical-binding: t; -*-
;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@gnu.org>
;; Keywords: GNOME, C, coding style

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'gnome-c-align)
(require 'gnome-c-snippet)

(defconst gnome-c-test-program-1 "\
GGpgCtx *g_gpg_ctx_new (GError **error);

typedef void (*GGpgProgressCallback) (gpointer user_data,
                                      const gchar *what,
                                      gint type,
                                      gint current,
                                      gint total);

void g_gpg_ctx_set_progress_callback (GGpgCtx *ctx,
                                      GGpgProgressCallback callback,
                                      gpointer user_data,
                                      GDestroyNotify destroy_data);
void g_gpg_ctx_add_signer (GGpgCtx *ctx, GGpgKey *key);
guint g_gpg_ctx_get_n_signers (GGpgCtx *ctx);
GGpgKey *g_gpg_ctx_get_signer (GGpgCtx *ctx, guint index);
void g_gpg_ctx_clear_signers (GGpgCtx *ctx);
")

(defconst gnome-c-test-program-1-aligned "\
GGpgCtx *g_gpg_ctx_new                   (GError              **error);

typedef void (*GGpgProgressCallback) (gpointer user_data,
                                      const gchar *what,
                                      gint type,
                                      gint current,
                                      gint total);

void     g_gpg_ctx_set_progress_callback (GGpgCtx              *ctx,
                                          GGpgProgressCallback  callback,
                                          gpointer              user_data,
                                          GDestroyNotify        destroy_data);
void     g_gpg_ctx_add_signer            (GGpgCtx              *ctx,
                                          GGpgKey              *key);
guint    g_gpg_ctx_get_n_signers         (GGpgCtx              *ctx);
GGpgKey *g_gpg_ctx_get_signer            (GGpgCtx              *ctx,
                                          guint                 index);
void     g_gpg_ctx_clear_signers         (GGpgCtx              *ctx);
")

(defconst gnome-c-test-program-2 "\
GDK_AVAILABLE_IN_3_16
const gchar **          gtk_widget_list_action_prefixes (GtkWidget             *widget);
")

(defconst gnome-c-test-program-3 "\
  /* overridable methods */
  void       (*set_property)            (GObject        *object,
                                         guint           property_id,
                                         const GValue   *value,
                                         GParamSpec     *pspec);
  void       (*get_property)            (GObject        *object,
                                         guint           property_id,
                                         GValue         *value,
                                         GParamSpec     *pspec);
")

(defconst gnome-c-test-program-4 "\
FOO_AVAILABLE_IN_ALL
int foo (struct foo ***a, int b, ...) G_GNUC_CONST;
")

(defconst gnome-c-test-program-4-aligned "\
FOO_AVAILABLE_IN_ALL
int foo (struct foo ***a,
         int           b,
         ...) G_GNUC_CONST;
")

(defconst gnome-c-test-program-5 "\
int  * bar (const char * const *  * a, int b);
")

(defconst gnome-c-test-program-5-aligned "\
int *bar (const char * const **a,
          int                  b);
")

(defconst gnome-c-test-program-6 "\
int foo (char **a, int b);
type_1234567890 bar (char a, int b);
int identifier_1234567890 (double a, double b);
")

(defconst gnome-c-test-program-6-aligned-1 "\
int             foo
                (char **a,
                 int    b);
type_1234567890 bar
                (char   a,
                 int    b);
int             identifier_1234567890
                (double a,
                 double b);
")

(defconst gnome-c-test-program-6-aligned-2 "\
int             foo (char **a,
                     int    b);
type_1234567890 bar (char   a,
                     int    b);
int             identifier_1234567890
                    (double a,
                     double b);
")

(defconst gnome-c-test-program-7 "\
G_DECLARE_FINAL_TYPE (GGpgEngineInfo, g_gpg_engine_info, G_GPG, ENGINE_INFO,
                      GObject)
")

(ert-deftest gnome-c-test-align--guess-optimal-columns ()
  "Tests the `gnome-c-align--guess-optimal-columns'."
  (with-temp-buffer
    (insert gnome-c-test-program-1)
    (c-mode)
    (let* (gnome-c-align-max-column
	   (columns
	    (gnome-c-align--guess-optimal-columns (point-min) (point-max))))
      (should (= (cdr (assq 'identifier-start-column columns)) 9))
      (should (= (cdr (assq 'arglist-start-column columns)) 41))
      (should (= (cdr (assq 'arglist-identifier-start-column columns)) 64)))))

(ert-deftest gnome-c-test-align-region ()
  "Tests the `gnome-c-align-decls-region'."
  (with-temp-buffer
    (insert gnome-c-test-program-1)
    (c-mode)
    (let (gnome-c-align-max-column)
      (gnome-c-align-guess-optimal-columns (point-min) (point-max))
      (gnome-c-align-decls-region (point-min) (point-max)))
    (should (equal (buffer-string) gnome-c-test-program-1-aligned))))

(ert-deftest gnome-c-test-align-region-2 ()
  "Tests the `gnome-c-align-decls-region'."
  (with-temp-buffer
    (insert gnome-c-test-program-4)
    (c-mode)
    (let (gnome-c-align-max-column)
      (gnome-c-align-guess-optimal-columns (point-min) (point-max))
      (gnome-c-align-decls-region (point-min) (point-max)))
    (should (equal (buffer-string) gnome-c-test-program-4-aligned))))

(ert-deftest gnome-c-test-align-region-3 ()
  "Tests the `gnome-c-align-decls-region'."
  (with-temp-buffer
    (insert gnome-c-test-program-5)
    (c-mode)
    (let (gnome-c-align-max-column)
      (gnome-c-align-guess-optimal-columns (point-min) (point-max))
      (gnome-c-align-decls-region (point-min) (point-max)))
    (should (equal (buffer-string) gnome-c-test-program-5-aligned))))

(ert-deftest gnome-c-test-align-region-4 ()
  "Tests the `gnome-c-align-decls-region', with max columns set."
  (with-temp-buffer
    (insert gnome-c-test-program-6)
    (c-mode)
    (let ((gnome-c-align-max-column 20))
      (gnome-c-align-guess-optimal-columns (point-min) (point-max))
      (gnome-c-align-decls-region (point-min) (point-max)))
    (should (equal (buffer-string) gnome-c-test-program-6-aligned-1))))

(ert-deftest gnome-c-test-align-region-5 ()
  "Tests the `gnome-c-align-decls-region', with max columns set."
  (with-temp-buffer
    (insert gnome-c-test-program-6)
    (c-mode)
    (let ((gnome-c-align-max-column 30))
      (gnome-c-align-guess-optimal-columns (point-min) (point-max))
      (gnome-c-align-decls-region (point-min) (point-max)))
    (should (equal (buffer-string) gnome-c-test-program-6-aligned-2))))

(ert-deftest gnome-c-test-align-guess-columns-1 ()
  "Tests the `gnome-c-align-guess-columns'."
  (with-temp-buffer
    (insert gnome-c-test-program-2)
    (c-mode)
    (let (gnome-c-align-max-column)
      (gnome-c-align-guess-columns (point-min) (point-max)))
    (should (= gnome-c-align-identifier-start-column 24))
    (should (= gnome-c-align-arglist-start-column 56))
    (should (= gnome-c-align-arglist-identifier-start-column 80))))

(ert-deftest gnome-c-test-align-guess-columns-2 ()
  "Tests the `gnome-c-align-guess-columns'."
  (with-temp-buffer
    (insert gnome-c-test-program-3)
    (c-mode)
    (let (gnome-c-align-max-column)
      (gnome-c-align-guess-columns (point-min) (point-max)))
    (should (= gnome-c-align-identifier-start-column 13))
    (should (= gnome-c-align-arglist-start-column 40))
    (should (= gnome-c-align-arglist-identifier-start-column 57))))

(ert-deftest gnome-c-test-snippet-guess-name-from-declaration ()
  "Tests the `gnome-c-snippet--guess-name-from-declaration'."
  (with-temp-buffer
    (insert gnome-c-test-program-7)
    (c-mode)
    (setq buffer-file-name "gpgme-glib.h")
    (let ((package (gnome-c-snippet--guess-name-from-declaration 'package))
	  (class (gnome-c-snippet--guess-name-from-declaration 'class))
	  (parent-package
	   (gnome-c-snippet--guess-name-from-declaration 'parent-package))
	  (parent-class
	   (gnome-c-snippet--guess-name-from-declaration 'parent-class)))
      (should (equal package '("G" "Gpg")))
      (should (equal class '("Engine" "Info")))
      (should (equal parent-package '("G")))
      (should (equal parent-class '("Object"))))))

(ert-deftest gnome-c-test-snippet-guess-name-from-declaration-2 ()
  "Tests the `gnome-c-snippet--guess-name-from-declaration'."
  (let (buffer)
    (unwind-protect
	(progn
	  (setq buffer (generate-new-buffer "header"))
	  (with-current-buffer buffer
	    (insert gnome-c-test-program-7)
	    (c-mode)
	    (setq buffer-file-name "gpgme-glib.h"))
	  (with-temp-buffer
	    (c-mode)
	    (setq buffer-file-name "gpgme-glib.c")
	    (let ((package
		   (gnome-c-snippet--guess-name-from-declaration 'package))
		  (class
		   (gnome-c-snippet--guess-name-from-declaration 'class))
		  (parent-package
		   (gnome-c-snippet--guess-name-from-declaration
		    'parent-package))
		  (parent-class
		   (gnome-c-snippet--guess-name-from-declaration
		    'parent-class)))
	      (should (equal package '("G" "Gpg")))
	      (should (equal class '("Engine" "Info")))
	      (should (equal parent-package '("G")))
	      (should (equal parent-class '("Object"))))))
      (kill-buffer buffer))))

(ert-deftest gnome-c-test-snippet-guess-name-from-file-name ()
  "Tests the `gnome-c-snippet--guess-name-from-file-name'"
  (with-temp-buffer
    (c-mode)
    (setq buffer-file-name "g-gpg-engine-info.c")
    (let ((package
	   (gnome-c-snippet--guess-name-from-file-name 'package))
	  (class
	   (gnome-c-snippet--guess-name-from-file-name 'class))
	  (parent-package
	   (gnome-c-snippet--guess-name-from-file-name 'parent-package))
	  (parent-class
	   (gnome-c-snippet--guess-name-from-file-name 'parent-class)))
      (should (equal package '("G")))
      (should (equal class '("Gpg" "Engine" "Info")))
      (should (equal parent-package nil))
      (should (equal parent-class nil)))))
