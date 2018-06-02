;; Copyright (C) 2015-2016 Free Software Foundation, Inc.

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

;;; Commentary:

;;; Code:

(require 'ert)
(require 'dbus-codegen)

(ert-deftest dbus-codegen--read-signature ()
  (should (equal '(1 . :int32) (dbus-codegen--read-signature "i" 0)))
  (should-error (dbus-codegen--read-signature "a" 0))
  (should (equal '(2 :array :int32) (dbus-codegen--read-signature "ai" 0)))
  (should (equal '(4 :array (:dict-entry :string :int32))
		 (dbus-codegen--read-signature "a{si}" 0)))
  (should (equal '(4 :array (:dict-entry :string (:variant)))
		 (dbus-codegen--read-signature "a{sv}" 0)))
  (should-error (dbus-codegen--read-signature "a{sii}" 0))
  (should (equal '(5 :array (:struct (:string :int32 :int32)))
		 (dbus-codegen--read-signature "a(sii)" 0))))

(ert-deftest dbus-codegen--object-path-p ()
  (should (dbus-codegen--object-path-p "/"))
  (should (not (dbus-codegen--object-path-p "//")))
  (should (not (dbus-codegen--object-path-p "/a/")))
  (should (dbus-codegen--object-path-p "/a/b"))
  (should (not (dbus-codegen--object-path-p "/a!")))
  (should (dbus-codegen--object-path-p "/a")))

(ert-deftest dbus-codegen--annotate-arg ()
  (should (equal '(:int32 1)
		 (dbus-codegen--annotate-arg ':int32 1)))
  (should-error (dbus-codegen--annotate-arg '(:array :int32) 1))
  (should (equal '((:array :int32 1 :int32 2 :int32 3))
		 (dbus-codegen--annotate-arg '(:array :int32) '(1 2 3))))
  ;; Type mismatch of the first element of a struct.
  (should-error (dbus-codegen--annotate-arg '(:struct :string :int32 :int32)
					    '(1 2 3)))
  (should (equal '((:array (:dict-entry :string "a" :int32 1)
			   (:dict-entry :string "b" :int32 2)
			   (:dict-entry :string "c" :int32 3)))
		 (dbus-codegen--annotate-arg
		  '(:array (:dict-entry :string :int32))
		  '(("a" . 1) ("b" . 2) ("c" . 3))))))

(eval-when-compile
  (defconst dbus-codegen-tests-introspection-data "\
<node>
  <interface name='org.gtk.GDBus.PeerTestInterface'>
    <method name='HelloPeer'>
      <arg type='s' name='greeting' direction='in'/>
      <arg type='s' name='response' direction='out'/>
    </method>
    <method name='EmitSignal'/>
    <method name='EmitSignalWithNameSet'/>
    <method name='OpenFile'>
      <arg type='s' name='path' direction='in'/>
    </method>
    <signal name='PeerSignal'>
      <arg type='s' name='a_string'/>
    </signal>
    <property type='s' name='PeerProperty' access='read'/>
    <property type='s' name='PeerPropertyAnnotated' access='read'>
      <annotation name='org.freedesktop.DBus.Property.EmitsChangedSignal'
                  value='false'/>
    </property>
  </interface>
</node>"))

(ert-deftest dbus-codegen-define-proxy ()
  (dbus-codegen-define-proxy test-proxy
			     dbus-codegen-tests-introspection-data
			     "org.gtk.GDBus.PeerTestInterface")
  (should (fboundp 'test-proxy-create))
  (should (fboundp 'test-proxy-destroy))
  (should (fboundp 'test-proxy-hello-peer))
  (should (fboundp 'test-proxy-hello-peer-asynchronously))
  (should (fboundp 'test-proxy-emit-signal))
  (should (fboundp 'test-proxy-emit-signal-asynchronously))
  (should (fboundp 'test-proxy-emit-signal-with-name-set))
  (should (fboundp 'test-proxy-emit-signal-with-name-set-asynchronously))
  (should (fboundp 'test-proxy-open-file))
  (should (fboundp 'test-proxy-open-file-asynchronously))
  (should (fboundp 'test-proxy-register-peer-signal-signal))
  (should (fboundp 'test-proxy-send-peer-signal-signal))
  (should (fboundp 'test-proxy-handle-peer-signal-signal))
  (should (fboundp 'test-proxy-peer-property))
  (should (fboundp 'test-proxy-retrieve-peer-property-property))
  (should (fboundp 'test-proxy-peer-property-annotated))
  (should (fboundp 'test-proxy-retrieve-peer-property-annotated-property)))

(ert-deftest dbus-codegen-define-skeleton ()
  (dbus-codegen-define-skeleton test-skeleton
				dbus-codegen-tests-introspection-data
				"org.gtk.GDBus.PeerTestInterface")
  (should (fboundp 'test-skeleton-create))
  (should (fboundp 'test-skeleton-destroy))
  (should (fboundp 'test-skeleton-register-hello-peer-method))
  (should (fboundp 'test-skeleton-handle-hello-peer-method))
  (should (fboundp 'test-skeleton-register-emit-signal-method))
  (should (fboundp 'test-skeleton-handle-emit-signal-method))
  (should (fboundp 'test-skeleton-register-emit-signal-with-name-set-method))
  (should (fboundp 'test-skeleton-handle-emit-signal-with-name-set-method))
  (should (fboundp 'test-skeleton-register-open-file-method))
  (should (fboundp 'test-skeleton-handle-open-file-method))
  (should (fboundp 'test-skeleton-register-peer-signal-signal))
  (should (fboundp 'test-skeleton-send-peer-signal-signal))
  (should (fboundp 'test-skeleton-handle-peer-signal-signal))
  (should (fboundp 'test-skeleton-register-peer-property-property))
  (should (fboundp 'test-proxy-peer-property-annotated))
  (should (fboundp 'test-proxy-retrieve-peer-property-annotated-property)))

(provide 'dbus-codegen-tests)

;;; dbus-codegen-tests.el ends here
