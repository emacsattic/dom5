;;; dom5.el --- Add HTML5's DOM methods to dom.el

;; Copyright (C) 2007  Edward O'Connor

;; Author: Edward O'Connor <hober0@gmail.com>
;; Keywords: xml
;; URL: http://edward.oconnor.cx/elisp/dom5.el

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; An implementation of the various new DOM methods defined in the HTML5
;; draft specification.

;; N.B. Functions prefixed by `dom-' are public-facing, and augment the
;; API provided by dom.el. Functions prefixed with `dom5-' are internal
;; and should not be called.


;;; History:
;; 2007-08-10: Initial version.

;;; Code:

(require 'dom)

;; interface HTMLDocument
;;   ...
;;   NodeList getElementsByName(in DOMString elementName);
;;   NodeList getElementsByClassName(in DOMString[] classNames);
;;   ...

;; The getElementsByName(name) method a string name, and must return a
;; live NodeList containing all the a, applet, button, form, iframe,
;; img, input, map, meta, object, select, and textarea elements in that
;; document that have a name attribute whose value is equal to the name
;; argument.

(defun dom-document-get-elements-by-name (document name)
  "Return all of the nodes in DOCUMENT whose name= is NAME."
  (error "Not implemented yet"))

;; The getElementsByClassName(classNames) method takes an array of
;; strings representing classes. When called, the method must return a
;; live NodeList object containing all the elements in the document that
;; have all the classes specified in that array. If the array is empty,
;; then the method must return an empty NodeList.

(defun dom-document-get-elements-by-class-name (document &rest class-names)
  "Return all nodes in DOCUMENT thathave all classes named in CLASS-NAMES."
  (dom5-element-get-elements-by-class-name-1
   (dom-document-element document)
   class-names))

;; interface HTMLElement : Element {
;;  ...
;;  NodeList getElementsByClassName(in DOMString[] classNames);

(defun dom-element-get-elements-by-class-name (element &rest class-names)
  "Return all nodes under ELEMENT thathave all classes named in CLASS-NAMES."
  (dom5-element-get-elements-by-class-name-1
   (dom-element-first-child element)
   class-names))



(defun dom5-element-has-class (element class)
  "Non-nil if ELEMENT has CLASS."
  (and (eq (dom-node-type element) dom-element-node)
       (catch 'found
         (let ((attrs (dom-node-attributes element)))
           (dolist (attr attrs)
             (when (eq (dom-node-name attr) 'class)
               (dolist (class-name (split-string (dom-node-value attr)))
                 (when (equal class-name class)
                   (throw 'found t))))))
         nil)))

(defun dom5-element-get-elements-by-class-name-1 (element class-names)
  "Return a list of nodes under ELEMENT with all classes in CLASS-NAMES.
CLASS-NAMES is a list of strings. The elements are ELEMENT, its
siblings, and their descendants. This is used by
`dom-element-get-elements-by-class-name' and
`dom-document-get-elements-by-class-name'."
  ;; We don't want to call this recursively because of performance.
  (let (stack result)
    (while element
      ;; when `element' has all class names in `class-names'
      (when (and (eq (dom-node-type element) dom-element-node)
                 (reduce (lambda (a b) (and a b))
                    (mapcar (lambda (class-name)
                              (dom5-element-has-class element class-name))
                            class-names)))
	(setq result (cons element result)))
      (setq element
	    (cond ((dom-node-first-child element)
		   (when (dom-node-next-sibling element)
		     (push (dom-node-next-sibling element) stack))
		   (dom-node-first-child element))
		  ((dom-node-next-sibling element))
		  (t (pop stack)))))
    (nreverse result)))

(provide 'dom5)
;;; dom5.el ends here
