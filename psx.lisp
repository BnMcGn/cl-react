(in-package :cl-react.psx)

;;; *******************************************************************************************

#.(progn
    (defvar *user-readtable-case* (readtable-case *readtable*))
    (setf (readtable-case *readtable*) :invert))

;;; *******************************************************************************************

(defparameter *binary-attrs*
  '(:read-only
    :disabled
    :hidden)
  "True/false attributes. When set to NIL, they are simply ignored.")


(defconstant +dom-types+
  '(:math :svg :a :abbr :address :area :article :aside :audio :b :base :bdi :bdo
 :blockquote :body :br :button :button :button :button :canvas :caption :cite
 :code :col :colgroup :command :command :command :command :datalist :dd :del
 :details :dfn :div :dl :dt :em :embed :fieldset :figcaption :figure :footer
 :form :h1 :h2 :h3 :h4 :h5 :h6 :head :header :hgroup :hr :html :i :iframe :img
 :input :ins :kbd :keygen :label :legend :li :link :map :mark :menu
 :meta :meta :meta :meta :meta :meta :meter :nav :noscript :object :ol
 :optgroup :option :output :p :param :pre :progress :q :rp :rt :ruby :s :samp
 :script :section :select :small :source :span :strong :style :sub :summary
 :sup :table :tbody :td :textarea :tfoot :th :thead :time :title :tr :track :u
    :ul :var :video :wbr))
	    
(defparameter *attr-synonyms*
  '(:readonly :read-only
    :class :class-name)
  "HTML attributes that differ from JSX or that need some doctoring to work with Parenscript")

(defun parse-attr (attr value)
  (let ((jsx-attr (or (getf *attr-synonyms* attr) attr)))
    (when (or (not (find jsx-attr *binary-attrs*)) value)
	(list (make-symbol (string jsx-attr)) value))))

(defun format-attrs (attrs &key accum)
  (if (null attrs)
      (nreverse accum)
      (destructuring-bind (key val) (parse-attr (first attrs) (second attrs))
        (if key
            (format-attrs (cddr attrs) :accum (list* val key accum))
            (format-attrs (cddr attrs) :accum accum)))))

(defun dom-type-p (type)
  (find type +dom-types+))

(defun divide-attributes/body (things &key accum)
  "Divide a list at the point where key/value pairs stop occurring."
  (if (keywordp (car things))
      (divide-attributes/body (cddr things)
                              :accum (list* (second things) (car things) accum))
      (values (nreverse accum) things)))
 
(defun %call-body (element attribs body
                   &aux (attribs (when attribs `(create ,@attribs)))
                   (elementx (make-symbol (string element))))
  (if (dom-type-p element)
      `(chain React DOM (,elementx ,attribs ,@body))
      `(chain React (create-element ,elementx ,attribs ,@body))))

(defun proc-psx (code)
  (multiple-value-bind (attributes body) (divide-attributes/body (cdr code))
    (setf body
          (loop for itm in body
                collect (if (and (listp itm) (keywordp (car itm)))
                            (proc-psx itm)
                            itm)))
    (%call-body (car code) (format-attrs attributes)
                (if (rest body)
                    `((list ,@body))
                    (list (first body))))))

(defun compile-psx (form)
  (proc-psx form))

;;; *******************************************************************************************

#.(setf (readtable-case *readtable*) *user-readtable-case*)

;;; *******************************************************************************************
