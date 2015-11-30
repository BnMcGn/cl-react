(in-package #:cl-react)

(defpsmacro define-react-function (name lambda-list &key nicknames)
  (let ((args
	 (remove-if (lambda (sym) (find sym '(&optional &key))) lambda-list)))
    `(progn
       (defun ,name ,lambda-list
	 (chain |React| (,name ,@args)))
       ,@(mapcar (lambda (nickname)
		   `(defun ,nickname ,lambda-list (,name ,@args)))
		 nicknames))))

(defpsmacro cl-react:define-class (name &rest object-specification)
  `(defvar ,name
     (create-class (create ,@(reduce #'append object-specification)))))

(defpsmacro cl-react:psx (form)
  (compile-psx form))

(defparameter cl-react:with-ps 'with-ps)

(defun build ()
  (ps-compile-file "./cl-react.lisp"))
