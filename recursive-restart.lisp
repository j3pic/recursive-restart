(defpackage :recursive-restart
  (:use :common-lisp)
  (:export :recursive-restart-case))

(in-package :recursive-restart)

(defmacro recursive-restart-case (value-form &rest cases)
  "RECURSIVE-RESTART-CASE has the same semantics as RESTART-CASE, except
you can re-invoke any of the restarts in a mutually recursive fashion.
Example:

   (let ((invoked-bar nil))
     (recursive-restart-case
          (invoke-restart 'foo)
       (foo ()
          (format t \"Invoked FOO.~%\")
          (if invoked-bar
              :done
              (invoke-restart 'bar)))
       (bar ()
          (format t \"Invoked BAR.~%\")
          (setf invoked-bar t)
          (format t \"Invoking FOO again...~%\")
          (invoke-restart 'foo))))
"
          
  (let ((case-gensyms (mapcar (lambda (case)
				(declare (ignore case))
				(gensym)) cases))
	(re-entry-label (gensym))
	(value-form-entered (gensym))
	(restart-function (gensym))
	(restart-function-args (gensym))
	(block-name (gensym)))
    `(flet ,(loop for name in case-gensyms
	       for (_ lambda-list . body) in cases
	       collect `(,name ,lambda-list ,@body))
       (let ((,value-form-entered nil)
	     (,restart-function nil)
	     (,restart-function-args nil))
	 (block ,block-name
	   (tagbody
	      ,re-entry-label
	      (restart-case
		  (return-from ,block-name
		    (if (not ,value-form-entered)
			(progn
			  (setf ,value-form-entered t)
			  ,value-form)
			(apply ,restart-function ,restart-function-args)))
		,@(loop for (case-name _ . body) in cases
		     for function-name in case-gensyms
		     collect `(,case-name (&rest args)
					  (setf ,restart-function #',function-name)
					  (setf ,restart-function-args args)
					  (go ,re-entry-label))))))))))
    
