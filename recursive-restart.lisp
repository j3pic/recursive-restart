(defpackage :recursive-restart
  (:use :common-lisp :alexandria)
  (:export :recursive-restart-case
           :restart-bind*
           :restart-return
           :do-restart
           :handler-bind*
           :handler-return))

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

(defmacro restart-labels (bindings &body body)
  `(recursive-restart-case
    (progn ,@body)
    ,@bindings))

(defmacro restart-bind* (bindings &body body)
"Analogous to the relation between let and let*.

 (restart-bind* ((retry (lambda (c) (invoke-restart 'continue)))
              (continue (lambda (c) (print :retry))))
   (error \"error!\"))
"
  `(restart-bind (,(car bindings))
     ,(if (cdr bindings)
          `(restart-bind* ,(cdr bindings)
             ,@body)
          body)))

(defmacro restart-return (bindings &body body)
  "The variation of restart-case whose behavior is the same but
the semantics are that of RESTART-BIND.
Just as RESTART-CASE, the condition is handled first (that is, it jumps
out of the RESTART-BIND scope with GO) and then
the restart function is called. Finally, RESTART-RETURN returns
the value of restart function."
  (with-gensyms (block-name)
    (let ((bindings2
           (mapcar
            (lambda (binding)
              (destructuring-bind
                    (name function . key-value-pair)
                  binding
                (with-gensyms (fn-name rest)
                  (list `(,fn-name
                          (&rest ,rest)
                          (return-from ,block-name
                            (apply ,function ,rest)))
                        `(,name (named-lambda ,(symbolicate name '-handler)
                                    (&rest ,rest)
                                  (apply #',fn-name ,rest))
                                ,@key-value-pair)))))
            bindings)))
      `(block ,block-name
         (flet ,(mapcar #'first bindings2)
           (return-from ,block-name
             (restart-bind
                 ,(mapcar #'second bindings2)
               ,@body)))))))

(defmacro do-restart (bindings &body body)
  "A construct that, after a restart is invoked, it jumps to the start and reevaluate
the body by default. Example:

 (do-restart ((retry (lambda (c) (print :retry)))
           (continue (lambda (c) (print :retry))))
   (error \"error!\"))
"
  (with-gensyms (start)
    `(block nil
       (tagbody
          ,start
          (return
            (restart-bind
                ,(mapcar
                  (lambda (binding)
                    (destructuring-bind
                          (name function . key-value-pair)
                        binding
                      (with-gensyms (rest)
                        `(,name (named-lambda ,(symbolicate name '-handler)
                                    (&rest ,rest)
                                  (prog1
                                      (apply ,function ,rest)
                                    (go ,start)))
                                ,@key-value-pair))))
                  bindings)
              ,@body))))))

(defmacro handler-bind* (bindings &body body)
"Analogous to the relation between let and let*.
In standard handler-bind, the execution of the handler is
'run in a dynamic environment where none of these handler bindings are visible (to
avoid recursive errors).'
 -- (http://www.lispworks.com/documentation/HyperSpec/Body/m_handle.htm)

 (handler-bind* ((error    (lambda (c) (print :error)))
                 (my-error (lambda (c) (print :my) (signal c))))
   (error 'my-error))
"
  `(handler-bind (,(car bindings))
     ,(if (cdr bindings)
          `(handler-bind* ,(cdr bindings)
             ,@body)
          body)))

(defmacro handler-return (bindings &body body)
  "The variation of handler-case whose behavior is the same but
the semantics are that of HANDLER-BIND.
Just as HANDLER-CASE, the condition is handled first (that is, it jumps
out of the HANDLER-BIND scope with GO) and then
the handler function is called. Finally, HANDLER-RETURN returns
the value of the handler function. Example:

 (restart-return ((retry (lambda (c) (print :retry)))
               (continue (lambda (c) (print :retry))))
   (error \"error!\"))

is equivalent to:

 (restart-case
     (error \"error!\")
   (retry (c) (print :retry))
   (continue (c) (print :retry)))
"
  (with-gensyms (block-name)
    (let ((bindings2
           (mapcar
            (lambda (binding)
              (destructuring-bind
                    (name function . key-value-pair)
                  binding
                (with-gensyms (fn-name rest)
                  (list `(,fn-name
                          (&rest ,rest)
                          (return-from ,block-name
                            (apply ,function ,rest)))
                        `(,name (named-lambda ,(symbolicate name '-handler)
                                              (&rest ,rest)
                                              (apply #',fn-name ,rest))
                                ,@key-value-pair)))))
            bindings)))
      `(block ,block-name
         (flet ,(mapcar #'first bindings2)
           (return-from ,block-name
             (handler-bind
                 ,(mapcar #'second bindings2)
               ,@body)))))))



