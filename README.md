# recursive-restart

Implements several forms for using restarts recursively. The first is `recursive-restart-case`, which
works exactly like CL's native `restart-case`, except you can invoke the restarts from the restart
handlers in a mutually-recursive fashion, such as this:


    (let ((invoked-bar nil))
      (recursive-restart-case
           (invoke-restart 'foo)
        (foo ()
           (format t "Invoked FOO.~%")
           (if invoked-bar
               :done
               (invoke-restart 'bar)))
        (bar ()
           (format t "Invoked BAR.~%")
           (setf invoked-bar t)
           (format t "Invoking FOO again...~%")
           (invoke-restart 'foo))))

Also provided is `restart-labels`, which is a wrapper macro that allows you to specify the handlers
before the body, which gains an implicit PROGN. The syntax mirrors that of CL's `labels` form:

    (restart-labels
        (foo () (format t "Invoked FOO.~%"))
        (bar () (format t "Invoked BAR.~%"))
      (format t "Starting body...~%")
      (invoke-restart 'foo))

`restart-bind*` is to `restart-labels` as `let*` is to `let`, ie, each handler is wrapped in a new `recursive-restart-case`.
