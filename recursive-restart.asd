(asdf:defsystem :recursive-restart
  :author ("Jeremy Phelps")
  :maintainer "Jeremy Phelps"
  :description "Restarts that can invoke themselves."
  :long-description "Restarts that can invoke themselves."
  :version "1"
  :depends-on (:alexandria)
  :components
  ((:file "recursive-restart")))
