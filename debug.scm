(module debug
  (debug
   debug?
   debug/syslog
   debug-priority
   make-syslog-port
   trace)

  (import chicken
          scheme
          extras
          data-structures
          srfi-13)
  (import-for-syntax data-structures
                     ports
                     matchable
                     srfi-13)
  (use ports
       srfi-13
       syslog)

  (include "debug-core.scm"))
