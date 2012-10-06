(module debug
  (debug debug? debug/syslog trace)
  (import chicken scheme extras data-structures ports srfi-13)
  (import-for-syntax ports matchable)
  (use syslog)

  (include "debug-core.scm"))
