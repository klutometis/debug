(module debug
  (debug debug? debug/syslog trace)
  (import chicken scheme extras data-structures ports srfi-13)
  (import-for-syntax data-structures ports matchable srfi-13)
  (use srfi-13 syslog)

  (include "debug-core.scm"))
