(uiop:define-package #:reckless-utf8
  (:use #:cl #:serapeum/iter)
  (:export #:utf8-encode-into
           #:utf8-decode-into
           #:utf8-valid-p
           #:utf8-byte-length
           
           #:utf8-error
           #:utf8-bounds-error
           #:utf8-encoding-error
           #:utf8-surrogate-error
           #:utf8-code-point-out-of-range
           #:utf8-decoding-error
           #:utf8-ill-formed-sequence
           #:utf8-unrepresentable-character))
