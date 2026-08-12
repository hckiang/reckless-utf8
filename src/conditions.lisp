;;;; conditions.lisp
(in-package #:reckless-utf8)

(define-condition utf8-error (error)
  ()
  (:documentation "Base condition for all errors signalled by reckless-utf8."))

(define-condition utf8-bounds-error (utf8-error)
  ((start :initarg :start :reader utf8-bounds-error-start)
   (end   :initarg :end   :reader utf8-bounds-error-end))
  (:report (lambda (c s)
             (format s "Invalid start/end arguments: start=~S, end=~S"
                     (utf8-bounds-error-start c)
                     (utf8-bounds-error-end c)))))

(define-condition utf8-encoding-error (utf8-error)
  ((code-point :initarg :code-point :reader utf8-encoding-error-code-point))
  (:documentation "Error while encoding a Unicode scalar value to UTF-8."))

(define-condition utf8-surrogate-error (utf8-encoding-error)
  ()
  (:report (lambda (c s)
             (format s "Surrogate U+~X is not a valid Unicode scalar value"
                     (utf8-encoding-error-code-point c)))))

(define-condition utf8-code-point-out-of-range (utf8-encoding-error)
  ()
  (:report (lambda (c s)
             (format s "Code point U+~X exceeds Unicode maximum #x10FFFF"
                     (utf8-encoding-error-code-point c)))))

(define-condition utf8-decoding-error (utf8-error)
  ((index  :initarg :index  :reader utf8-decoding-error-index)
   (reason :initarg :reason :reader utf8-decoding-error-reason
           :initform "Ill-formed UTF-8"))
  (:documentation "Error while decoding a UTF-8 octet sequence.")
  (:report (lambda (c s)
             (format s "~A at octet index ~D"
                     (utf8-decoding-error-reason c)
                     (utf8-decoding-error-index c)))))

(define-condition utf8-ill-formed-sequence (utf8-decoding-error)
  ())

(define-condition utf8-unrepresentable-character (utf8-decoding-error)
  ((code-point :initarg :code-point
               :reader utf8-unrepresentable-character-code-point))
  (:report (lambda (c s)
             (format s "Scalar value #x~X not representable at octet index ~D"
                     (utf8-unrepresentable-character-code-point c)
                     (utf8-decoding-error-index c)))))
