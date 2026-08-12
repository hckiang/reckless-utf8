(in-package #:reckless-utf8)

(declaim (optimize (speed 3) (safety 1) (debug 0) (compilation-speed 0)))

(deftype octet        () '(unsigned-byte 8))
(deftype octet-vector () '(simple-array octet (*)))
(deftype code-point   () '(integer 0 #x10FFFF))
(deftype array-index  () '(integer 0 #.array-dimension-limit))

(defun utf8-byte-length (string &key (start 0) end)
  "Number of UTF-8 octets needed for STRING[START..END)."
  (declare (type string string)
           (type array-index start)
           (type (or null array-index) end))
  (let ((end (or end (length string)))
        (n 0))
    (declare (type array-index end n))
    (loop for i from start below end
          for c of-type code-point = (char-code (schar string i))
          do (incf n (cond ((< c #x80) 1)
                           ((< c #x800) 2)
                           ((< c #x10000) 3)
                           (t 4))))
    n))

(defun utf8-encode-into (string dest &key (start 0) end (dest-start 0))
  (declare (type string string)
           (type octet-vector dest)
           (type array-index start dest-start)
           (type (or null array-index) end))
  (when (or (minusp start) (and end (minusp end)) (and end (not (<= start end))))
    (error 'utf8-bounds-error :start start :end end))
  (let ((end (or end (length string)))
        (di dest-start))
    (declare (type array-index end di))
    (loop for i from start below end
          for code of-type code-point = (char-code (schar string i))
          do (cond
               ((< code #x80)
                (setf (aref dest di) code)
                (incf di))
               ((< code #x800)
                (setf (aref dest di)     (logior #b11000000 (ash code -6))
                      (aref dest (1+ di)) (logior #b10000000 (logand code #x3f)))
                (incf di 2))
               ((< code #x10000)
                ;; Reject surrogates if they somehow appear
                (when (<= #xD800 code #xDFFF)
                  (error 'utf8-surrogate-error :code-point code))
                (setf (aref dest di)       (logior #b11100000 (ash code -12))
                      (aref dest (+ di 1)) (logior #b10000000 (logand (ash code -6) #x3f))
                      (aref dest (+ di 2)) (logior #b10000000 (logand code #x3f)))
                (incf di 3))
               (t
                (when (> code #x10FFFF)
                  (error 'utf8-code-point-out-of-range :code-point code))
                (setf (aref dest di)       (logior #b11110000 (ash code -18))
                      (aref dest (+ di 1)) (logior #b10000000 (logand (ash code -12) #x3f))
                      (aref dest (+ di 2)) (logior #b10000000 (logand (ash code -6) #x3f))
                      (aref dest (+ di 3)) (logior #b10000000 (logand code #x3f)))
                (incf di 4))))
    (- di dest-start)))


(declaim (type (simple-array (unsigned-byte 8) (256)) +utf8-info+))

(defparameter +utf8-info+
  (let ((a (make-array 256 :element-type 'octet :initial-element 0)))
    (loop for i from 0 to 127 do (setf (aref a i) 0)) ; ASCII
    (loop for i from #x80 to #xBF do (setf (aref a i) #b10000000)) ; cont / illegal as lead
    (loop for i from #xC0 to #xC1 do (setf (aref a i) #b10000000)) ; overlong 2-byte
    (loop for i from #xC2 to #xDF do (setf (aref a i) #b00000001)) ; 2-byte
    (loop for i from #xE0 to #xE0 do (setf (aref a i) #b00001110)) ; 3-byte, special
    (loop for i from #xE1 to #xEC do (setf (aref a i) #b00000010))
    (loop for i from #xED to #xED do (setf (aref a i) #b00000010)) ; surrogates handled below
    (loop for i from #xEE to #xEF do (setf (aref a i) #b00000010))
    (loop for i from #xF0 to #xF4 do (setf (aref a i) #b00000011))
    (loop for i from #xF5 to #xFF do (setf (aref a i) #b10000000)) ; too large
    a))

(defun utf8-valid-p (octets &key (start 0) end)
  (declare (type octet-vector octets)
           (type array-index start)
           (type (or null array-index) end))
  (nlet work ((octets octets) (start start) (end end))
    (block done-ascii
      (let ((end (or end (length octets)))
            (i start))
        (declare (type array-index end i))
        (loop
          (when (>= i end) (return t))
          (let* ((b0 (aref octets i))
                 (info (aref +utf8-info+ b0)))
            (declare (type octet b0 info))
            (when (logbitp 7 info)          ; illegal lead
              (return nil))
            (let ((need (logand info #b11)))
              (declare (type (integer 0 3) need))
              (if (zerop need)            ; ASCII
                  (progn
                    (incf i)
                    (loop while (< i end)
                          for b = (aref octets i)
                          while (< b 128)
                          do (incf i)
                          finally (return (work octets i end))))
                  ;; multi-byte
                  (progn
                    (when (>= (+ i need) end) (return nil))
                    (let ((b1 (aref octets (+ i 1))))
                      (declare (type octet b1))
                      (unless (= (logand b1 #b11000000) #b10000000) (return nil))
                      (case need
                        ;; (1) ; already covered
                        (2
                         (let ((b2 (aref octets (+ i 2))))
                           (declare (type octet b2))
                           (unless (= (logand b2 #b11000000) #b10000000) (return nil))
                           ;; overlong E0 80–9F and surrogate ED A0–BF
                           (when (or (and (= b0 #xE0) (< b1 #xA0))
                                     (and (= b0 #xED) (>= b1 #xA0)))
                             (return nil))))
                        (3
                         (let ((b2 (aref octets (+ i 2)))
                               (b3 (aref octets (+ i 3))))
                           (declare (type octet b2 b3))
                           (unless (and (= (logand b2 #b11000000) #b10000000)
                                        (= (logand b3 #b11000000) #b10000000))
                             (return nil))
                           ;; overlong F0 80–8F and out-of-range F4 90–BF
                           (when (or (and (= b0 #xF0) (< b1 #x90))
                                     (and (= b0 #xF4) (>= b1 #x90)))
                             (return nil)))))
                      (incf i (1+ need))))))))))))


(defun utf8-decode-into (octets dest &key (start 0) end (dest-start 0))
  (declare (type octet-vector octets)
           (type string dest)
           (type array-index start dest-start)
           (type (or null array-index) end))
  (when (or (minusp start) (and end (minusp end)) (and end (not (<= start end))))
    (error 'utf8-bounds-error :start start :end end))
  (let ((end (or end (length octets)))
        (di  dest-start)
        (i   start))
    (declare (type array-index end di i))
    (labels ((fail (&optional (msg "Ill-formed UTF-8"))
               (error 'utf8-ill-formed-sequence :index i :reason msg))
             (need (n)
               (when (> (+ i n) end) (fail)))
             (store (code)
               (let ((char (code-char code)))
                 (unless char
                   (error 'utf8-unrepresentable-character
                          :index i
                          :code-point code))
                 (setf (schar dest di) char)
                 (incf di))))
      (loop while (< i end)
            do (let ((b0 (aref octets i)))
                 (declare (type octet b0))
                 (cond
                   ;; 1-byte
                   ((< b0 #x80)
                    (store b0)
                    (incf i))

                   ;; 2-byte  C2–DF 80–BF
                   ((<= #xC2 b0 #xDF)
                    (need 2)
                    (let ((b1 (aref octets (1+ i))))
                      (declare (type octet b1))
                      (unless (<= #x80 b1 #xBF) (fail))
                      (store (logior (ash (logand b0 #x1F) 6)
                                     (logand b1 #x3F)))
                      (incf i 2)))

                   ;; 3-byte
                   ((<= #xE0 b0 #xEF)
                    (need 3)
                    (let ((b1 (aref octets (+ i 1)))
                          (b2 (aref octets (+ i 2))))
                      (declare (type octet b1 b2))
                      (unless (and (<= #x80 b1 #xBF)
                                   (<= #x80 b2 #xBF)
                                   (cond ((= b0 #xE0) (<= #xA0 b1 #xBF))
                                         ((= b0 #xED) (<= #x80 b1 #x9F))
                                         (t t)))
                        (fail))
                      (store (logior (ash (logand b0 #x0F) 12)
                                     (ash (logand b1 #x3F) 6)
                                     (logand b2 #x3F)))
                      (incf i 3)))

                   ;; 4-byte
                   ((<= #xF0 b0 #xF4)
                    (need 4)
                    (let ((b1 (aref octets (+ i 1)))
                          (b2 (aref octets (+ i 2)))
                          (b3 (aref octets (+ i 3))))
                      (declare (type octet b1 b2 b3))
                      (unless (and (<= #x80 b1 #xBF)
                                   (<= #x80 b2 #xBF)
                                   (<= #x80 b3 #xBF)
                                   (cond ((= b0 #xF0) (<= #x90 b1 #xBF))
                                         ((= b0 #xF4) (<= #x80 b1 #x8F))
                                         (t t)))
                        (fail))
                      (store (logior (ash (logand b0 #x07) 18)
                                     (ash (logand b1 #x3F) 12)
                                     (ash (logand b2 #x3F) 6)
                                     (logand b3 #x3F)))
                      (incf i 4)))

                   (t (fail))))))
    (- di dest-start)))
