;; #!/usr/bin/sbcl --script

(defun print-documentation ()
  (write-line "Docs are for the weak." *standard-output*))

(define-condition input-error (error)
  ((fault :initarg :fault
	  :reader fault)))

(defstruct (stamp (:constructor make-stamp (&optional hours minutes
					      seconds milliseconds))
		  (:print-object format-stamp))
  (hours 0 :type fixnum)
  (minutes 0 :type fixnum)
  (seconds 0 :type fixnum)
  (milliseconds 0 :type fixnum))

(defun format-stamp (stamp stream)
  (format stream
	  "~2,'0D:~2,'0D:~2,'0D,~3,'0D"
	  (stamp-hours stamp)
	  (stamp-minutes stamp)
	  (stamp-seconds stamp)
	  (stamp-milliseconds stamp)))

;;; Assumes well-formed stamp
(defun read-stamp (stamp-string)
  (make-stamp (read-from-string (subseq stamp-string 0 2))
	      (read-from-string (subseq stamp-string 3 5))
	      (read-from-string (subseq stamp-string 6 8))
	      (read-from-string (subseq stamp-string 9 12))))

(defun stamp->seconds (stamp)
  (+ (* (stamp-hours stamp) 3600)
     (* (stamp-minutes stamp) 60)
     (stamp-seconds stamp)
     (* (stamp-milliseconds stamp) 0.001)))

(defun seconds->stamp (total-seconds)
  (multiple-value-bind (hours rest)
      (truncate total-seconds 3600)
    (multiple-value-bind (minutes rest)
	(truncate rest 60)
      (multiple-value-bind (seconds milliseconds)
	  (truncate rest)
	(make-stamp hours minutes seconds (round milliseconds 0.001))))))

(defun valid-stamp-p (stamp)
  (and (<= 0 (stamp-minutes stamp) 59)
       (<= 0 (stamp-seconds stamp) 59)
       (<= 0 (stamp-milliseconds stamp) 999)))

;; In order to sync a stamp we consider:
;;
;; 1) MS = movie start, ME = movie end: the times in the movie at which the
;;    first and the last subtitle used as interpolation points must actually
;;    appear, respectively.
;;
;; 2) FS = file start, FE = file end: the corresponding times in the out-of-sync
;;    subtitle file.
;;
;; For the time dilation coefficient we have: DIL=(ME-MS)/(FE-FS). Thus, for
;; every stamp pair A and B, the synced stamps will be: A'=(A-FS)*DIL+MS, and
;; B'=A'+DIFF*DIL, where DIFF=B-A.
;;
;; In order to accomplish that, MAKE-SYNCER accepts four stamps (MS, ME, FS and
;; FE) as arguments and produces a function which syncs two stamps (A and B)
;; passed to it as arguments. The appearance duration of each subtitle (i.e. the
;; time difference between A and B) is also scaled proportionally to the
;; dilation coefficient DIL.
(defun make-syncer (ms me fs fe)
  (let ((ms-sec (stamp->seconds ms))
	(me-sec (stamp->seconds me))
	(fs-sec (stamp->seconds fs))
	(fe-sec (stamp->seconds fe)))
    (let ((dil (/ (- me-sec ms-sec) (- fe-sec fs-sec))))
      (lambda (a b)
	(let* ((a-sec (stamp->seconds a))
	       (b-sec (stamp->seconds b))
	       (diff (- b-sec a-sec))
	       (a-synced-sec (+ (* (- a-sec fs-sec) dil) ms-sec))
	       (b-synced-sec (+ a-synced-sec (* diff dil))))
	  (values (seconds->stamp a-synced-sec)
		  (seconds->stamp b-synced-sec)))))))

(defun sync-line (line syncer)
  (multiple-value-bind (a b)
      (funcall syncer
	       (read-stamp (subseq line 0 12))
	       (read-stamp (subseq line 17 29)))
    (format nil "~A --> ~A" a b)))

;;; Constructs an association list out of ARGLIST given in command line,
;;; containing argument-value pairs for the REQUIRED-ARGS.
(defun collect-argvals (arglist required-args)
  (let (argvals)
    (dolist (arg required-args argvals)
      (let ((val (cadr (member arg arglist :test #'string=))))
	(when val
	  (push (cons arg val) argvals))))))

;;; Parses a stamp from a command line argument. The stamp is generated
;;; according to the four extracted TOKENS (corresponding to the 4 arguments of
;;; MAKE-STAMP) separated in the INPUT-STRING by a non-digit character
;;; separator. Empty tokens default to zero value.
(defun parse-stamp (input-string)
  (do* ((tokens nil (push (subseq string 0 separator-pos) tokens))
	(token-counter 0 (incf token-counter))
	(string input-string (setf string (subseq string (1+ separator-pos))))
	(separator-pos (position-if-not #'digit-char-p string)
		       (setf separator-pos
			     (position-if-not #'digit-char-p string))))
       ((or (= token-counter 3)		; parsed 3/4 tokens, last pending
	    (null separator-pos))	; ran out of tokens in INPUT-STRING
	(apply #'make-stamp
	       (mapcar (lambda (str)
			 (if (string= str "") 0
			     (read-from-string str)))
		       ;; last token (milliseconds) is parsed from the remainder
		       ;; of the STRING until its end, SEPARATOR-POS, or the
		       ;; next 3 characters, whichever comes first
		       (nreverse (push (subseq string
					       0
					       (min (or separator-pos
							(length string))
						    3))
				       tokens)))))))

(defun parse-args (args)
  (let ((argvals (collect-argvals args '("-i" "-o" "-ms" "-me" "-fs" "-fe"))))
    (destructuring-bind (input-file output-file ms me fs fe)
	(mapcar (lambda (arg) (cdr (assoc arg argvals :test #'string=)))
		'("-i" "-o" "-ms" "-me" "-fs" "-fe"))
      (unless (every #'identity (list input-file ms me fs fe))
	(signal 'input-error
		:fault "missing required input argument"))
      (unless (probe-file input-file)
	(signal 'input-error
		:fault "nonexistent input file"))
      (let ((output-file (or output-file
			     (let ((name (pathname-name input-file)))
			       (make-pathname :name (concatenate 'string name
								 "_synced")
					      :defaults input-file)))))
	(destructuring-bind (ms me fs fe)
	    (mapcar #'parse-stamp (list ms me fs fe))
	  (unless (every #'valid-stamp-p (list ms me fs fe))
	    (signal 'input-error
		    :fault "invalid input stamp(s)"))
	  (values input-file output-file ms me fs fe))))))

(defun main (&optional arglist)
  (flet ((input-error-handler (condition)
	   (format *error-output*
		   "Error: ~A. Exiting.~%"
		   (fault condition))
	   (return-from main)))
    (handler-bind ((input-error #'input-error-handler))
      (if (member "-h" (or arglist sb-ext:*posix-argv*) :test #'string=)
	  (print-documentation)
	  (multiple-value-bind (input-file output-file ms me fs fe)
	      (parse-args (or arglist sb-ext:*posix-argv*))
	    (let ((syncer (make-syncer ms me fs fe)))
	      (with-open-file
		  (in input-file :direction :input)
		(with-open-file
		    (out output-file :direction :output
				     :if-does-not-exist :create
				     :if-exists :supersede)
		  (do* ((line "" (setf line (read-line in nil)))
			(counter 0 (if (string= line "")
				       (setf counter 0)
				       (incf counter))))
		       ((null line))
		    (write-line (if (= counter 2)
				    (sync-line line syncer)
				    line)
				out))))))))))
