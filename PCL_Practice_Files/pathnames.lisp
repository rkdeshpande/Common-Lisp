;;;; We will write a function list-directory that will provide a wrapper around the standard
;;;; function DIRECTORY.  This function allows us to define a "wild pathname" and returns a
;;;; list of pathnames representing files that match this wild value.  But DIRECTORY has two
;;;; problems:
;;;;     1) Certain aspects of its behavior differ significantly between implementations, even
;;;;        on the same Operating system.
;;;;     2) While DIRECTORY provides us a powerful interface to list files, its use demands an
;;;;        understanding of how pathnames are implemented.  It will be better to use a wrapper
;;;;        list-directory and deal with these once and only once.


;;; The following three functions are helpers, designed to allow list-directory to accept a
;;; nonwild pathname of file or directory form, and convert it to the appropriate wild
;;; pathname

;; Test whether a given component of a pathname is "present," meaning neither NIL nor the
;; special value :unspecific.
(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

;; Test whether a pathname is already in directory form
(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

;; Convert any pathname to a directory form pathname
(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
			    (list (file-namestring pathname)))
	 :name      nil
	 :type      nil
	 :defaults pathname)
	pathname)))


;;; Our goal now is to generate a wild pathname we can pass to DIRECTORY.  It might seem that
;;; we can call MAKE-PATHNAME on the returned pathname from pathname-as-directory.  But
;;; unfortunately CLISP has a quirk that prevents this. CLISP will not return files without an
;;; extension unless the type component is set to NIL.  We will have to create a fourth helper
;;; to handle this circumstance.

;; This function takes a pathname of directory or file form and returns a properly formatted
;; wildcard based on implementation.
(defun directory-wildcard (dirname)
  (make pathname
	:name :wild
	:type #-clisp :wild #+clisp nil
	:defaults (pathname-as-directory dirname)))


;;; A simple implementation of list-directory will now work in SBCL, CMUCL, and LispWorks.  But
;;; there are a few more implementation problems we need to sort through:
;;;     1) Not all implementations return subdirectories of the given directory
;;;     2) Not all implementations retun directory names in directory form

;; The following function will return files and subdirectories, differentiating between these
;; two and using the appropriate format.
(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))

;; The function clisp-subdirectories-wildcard can be used in non-CLISP implementations, but
;; since we don't need it any other time, we'll use a #+ for its definition.
#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))


;; The following function accepts a pathname and returns the pathname if the file exists, and
;; NIL otherwise.  This mimics the behavior of PROBE-FILE in some implementations, and covers
;; any implementation differences for others.
(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname) ; in these implementations PROBE-FILE is good enough

  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname)) ; Try first with directory form, if NIL, try with file form

  #+clisp
  (or (ignore-errors
	(probe-file (pathname-as-file pathname))) ; avoid errors if pathname is directory
      (ignore-errors
	(let ((directory-form (pathname-as-directory pathname)))
	  (when (ext:probe-directory directory-form)
	    directory-form)))) ; avoid errors, return pathname instead of T if directory exists

  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists-p not implemented"))


;; We will allow pathname-as-file to exist for all implementations, even though CLISP is the
;; only implementation that requires it here.
(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
	(let* ((directory (pathname-directory pathname))
	       (name-and-type (pathname (first (last directory)))))
	  (make-pathname
	   :directory (butlast directory)
	   :name (pathname-name name-and-type)
	   :type (pathname-type name-and-type)
	   :defaults pathname))
	pathname)))


;; Our final function will take the name of a directory and a function, and it will call this
;; function on the pathnames of all the files under the directory, recursively.  We will also
;; have it take two keyword arguments: :directories and :test.  The former lets us call our
;; function on directories as well as files, while the latter allows us to call the function
;; only under circumstances that pass the test.
(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
	 (cond
	   ((directory-pathname-p name)
	    (when (and directories (funcall test name))
	      (funcall fn name))
	    (dolist (x (list-directory name)) (walk x)))
	   ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))
