;;; 我们的数据库代码支持插入，查询，更新和删除，更不用说还有用来添加新记录和导出内容的命令行接口，只有50行多一点代码。
(defvar *db* nil)

(defun make-cd (title artist rating ripped)
	(list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd)
  (push cd *db*))
  
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt) ;; returns a string 
  (format *query-io* "~a: " prompt) ;; *query-io* is a global variable that contains the input stream connected to the terminal. 
  (force-output *query-io*)
  (read-line *query-io*)) 
;; The call to FORCE-OUTPUT is necessary in some implementations to ensure that 
;; Lisp doesn’t wait for a newline before it prints the prompt.

(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
    (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                  :direction :output
                  :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename) ;; default :direction :input
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; general query
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;; selector-fn generator
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
	  (and
	    (if title (equal title (getf cd :title)) t)
		(if artist (equal artist (getf cd :artist)) t)
		(if rating (equal rating (getf cd :rating)) t)
		(if ripped-p (equal ripped (getf cd :ripped)) t))
    ))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
    (mapcar
    #'(lambda (row)
        (when (funcall selector-fn row)
        (if title (setf (getf row :title) title))
        (if artist (setf (getf row :artist) artist))
        (if rating (setf (getf row :rating) rating))
        (if ripped-p (setf (getf row :ripped) ripped)))
        row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
