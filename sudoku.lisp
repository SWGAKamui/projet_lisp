(defun possibility()
  (cons '0 '(1 2 3 4 5 6 7 8 9)))
(defun line ()
  (possibility)
  (possibility)
  (possibility)
  (possibility)
  (possibility)
  (possibility)
  (possibility)
  (possibility)
  (possibility))
(defun make-line()
  (list (line)
	(line)
	(line)
	(line)
	(line)
	(line)
	(line)
	(line)
	(line)))
  
(defun greed-maker ()
  (list (make-line)(make-line)(make-line)
	(make-line)(make-line)(make-line)
	(make-line)(make-line)(make-line)))
	
