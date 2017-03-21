(defun cigar-party (cigars is-weekend)
  (if (not is-weekend)
      (and (>= cigars 40) (<= cigars 60))
      (>= cigars 40)))

(defun date-fashion (you date)
  (cond
    ((or (<= you 2) (<= date 2)) 0)
    ((or (>= you 8) (>= date 8)) 2)
    (t 1)))

(defun squirrel-play (temp is-summer)
  (if (or is-summer)
      (and (>= temp 60) (<= temp 100))
      (and (>= temp 60) (<= temp 90))))

(defun caught-speeding (speed is-birthday)
  (if (not is-birthday)
      (cond
	((<= speed 60) 0)
	((and (<= speed 80) (> speed 60)) 1)
	((> speed 80) 2))
      (cond
	((<= speed 65) 0)
	((and (<= speed 85) (> speed 65)) 1)
	((> speed 85) 2))))
