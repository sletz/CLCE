
(defun apply-seq (seq fun)
  (do ((e (firstev seq) (link e)))
      ((nullptrp e))
    (funcall fun e)))


(defun player1 (midi-score)
  (let ((seq (score2seq midi-score)))
    (apply-seq seq #'(lambda (e) 
                       (if (= (evtype e) typenote) (midi-set-field  e 2 (round (-  (/ (midi-get-field e 2) 10) 2))))
                       (date e (round(+ (/ (date e) 10) 1)))
                       (ref e (+ (* 16 (port e)) (+ 1 (chan e) )))))
    ;;(kill-mf-player)
    (when (null *session-mf-player*)
      (setq *session-mf-player* (open-msh-player "Lisp_Player" )))
    (set-mf-player-seq *session-mf-player* seq 504)
    ;;(mf-player-start-seq *session-mf-player*)
    (start-callback)
    ))

(defun player ()
  (player1 *out*))
