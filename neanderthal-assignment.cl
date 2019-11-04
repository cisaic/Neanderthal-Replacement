;;;260578661
;;;Christina Isaicu
;;;Contributors:
   ;;;Ahmed Soliman for idea brainstorming (not a student in PSYC 315)
;;;file: neanderthal-assignment.cl
;;;purpose: simulate neanderthal replacement 
;;;adapted from neanderthal.cl by Thomas Shultz
;;;completed: 30 oct 2018
;;;altered code is indicated in RED

(defvar *path* nil
  "Path for saving files.")

(defvar *vec* (make-array 0)
  "Vector for storing populations.")

(defvar *state0* nil
  "Variable for first state (ex. Neanderthals represented by 0).")

(defvar *state1* nil
  "Variable for second state.")

(defvar *adv0* nil
  "Border advantage for first state.")

(defvar *adv1* nil
  "Border advantage for first state.")

;;;random numbers

(defun call-random (m n)
  "(m n)
Call (random n) m times. Used by seed-random."
  (do ((i 0 (1+ i)))
      ((= i m) nil)
    (if (zerop n) 
        (random 1)
      (random n))))

(defun seed-random ()
  "Seed random from the last 4 digits of time. 
Useful for generating unique random sequences."
  (let* ((time (get-internal-real-time))
         (n (multiple-value-bind (x y) (floor time 100) (cadr (list x y))))
         (hundreds (multiple-value-bind (x y) (floor time 100) (car (list x y))))
         (m (multiple-value-bind (x y) (floor hundreds 100) (cadr (list x y)))))
    (call-random m n)))

;;;saving lists in file

(defun lists->file (lst file &optional (separator #\tab))
  "(lst file &optional (separator #\tab))
Save reverse lst in file. Items in flat lst are printed 1 per line.
Separator is used to separate items on a line for embedded lst."
  (with-open-file
      (output-stream file :direction :output :if-exists :supersede)
    (do ((items (reverse lst) (cdr items)))
        ((null items))
      (let ((sub-item (car items)))
        (if (listp sub-item)
            (print-line sub-item separator output-stream)
          (format output-stream "~a ~%"
            sub-item))))))

(defun print-line (lst &optional (separator " ") output-stream)
  "(lst &optional (separator " ") output-stream)
Print each item in list on a line separated by separator. 
Then go to new line."
  (do ((lst lst (cdr lst)))
      ((null lst) (terpri output-stream))
    (princ (car lst) output-stream)
    (princ separator output-stream)))

;;;(setf *vec* (concatenate 'vector '(1 2) '(3 4))) -> #(1 2 3 4)

(defun build-list (n x)
  "(n x)
Make list of n xs."
  (do ((i 0 (1+ i))
       (lst nil (cons x lst)))
      ((= i n) lst)))

;;;(build-list 5 0) -> (0 0 0 0 0)

(defun build-vector (nx x ny y)
  "(nx x ny y)
Make vector of nx xs & ny ys."
  (let ((xlist (build-list nx x))
        (ylist (build-list ny y)))
    (setf *vec* (concatenate 'vector xlist ylist))))

;;; find type and position of neighbors

(defun neighborsf (pos)
  "(pos)
Neighbors types and their positions given position (pos) of death band
returns vector of neighbors with list of (position type) for each."
  (let ((neighbors (make-array 4))
        (size (length *vec*))
        (len (truncate(sqrt (length *vec*)))))
    
    (cond ((or (< pos 0)
               (> pos (1- size)))
           (format t "position out of bounds"))
          
          ;;; TODO refactor if statements to helper functions
          (t
           ;;; top
           (if (> pos (1- len)) 
               (progn
                 (setf top (list (- pos len)
                                 (svref *vec* (- pos len))))
                 (setf (svref neighbors 0) top))
             (setf (svref neighbors 0) '(nil nil)))
           
           ;;; left
           (if (not (= (mod pos len) 0)) 
               (progn
                 (setf left (list (1- pos)
                                  (svref *vec* (1- pos))))
                 (setf (svref neighbors 1) left))
             (setf (svref neighbors 1) '(nil nil)))
           
           ;;; right
           (if (not (= (mod pos len)(1- len)))
               (progn
                 (setf right (list (+ pos 1)
                                   (svref *vec* (+ pos 1))))
                 (setf (svref neighbors 2) right))
             (setf (svref neighbors 2) '(nil nil)))
           
           ;;;bottom
           (if (< pos (- size len))
               (progn
                 (setf bottom (list (+ pos len)
                                    (svref *vec* (+ pos len))))
                 (setf (svref neighbors 3) bottom))
             (setf (svref neighbors 3) '(nil nil)))))
    
    (return-from neighborsf neighbors)))

(defun calcBias (neighbors advantage selfType)
  "(neighbors advantage selfType)
calculate bias by iterating through neighbors and identifying neighbor type
bias is positive if neighbor is same type as death band, or negative if different
if no neighbor, function consults the border advantage specified by user
(wheather boarders are advantageous, detrimental, or neutral)."
  (do ((i 0 (1+ i))
       (bias 0 (if (eql nil (second (svref neighbors i)))
                   (+ bias advantage)
                 (if (eql (second (svref neighbors i))
                          selfType)
                     (+ bias 1)
                   (- bias 1)))))
      ((= i (length neighbors)) bias)))

(defun difNeighborsp (otherType neighbors)
  "(otherType neighbors)
returns true if any of the neighbors are different type than the band that died."
  (do ((i 0 (1+ i)))
      ((= i (length neighbors)) nil)
    (if (eql (second (svref neighbors i))
             otherType)
        (return t))))

(defun alldifNeighborsp (selfType neighbors)
  "(sameType neighbors)
returns true if ALL of the neighbors are of different type than the band that died."
  (do ((i 0 (1+ i)))
      ((= i (length neighbors)) t)
    (if (eql (second (svref neighbors i))
             selfType)
        (return nil))))

(defun printpopulation ()
  "()
print *vec* in array to visualize borders"
  (setf veclength (sqrt (length *vec*)))
  (loop for i to (1- (length *vec*)) doing       
        (cond ((= i (1- (length *vec*)))
               ;;;(format t "~a ~% ~% ~%" (svref *vec* i)))
               (format t "~a ~%" (svref *vec* i)))
              ((= (1- veclength) 
                  (round(mod i veclength)))
               (format t "~a ~%" (svref *vec* i)))
              (t 
               (format t "~a" (svref *vec* i))))))

(defun replacement (rep pos fitness)
  "(pos fitness)
replacement formerly defined as death-birth
if neighbors are all the same, death band is replaced by same type, so do nothing
if at least one neighbor is different:
	calculate weighted advantage according to fitness, boarder advantage, and neighboring types
	if random value is less than weighted advantage, same type replaces death band, else other type replaces it."
  (let* ((neighbors (neighborsf pos))
         (advantage (if (eql (svref *vec* pos) *state0*)
                        *adv0* *adv1*))
         (selfType (if (eql (svref *vec* pos) *state0*)
                       *state0* *state1*))
         (otherType (if (eql (svref *vec* pos) *state0*)
                        *state1* *state0*))
         (rand (random 1.0)))
    
    (setf bias (calcBias neighbors advantage selfType)) 
    (setf bias (/ bias (* 5 (length neighbors))))
    (setf difNeigh (difNeighborsp otherType neighbors))
    
    (if difNeigh
        (progn
          (if (alldifNeighborsp selfType neighbors)
              (setf (svref *vec* pos) otherType)
            (progns
              (if (eql selfType *state0*)
                  (setf fitness (- 1 fitness)))
              (setf weightedAdv (+ fitness bias))
              
              (if (< rand weightedAdv)
                  (setf (svref *vec* pos) selfType)
                (setf (svref *vec* pos) otherType))))))))
;;;(format t "=====~%")))))
;;;(format t "~a ~%" *vec*)))
;;;(printpopulation)
;;(format t "rep ~a~%" rep)))

(defun tally (x)
  "(x)
Count x in *vec*."
  (do ((n (length *vec*))
       (i 0 (1+ i))
       (result 0 (if (= (svref *vec* i) x)
                     (1+ result)
                   result)))
      ((= i n) result)))

(defun run1 (rep nx x ny y bias)
  "(rep nx x ny y bias)
Run 1 simulation with nx ny & y-bias."
  (build-vector nx x ny y)
  (do ((cycles 0 (1+ cycles))
       (tx nx (tally x))
       (ty ny (tally y)))
      
      ((or (= tx 0)
           (= ty 0))  
       (list tx ty cycles))
    (let* ((death (random (+ nx ny))))
      (replacement cycles death bias))))


(defun run (n nx x advx ny y advy bias)
  "(n nx x advx ny y advy bias
Run n replications with nx ny with base fitness (bias) and costal advantage for x and y 
advx 0 = no advantage/disadvantage at the coast for x
advx -1 = disadvantage for x
advx 1 = advantage for x
"
  (seed-random)
  (setf *path* "/Users/christinaisaicu/Documents/0McGill/U3/FALL2018/PSYC 315/neanderthal-replacement/assignment-output/")
  (setf *state0* x)
  (setf *state1* y)
  (setf *adv0* advx)
  (setf *adv1* advy)
  (print (list nx advx ny advy bias))
  
  (do ((i 0 (1+ i))
       (results nil (cons (run1 i nx x ny y bias)
                          results)))
      ((= i n) (lists->file results (concatenate 'string *path* "ny" (princ-to-string ny) "-advx" (princ-to-string advx) "-advy" (princ-to-string advy) "-bias" (princ-to-string bias) ".txt")))
    (print i)))


s