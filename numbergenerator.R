library(stringr)

# Generate 0 borrowings

subtraction <- data.frame(termA = sample(1:9, 25, replace=TRUE ), termB = sample(1:9, 25, replace=TRUE ), ans = "")

for (j in 1:nrow(subtraction)) {
  while ((str_length(subtraction[j,1]) < 10) & (str_length(subtraction[j,2]) < 9)) {
    terms <- data.frame(termA = rep(0,10), termB = rep(1,10))
    while (any(terms[2] > terms[1])) {
      terms$termA <- sample(0:9, 10, replace=TRUE)
      terms$termB <- sample(0:9, 10, replace=TRUE)
    }
    subtraction[j,1] <- as.numeric(paste(terms$termA, collapse = "" ))
    subtraction[j,2] <- as.numeric(paste(terms$termB, collapse = "" ))
  }
}
subtraction$ans <- subtraction$termA - subtraction$termB


# Generate 6/10 borrowings per 10digit number

subtractionDiff <- data.frame(termA = sample(1:9, 25, replace=TRUE ), termB = sample(1:9, 25, replace=TRUE ), ans = "")

for (j in 1:nrow(subtractionDiff)) {
  while ((str_length(subtractionDiff[j,1]) < 10) & (str_length(subtractionDiff[j,2]) < 9)) {
    terms <- data.frame(termA = rep(0,10), termB = rep(1,10))
    while ((sum(terms$termA < terms$termB) != 6)) {
      terms$termA <- sample(0:9, 10, replace=TRUE)
      terms$termB <- sample(0:9, 10, replace=TRUE)
    }
    subtractionDiff[j,1] <- as.numeric(paste(terms$termA, collapse = "" ))
    subtractionDiff[j,2] <- as.numeric(paste(terms$termB, collapse = "" ))
  }
}
subtractionDiff$ans <- subtractionDiff$termA - subtractionDiff$termB


#I stop for now. I'll pick 25 numbers for each condition. Answers are always positive 10 digit numbers, which is *not* specified here.


###############################################

# ;;generate easy stimuli for subtraction, no carrying/borrowing.
# (defun get-easy-stims (nr)
#   (let ((lst nil))
#     (dotimes (i nr)
#       (push 
#         (let ((A "0")
#               (B "0"))
#           (do () ((and ;keep trying to make such numbers until all terms are 10 digits long
#                    (equalp (length (write-to-string (parse-integer A))) 10)
#                    (equalp (length (write-to-string (parse-integer B))) 10)
#                    (equalp (length (write-to-string (- (parse-integer A) (parse-integer B)))) 10)))
#             (setf A "")
#             (setf B "")
#             (dotimes (i 10)
#               (let ((termA (random 10))
#                     (termB (random 10)))
#                 (do () ((>= termA termB)) ;choose random numbers, make sure B <= A
#                   (setf termA (random 10))
#                   (setf termB (random 10)))
#                 (setf A (format nil "~a~a" termA A))
#                 (setf B (format nil "~a~a" termB B)))))
#           (list (parse-integer A) (parse-integer B)))
#         lst))
#     lst))
# 
# ;;calculate how many times a carry is necessary in a subtraction
# (defun calculate-carries (termA termB)
#   (let ((A (reverse (format nil "~a" termA)))
#         (B (reverse (format nil "~a" termB)))
#         (prev1 "")
#         (prev2 "")
#         (carries 0))
#     (loop 
#       for dig1 across A
#       for dig2 across B
#       do
#       (setf dig1 (parse-integer (format nil "~a" dig1)))
#       (setf dig2 (parse-integer (format nil "~a" dig2)))
#       (if (< dig1 dig2) (incf carries)) ;if dig1 < dig2, add carry
#       (when (and
#              (equalp dig1 dig2) ;if both digs are the equal
#              (> (length prev1) 0) ;there already a previous digit
#              (< (- (parse-integer prev1) (parse-integer prev2)) 0)) ;and the previous subtraction needs to carry
#         (incf carries)) ;add a carry     
#       (if (and (> (length prev1) 0) ;in case dig1 == 0 and carry is necessary, increase carries by 10, as we don't want that
#            (equalp dig1 0) 
#            (< (- (parse-integer prev1) (parse-integer prev2)) 0))
#            (incf carries 10)) ;geen doorlopende carries!!
#            
#            (setf prev1 (format nil "~a~a" dig1 prev1))
#            (setf prev2 (format nil "~a~a" dig2 prev2)))
#            carries))
#            
#            
#            ;;generate hard stimuli for subtraction, ie 6 carries in a 10 digit subtraction
#            (defun get-hard-stims (nr)
#            (let ((lst nil))
#            (dotimes (i nr)
#            (push 
#            (let ((A "0")
#            (B "0"))
#            (do () ((and
#            (equalp (length (write-to-string (parse-integer A))) 10)
#            (equalp (length (write-to-string (parse-integer B))) 10)
#            (equalp (length (write-to-string (- (parse-integer A) (parse-integer B)))) 10)
#            (> (- (parse-integer A) (parse-integer B)) 0)
#            (equalp (calculate-carries (parse-integer A) (parse-integer B)) 6)))
#            (setf A "")
#            (setf B "")
#            ; (print (incf cnt))
#            (dotimes (i 10)
#            (let ((termA (random 10))
#            (termB (random 10)))
#            (do () ((or (and (> (random 3) 1) (<= termA termB)) (>= termA termB)))
#            (setf termA (random 10))
#            (setf termB (random 10)))
#            (setf A (format nil "~a~a" termA A))
#            (setf B (format nil "~a~a" termB B)))))
#            (list (parse-integer A) (parse-integer B)))
#            lst))
#            lst))
#            