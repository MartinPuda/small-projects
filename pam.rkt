#lang racket

;;Helper functions

;enumerate

(define (enumerate lst)
  (for/list ([index (in-naturals)]
             [element lst])
    (list index element)))

;concatenate

(define (concatenate matrix)
  (apply append matrix))

;full

(define (full len item)
  (make-list len item))

;sum

(define (sum lst)
  (apply + lst))

;mean

(define (mean lst)
  (let ((len (length lst)))
    (if (zero? len) 0
        (/ (sum lst) len))))

;matrix op

(define (matrix-op matrix op axis)
  (case axis
    ((0) (apply map op matrix))
    ((1) (map (lambda (row) (apply op row)) matrix))))

;rows and cols selection

(define (transpose matrix)
  (apply map list matrix))

(define (select-rows matrix rows)
  (map (lambda (i) (list-ref matrix i)) rows))

(define (select-cols matrix columns)
  (transpose
   (select-rows (transpose matrix) columns)))

(define (select-indexes lst columns)
  (map (lambda (i) (list-ref lst i))
       columns))

;rc selection m[[0,1,...],[0,2,...]]

(define (index-select matrix r c)
  (list-ref (list-ref matrix r) c))

(define (rc-select matrix l1 l2)
  (map (lambda (r c) (index-select matrix r c))
       l1
       l2))

;numpy-argmin

(define (numpy-argmin matrix axis)
  (case axis
    ((#f) (let ((flat (flatten matrix)))
            (second (argmin car (map list
                                     flat
                                     (range (length flat)))))))
    ((0) (map (lambda (col)
                (numpy-argmin col #f))
              (transpose matrix)))
    ((1) (map (lambda (row)
                (numpy-argmin row #f)) matrix))))

;; read-distance-matrix-from-file

(define (split-row row)
  (map (lambda (s) (string->number (string-trim s)))
       (regexp-split #rx"," (string-trim row))))

(define (read-distance-matrix-from-file file-path)
  (let ((rows (port->lines (open-input-file file-path))))
    (map split-row rows)))

;; assign-clusters

(define (assign-clusters distance-matrix medoids)
  (let ((cluster-indices (numpy-argmin (select-cols distance-matrix medoids) 1))
        (clusters (build-vector (length medoids) (lambda (i) (list)))))
    (for-each (lambda (i+cl-dex)
                (let ((i (first i+cl-dex))
                      (cluster-index (second i+cl-dex)))
                  (vector-set! clusters cluster-index
                               (append (vector-ref clusters cluster-index)
                                       (list i)))))
              (enumerate cluster-indices))
    (vector->list clusters)))

;; calculate-cost

(define (calculate-cost distance-matrix clusters medoids)
  (let ((medoid-indices (concatenate
                         (for/list ((i+c (enumerate clusters)))
                           (let ((i (first i+c))
                                 (cluster (second i+c)))
                             (full (length cluster)
                                   (list-ref medoids i)))))))
    (sum (rc-select distance-matrix
                    (range (length distance-matrix))
                    medoid-indices))))

;; calculate-silhouette
(define (calculate-silhouette distance-matrix clusters)
  (let ((n (length distance-matrix))
        (silhouette 0))
    (for ((cluster clusters))
      (for ((vector cluster))
        (let ((ai (mean (select-indexes (list-ref distance-matrix vector)
                                     cluster)))
              (bi +inf.0))
          (for ((other-cluster clusters))
            (when (not (equal? other-cluster cluster))
              (set! bi (min bi (mean (select-indexes (list-ref distance-matrix vector)
                                                  other-cluster))))))
          (let ((s (/ (- bi ai) (max ai bi))))
            (set! silhouette (+ silhouette s))))))
    (/ silhouette n)))

;; greedy-medoid-selection

(define (greedy-medoid-selection distance-matrix n k)
  (let* ((random-index (random 0 n))
         (medoids (list random-index)))
    (let loop ((medoids medoids))
      (if (< (length medoids) k)
          (let* ((remaining-indices (set->list (set-subtract
                                                (list->set (range n))
                                                (list->set medoids))))
                 (remaining-distances (select-cols (select-rows distance-matrix
                                                                remaining-indices)
                                                   medoids))
                 (min-distances (matrix-op remaining-distances min 1))
                 (next-medoid (numpy-argmin min-distances #f)))
            (loop (append medoids
                          (list (list-ref remaining-indices next-medoid)))))
          medoids))))

;; pam

(define (pam distance-matrix k)
  (let* ((n (length distance-matrix))
         (medoids (greedy-medoid-selection distance-matrix n k))
         (clusters (assign-clusters distance-matrix medoids))
         (total-cost (calculate-cost distance-matrix clusters medoids)))
    (let loop ((updated #t)
               (medoids medoids)
               (clusters clusters)
               (total-cost total-cost))
      (if (not updated)
          (list clusters medoids (calculate-silhouette distance-matrix clusters))
          (let ((updated #f)
                (len (length medoids)))
            (let loop2 ((i 0))
              (when (< i len)
                (let ((non-medoids (remove* medoids (range n))))
                  (for ((non-medoid non-medoids))
                    (let* ((new-medoids (list-set medoids i non-medoid))
                           (new-total-cost (calculate-cost distance-matrix
                                                           clusters
                                                           new-medoids)))
                      (when (< new-total-cost total-cost)
                        (set! medoids new-medoids)
                        (set! total-cost new-total-cost)
                        (set! updated #t)))))
                (when updated (loop2 (+ i 1)))))
            (set! clusters (assign-clusters distance-matrix medoids))
            (loop updated medoids clusters total-cost))))))

;; main

(define (main)
  (let* ((file-path "C:/Users/mapud/Downloads/matr.txt")
         (distance-matrix (read-distance-matrix-from-file file-path))
         (k-values (range 2 6))
         (max-avg-silhouette -2)
         (best-k 0)
         (best-clusters '())
         (best-medoids '())
         (start-time (current-seconds)))
    (for-each (lambda (k)
                (let* ((pam_ (pam distance-matrix k))
                       (clusters (first pam_))
                       (medoids (second pam_))
                       (silhouette (third pam_)))
                  (printf "K is ~a, and silhouette is ~a~%" k silhouette)
                  (when (> silhouette max-avg-silhouette)
                    (set! max-avg-silhouette silhouette)
                    (set! best-k k)
                    (set! best-clusters clusters)
                    (set! best-medoids medoids))))     
              k-values)
    (let* ((end-time (current-seconds))
           (elapsed-time (- end-time start-time)))
      (printf "Elapsed Time: ~a seconds~%" elapsed-time)
      (printf "Best k is: ~a~%" best-k)
      (printf "Silhouette is: ~a~%" max-avg-silhouette)
      (for ((i (range best-k)))
        (printf "Cluster ~a has ~a vectors~%" i (length (list-ref best-clusters i)))))))
