(define (rad degrees)
  (* (/ 3.1415926535 180) degrees))

(define (range start end step)
  (if (>= start end) '()
      (cons start (range (+ start step) end step))))

(define (grass-pattern-help x width height)
  (if (= width x) (list (list width height))
      (cons (list (+ x 5)
                  (- height (+ 25 (random (floor (/ height 2))))))
            (cons (list (+ x 10) (- height 25))
                  (grass-pattern-help (+ x 10) width height)))))

(define (grass-pattern width height)
  (append (list (list width height)
                (list 0 height)
                (list 0 (- height 25)))
          (grass-pattern-help 0 width height)))

(define (draw-egg image layer height x deg)
  (gimp-image-select-ellipse image 0 x (- height 25 112) 75 112)
  (let ((rot (car (gimp-item-transform-rotate layer 0 TRUE 0 0))))
    (gimp-context-set-brush "1. Pixel")
    (gimp-context-set-brush-size 5)
    (gimp-context-set-foreground '(245 222 179))
    (gimp-drawable-edit-fill rot 0)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-drawable-edit-stroke-selection rot)
    (gimp-context-set-foreground '(255 255 255))
    (gimp-context-set-brush "2. Hardness 050")
    (gimp-context-set-brush-size 15)
    (for-each (lambda (y)
                (gimp-context-set-foreground (list (random 255)
                                                   (random 255)
                                                   (random 255)))
                (gimp-pencil rot 4 (vector 0 y 75 y)))
              (range 0 112 15))
    (gimp-context-set-brush-size 3)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-drawable-edit-stroke-selection rot)
    (gimp-item-transform-rotate rot (rad deg) TRUE 0 0)
    (gimp-selection-none image)
    (for-each (lambda (l)
                (if (= 1 (car (gimp-layer-is-floating-sel l)))
                    (gimp-floating-sel-anchor l)
                    l))
              (vector->list (nth 1 (gimp-image-get-layers image))))
    (gimp-selection-none image)))

(define (egg-eggs image layer height)
  (for-each (lambda (x)
              (draw-egg image layer height x (- (random 20) 10)))
            (range 15 (car (gimp-image-width image)) 100)))

(define (draw-grass image layer)
  (gimp-selection-none image)
  (let ((points (list->vector (apply append (grass-pattern
                                             (car (gimp-image-width image))
                                             (car (gimp-image-height image)))))))
    (gimp-image-select-polygon image 0 (vector-length points) points)
    (gimp-context-set-foreground '(0 255 0))
    (gimp-drawable-edit-fill layer 0)
    (gimp-context-set-brush "2. Hardness 075")
    (gimp-context-set-brush-size 1)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-drawable-edit-stroke-selection layer)
    (gimp-selection-none image)))

(define (draw-bg image layer)
  (gimp-context-set-foreground '(232 244 248))
  (gimp-drawable-edit-bucket-fill layer 0 1 1))

(define (eggs i d w h)
  (gimp-context-set-foreground '(255 255 255))
  (gimp-context-set-background '(255 255 255))
  (let* ((image (car (gimp-image-new w h 0)))
         (layer (car (gimp-layer-new image w h RGB-IMAGE "1" 100 LAYER-MODE-NORMAL))))
    (gimp-image-insert-layer image layer 0 0)
    (gimp-context-set-foreground '(255 255 255))
    (gimp-drawable-edit-fill layer 0)
    (egg-eggs image layer h)
    (draw-bg image layer)
    (draw-grass image layer)
    (gimp-display-new image)
    (gimp-selection-none image)
    (cons image layer)))

(script-fu-register
 "eggs"
 _"Add Eggs..."
 _"Add eggs"
 "Martin Puda"
 "Martin Puda"
 "7/4/2023"
 "*"
 SF-IMAGE       "Input image" 0
 SF-DRAWABLE    "Input drawable" 0
 SF-VALUE      "Width" "300"
 SF-VALUE      "Height" "150")

(script-fu-menu-register "eggs" "<Image>/My_Scripts")