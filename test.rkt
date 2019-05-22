#lang racket
(require "Bitmap.rkt")
(require "functions.rkt")

(define image (make-object bitmap-image%))
(send image load-image "butterfly.bmp")
 ;(send image write-image name)  ;general way to write an image


(send (vertical-flip image) write-image "v-flip.bmp") 
(send (rotate-image image) write-image "rotated.bmp")
(send (horizontal-flip image) write-image "h-flip.bmp")
(send (blackwhite image) write-image "bw.bmp")
(collage2in1 image image "collage2in1.bmp")
(collage4in1 image image image image "collage4in1.bmp")
(send (blur image) write-image "blur.bmp")
(send (motion-blur image) write-image "motion_blur.bmp")
(send (find-edges image) write-image "edges.bmp")
(send (sharpen image) write-image "sharpen.bmp")
(send (emboss image) write-image "embossed.bmp")
(define filter '((0.1 0.1 0.1)
				 (0.1 0.2 0.1)
				 (0.1 0.1 0.1)))
(send (gen-filter image filter 2 0) write-image "gen-filter.bmp") 
(send (gaussian-filter image) write-image "gauss.bmp")
(send (threshold image (node 150 150 100)) write-image "thresh.bmp")
(send image image->rle "compressed.rle")
(define image-decompressed (make-object bitmap-image%))
(send image-decompressed rle->image  "compressed.rle")
(send image-decompressed write-image "decompressed.bmp")

;;;;examples to apply 1 function after other

(send (blackwhite (rotate-image image)) write-image "final.bmp")
(send (find-edges (vertical-flip image)) write-image "final1.bmp")