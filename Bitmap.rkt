#lang racket

(provide (struct-out node))
(provide (all-defined-out))

;Structure to store 24 bit BMP images
(struct node (red green blue) #:transparent)

;Function to make a 2-d vector
(define (make-2d-vector r c)
  (build-vector r       
	(lambda (x) (make-vector c #f))))

; Default BMP image class
(define bitmap-image%
  (class object%

    ;Default name for image incase nothing is provided as input
    

    (super-new)

    (define image-vec   'null)          ;stores image in vector format 
    (define file-size   'undefined)     ;total size of file in bytes
    (define offset      'null)          ;Defines offset to start of pixel Data
    (define header-size 'null)          ;Define file header size
    (define width       'null)          ;Defines Image width
    (define height      'null)          ;Defines Image height
    (define bit-count   'null)          ;BMP image type
    (define c-type      'null)          ;Type of compression used ,0 if uncompresses
    (define color-count 'null)          ;Number Color Map entries that are actually used
    (define sig-color   'null)          ;Number of significant colors
    (define color_table '())          ;Reads color table for images except 24 bit images



    ;returns a copy of image vector 
    (define/public (get-image)
      (vector-copy image-vec))

    ;returns a copy of image vector 
    (define/public (set-image-vec! new-image-vec)
      (set! image-vec new-image-vec))
 

    ;info about image 
    (define/public (image-info)
      (begin (display "Height: ")    (display height)    (display "\tWidth: ")       (display width)       (newline)
             (display "File Size: ") (display file-size) (display "\tHeader Size: ") (display header-size) (newline)
             (display "Offset: ")    (display offset)    (display "\tType: ")        (display bit-count)   (newline)))
             
    ;returns image vector 
    (define/public (image-width)
      (list bit-count header-size width height offset))


    ;function to read k continuous bit from binary from in little endian format
    (define (read-k-bytes k file-handler)
      (cond [(= k 0) (error "Cannot read 0 Bytes")]
            [(= k 1) (read-byte file-handler)]
            [else (+ (read-byte file-handler) (* 256 (read-k-bytes (- k 1) file-handler)))]))

    ;Forward k-bytes in binary file frame i.e rad k bytes which have no use as such
    (define (forward-k-bytes k file-handler)
      (cond [(= k 0) (void)]
            [(= k 1) (read-byte file-handler)]
             [else   (begin (read-byte file-handler) (forward-k-bytes (- k 1) file-handler))]))

    ;checks if file is an bmp image
    (define (bmp? in)
      (let* ([B (read-byte in)]
             [M (read-byte in)])
        (if (and (= B 66) (= M 77))
            (display "Bitmap image loaded\n")
            (error   "Invalid Bitmap image\n"))))

    
    ;Function to store color table if present 
    (define (store_color_palette unused_byte in)
           (cond [(= 0 unused_byte ) (void)]
                 [(= 1 unused_byte ) (set! color_table (cons (read-byte in) color_table))]
                 [else (begin (set! color_table (cons (read-byte in) color_table)) 
                              (store_color_palette (- unused_byte 1) in))]))

    
    ;Read pixels from rows
    (define (column_read j row-vector in)
      (cond [(and (< j width) (>= j 0))
             (begin (define blue   (read-byte in))
                    (define green  (read-byte in))
                    (define red    (read-byte in))
                    (vector-set!   row-vector j (node red green blue))
                    (column_read   (+ j 1) row-vector in)
                    )]))
    
    ;Read Pixels by column
    (define (row_read i null_padding in)
      (cond [(= i 0) (column_read 0 (vector-ref image-vec i) in)]
            [(> i 0) (begin
                       (column_read 0 (vector-ref image-vec i) in)
                       (forward-k-bytes null_padding in)
                       (row_read (- i 1) null_padding in))]))

    ;Check that height is consistent and not null before writing to a file
    (define (image_height_integrity_check)
      (cond ((or (= height 0) (equal? height 'null)) (error "Image cannot be of height 0 or null size"))))

    ;Check that height is consistent and not null before writing to a file
    (define (image_width_integrity_check)
      (cond ((or (= width 0) (equal? width 'null)) (error "Image cannot be of width 0 or null size"))))

    ;Convert integer to bytes of given size
    (define (to_bytes size integer_num)
      (cond ((= size 0) '())
            (else (cons (modulo integer_num 256) (to_bytes (- size 1) (quotient integer_num 256))))))

    ;null paddings for BMP images
    (define (add_padding write_null_padding_count)
      (cond ((= write_null_padding_count 0) '())
            ((= write_null_padding_count 1) '(0))
            ((= write_null_padding_count 2) '(0 0))
            ((= write_null_padding_count 3) '(0 0 0))
            ))
    
    ;Writing null paddings to binary file
    (define (null_write write_null_padding out)
      (foldl (lambda (x y) (write-byte x out)) '() write_null_padding))
    
    ;Writing data to a row
    (define (column_write j row-vector out)
      (cond [(and (< j width) (>= j 0))
             (begin (define x (vector-ref row-vector j))
                    (write-byte   (node-blue x) out)
                    (write-byte   (node-green x) out)
                    (write-byte   (node-red x) out)
                    (column_write (+ j 1) row-vector out)
                    )]))
    
    ;Writing data by column
    (define (row_write i write_null_padding out)
      (cond [(= i 0) (column_write 0 (vector-ref image-vec i) out)]
            [(> i 0) (begin
                       (column_write 0 (vector-ref image-vec i) out)
                       (null_write write_null_padding out)
                       (row_write (- i 1)  write_null_padding out))]))

    ;Writing image and file header
    (define (header_write l out)
      (cond ((= (length l) 0) (error "Valid image not provided"))
            ((= (length l) 1) (write-byte (car l) out))
            (else (begin (write-byte   (car l) out)
                       (header_write (cdr l) out)))))


    
    ;Loads an image file as vector of vector of structs
    (define/public (load-image file_name)

      ;open file in binary mode
      (define in (open-input-file file_name #:mode 'binary))

      ;Reading file header
      (bmp? in)
      (set! file-size  (read-k-bytes 4 in))
      (forward-k-bytes 4 in)
      (set! offset  (read-k-bytes 4 in))

      ;Reading Image headers
      (set! header-size (read-k-bytes 4 in))
      (set! width       (read-k-bytes 4 in))
      (set! height      (read-k-bytes 4 in))
      (forward-k-bytes 2 in)
      (set! bit-count   (read-k-bytes 2 in))
      (set! c-type      (read-k-bytes 4 in))

      ;Error checking to load normal BMP images
      (cond (not (= c-type     0)) (error "Compresseed_images not supported"))
      (cond (not (= bit-count 24)) (error "Only 24 bit RGB BMP accepted"))

      (define unused_information (- header-size 20))
      (forward-k-bytes unused_information in)

      (store_color_palette (- offset (+ 14 header-size)) in)
      (define null_padding (modulo (- 4 (modulo (* width 3) 4)) 4))
      (set! image-vec (make-2d-vector height width))
      (row_read (- height 1) null_padding in)          
      )

   
    ;Function to write image to a file
    (define/public (write-image filename)

      (set! height (vector-length image-vec))
      (set! width  (vector-length (vector-ref image-vec 0)))
      
      ;Checking for valid images
      (image_width_integrity_check)
      (image_height_integrity_check)

      ;Managing null paddings will be constant for an image
      (define write_null_padding_count (modulo (- 4 (modulo (* width 3) 4)) 4))
      (define write_null_padding (add_padding write_null_padding_count))
      (define file-size (+ 54 (* (+ (* width 3) write_null_padding_count) height)))
           
      ;Create bitmap file header
      (define File-header (append '(66 77)  (to_bytes 4 file-size) '(0 0 0 0 54 0 0 0)))
      
      ;Creating 40 bit header
      (define Image-header (append (to_bytes 4 40) (to_bytes 4 width) (to_bytes 4 height) (to_bytes 2 1) (to_bytes 2 24)
                                   (to_bytes 4 0) (to_bytes 4 0) (to_bytes 4 0) (to_bytes 4 0) (to_bytes 4 0) (to_bytes 4 0)))

      ;-Writing matrix data
      (define out (open-output-file filename #:mode 'binary #:exists 'replace))
      (header_write File-header out)
      (header_write Image-header out)
      (row_write (- height 1) write_null_padding out)
      (close-output-port out)    
      )

    ;Converts a row in rle format
    (define (rle-row l)
      (define (rle-helper code l l1 n)
        (if (null? l) (reverse (cons (list l1 n) code))
            (if (or (= n 255) (not (equal? (car l) l1))) (rle-helper (cons (list l1 n) code)  (cdr l) (car l) 1)
                (rle-helper code (cdr l) l1 (+ n 1)))))
      (if (null? l) (list)
          (rle-helper '() l (car l) 0)))

    ;Compares closness of pixels
    (define (inrange n1 n2)
      (if (and (> (abs (- (node-red n1) (node-red n2))) 4)
               (> (abs (- (node-red n1) (node-red n2))) 4)
               (> (abs (- (node-red n1) (node-red n2))) 4)) #f #t ))

    ;Converts row in lossy-rle format
    (define (rle-lossy-row l)
      (define (rle-helper code l l1 n)
        (if (null? l) (reverse (cons (list l1 n) code))
            (if (or (= n 255) (not (inrange (car l) l1))) (rle-helper (cons (list l1 n) code)  (cdr l) (car l) 1)
                (rle-helper code (cdr l) l1 (+ n 1)))))
      (if (null? l) (list)
          (rle-helper '() l (car l) 0)))

    
    ;Writes a list to binary file
    (define (list-write l out)
      (cond [(not (null? l)) (begin (write-byte (car l) out) (list-write (cdr l) out))])) 


    ;Convert an image in rle format
    (define/public (image->rle file-name) 
      ;Checking for valid images
      (image_width_integrity_check)
      (image_height_integrity_check)
      
      ;Vec to image
      (define image_list  (vector->list image-vec))
      (set! image_list  (map (lambda (x) (vector->list x)) image_list))
      (define rle_image (append* (map rle-lossy-row image_list)))
      ;Open a file
      (define out (open-output-file file-name #:mode 'binary #:exists 'replace))
      (define File-header (append '(70 70) (to_bytes 4 file-size) (to_bytes 4 width) (to_bytes 4 height)))
      (header_write File-header out)
      (define File-payload (foldr (lambda(x y) (cons (node-red (car x)) (cons (node-green (car x)) (cons (node-blue  (car x))
                                                                                                         (append (to_bytes 1 (cadr x)) y))))) '() rle_image))
      (list-write File-payload out)
      (close-output-port out))

    ;Checks if rle format
    (define (rle? in)
      (let* ([B (read-byte in)]
             [M (read-byte in)])
        (if (and (= B 70) (= M 70))
            (display "RLE image loaded\n")
            (error   "Invalid RLE image\n"))))

    ;Take a column in rle format
    (define (read-rle-column in size)
      (cond [(= size width) '()]
            [else (begin (define red   (read-byte in))
                         (define green (read-byte in))
                         (define blue  (read-byte in))
                         (define count (read-byte in))
                         (cons (cons (node red green blue) count) (read-rle-column in (+ size count))))]))

    ;Read rle rows 
    (define (read-rle-row in size)
      (cond [(= size height) '()]
            [else (cons (read-rle-column in 0) (read-rle-row in (+ size 1)))]))

    ;Changes each row in rle to pixels
    (define (rle->img-helper code)           
      (define (row-rle l)
        (foldr merger '() l))
      (define (merger x list)
        (define (merger-helper e l n)
          (if (= n 0) l
              (merger-helper e (cons e l) (- n 1))))
        (merger-helper (car x) list (cdr x)))  
      (map row-rle code))
    
    ;reading a rle file
    (define/public (rle->image file-name)
      ;open file in binary mode
      (define in (open-input-file file-name #:mode 'binary))
      (rle? in)
      (set! file-size    (read-k-bytes 4 in))
      (set! width        (read-k-bytes 4 in))
      (set! height       (read-k-bytes 4 in))
      (define image_list (read-rle-row in 0))
      (set! image_list (rle->img-helper image_list))
      (set! image_list (map (lambda (x) (list->vector x)) image_list))
      (set! image-vec  (list->vector image_list)) 
      (close-input-port in))
    ))