
(define-module zip-archive
  (use srfi-19)
  (use rfc.zlib)
  (use binary.pack)
  (export open-output-zip-archive
          zip-add-entry
          zip-close
          call-with-output-zip-archive))

(select-module zip-archive)

(define (date->dos-format date)
  (let* ((year (date-year date))
         (month (date-month date))
         (day (date-day date))
         (hour (date-hour date))
         (minute (date-minute date))
         (second (date-second date)))
    (+ (ash (- year 1980) 25) (ash month 21) (ash day 16)
       (ash hour 11) (ash minute 5) (quotient second 2))))

(define-class <local-file-header> ()
  ((compress-method :init-keyword :compress-method)
   (timestamp :init-keyword :timestamp)
   (checksum :init-keyword :checksum)
   (compressed-size :init-keyword :compressed-size)
   (uncompressed-size :init-keyword :uncompressed-size)
   (filename-size :init-keyword :filename-size)
   (offset :init-keyword :offset)
   (filename :init-keyword :filename)))

(define-class <zip-archive> ()
  ((port :init-keyword :port)
   (name :init-keyword :name)
   (tempname :init-keyword :tempname)
   (timestamp :init-form (current-date))
   (local-file-headers :init-form '())))

(define-method write-pk0304 ((za <zip-archive>) (lfh <local-file-header>))
  (pack "VvvvVVVVvva*"
    (list #x04034b50
          20
          0
          (~ lfh 'compress-method)
          (date->dos-format (~ lfh 'timestamp))
          (~ lfh 'checksum)
          (~ lfh 'compressed-size)
          (~ lfh 'uncompressed-size)
          (~ lfh 'filename-size)
          0
          (~ lfh 'filename))
      :output (~ za 'port)))

(define (open-output-zip-archive filename)
  (receive (port tempname)
      (sys-mkstemp (string-append (sys-dirname filename) "/ziptmp"))
    (make <zip-archive> :port port :name filename :tempname tempname)))

(define-method zip-add-entry
  ((za <zip-archive>) (name <string>) (content <string>)
   :key (timestamp (~ za 'timestamp))
        (compression-level Z_DEFAULT_COMPRESSION))
  (let* ((position (port-tell (~ za 'port)))
         (compress-method (if (= compression-level Z_NO_COMPRESSION) 0 8))
         (compressed
          (if (= compress-method 0)
              content
              (deflate-string content
                :window-bits -15
                :compression-level compression-level)))
         (local-file-header
          (make <local-file-header>
             :compress-method compress-method
             :timestamp timestamp
             :checksum (crc32 content)
             :compressed-size (string-size compressed)
             :uncompressed-size (string-size content)
             :filename-size (string-size name)
             :offset position
             :filename name)))
    (write-pk0304 za local-file-header)
    (display compressed (~ za 'port))
    (push! (~ za 'local-file-headers) local-file-header)))
         
(define-method write-pk0102 ((za <zip-archive>) (lfh <local-file-header>))
  (pack "VvvvvVVVVvvvvvVVa*"
    (list #x02014b50 20 20 0
          (~ lfh 'compress-method)
          (date->dos-format (~ lfh 'timestamp))
          (~ lfh 'checksum)
          (~ lfh 'compressed-size)
          (~ lfh 'uncompressed-size)
          (~ lfh 'filename-size)
          0 0 0 0 0
          (~ lfh 'offset)
          (~ lfh 'filename))
    :output (~ za 'port)))

(define-method zip-close ((za <zip-archive>))
  (let ((cd (port-tell (~ za 'port)))
        (num (length (~ za 'local-file-headers))))
    (for-each (pa$ write-pk0102 za) (reverse (~ za 'local-file-headers)))
    (let1 eoc (port-tell (~ za 'port))
      (pack "VvvvvVVv"
        (list #x06054b50 0 0 num num (- eoc cd) cd 0)
        :output (~ za 'port)))
    (close-output-port (~ za 'port)))
  (sys-rename (~ za 'tempname) (~ za 'name)))

(define-syntax call-with-output-zip-archive
  (syntax-rules ()
    ((_ filename proc)
     (let1 za (open-output-zip-archive filename)
       (guard (e (else (close-output-port (~ za 'port))
                       (sys-unlink (~ za 'tempname))
                       (raise e)))
         (proc za)
         (zip-close za))))))

(provide "zip-archive")
