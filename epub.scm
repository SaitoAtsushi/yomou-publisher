
(define-module epub
  (use srfi-1)
  (use srfi-19)
  (use sxml.serializer)
  (use text.tree)
  (use gauche.collection)
  (use gauche.parameter)
  (use gauche.charconv)
  (use zip-archive)
  (use rfc.zlib)
  (use rfc.md5)
  (export container
          mimetype
          style-sheet
          line->paragraph
          epubize
          *fsencode*))

(select-module epub)

(define *fsencode* (make-parameter 'utf8))

(define (fsencode str)
  (ces-convert str (gauche-character-encoding) (*fsencode*)))

(define (sanitize title)
  (regexp-replace-all #/[\/()"?<>|:;*~\r\n]/ title ""))

(define (style-sheet vertical lineheight)
  #`"
,(if vertical \"html {
 -epub-writing-mode: vertical-rl;
}\" \"\")
ol {
 list-style-type: none;
 padding: 0;
 margin: 0;
}
p {
 margin: 0;
,(if lineheight #`\"line-height: ,|lineheight|%\" \"\")
}
body {
 margin: 0;
 padding: 0;
}")

(define (line->paragraph a)
  (reverse!
   (receive (x y)
       (fold2 (lambda(elt b para)
                (if (and (pair? elt) (equal? (car elt) 'br))
                    (values (cons (cons 'p (reverse! para)) b) '())
                    (values b (cons elt para))))
              '() '()
              a)
     (if (null? y) x (cons (cons 'p (reverse! y)) x)))))

(define (container)
  (with-output-to-string
    (^[]
      (display "<?xml version='1.0' ?>")
      (write-tree
       (srl:parameterizable
        `(*TOP*
          (container
           (@ (xmlns "urn:oasis:names:tc:opendocument:xmlns:container")
              (version "1.0"))
           (rootfiles
            (rootfile (@ (full-path "OPS/content.opf")
                         (media-type "application/oebps-package+xml"))))))
       #f
       '(indent . #f)
       )))))

(define (mimetype)
  "application/epub+zip")

(define (title-page title author ex)
  (with-output-to-string
    (^[]
      (display "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n")
      (display "<!DOCTYPE html>\n")
      (write-tree
       (srl:parameterizable
        `(*TOP*
          (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                   (xml:lang "ja"))
                (head (title ,title)
                      (link (@ (rel "stylesheet")
                               (type "text/css")
                               (href "style.css"))))
                (body
                 (h1 ,title)
                 (h2 "作者")
                 (p ,author)
                 ,@(if ex
                      `((h2 "あらすじ") (p ,ex))
                      '()))))
        #f
        '(omit-xml-declaration . #t)
        '(indent . #f)
        )))))

(define (uuid4 src)
  (let1 v (digest-string <md5> src)
    (string-byte-set! v 6 (logior (logand (string-byte-ref v 6) #x0f) #x40))
    (string-byte-set! v 8 (logior (logand (string-byte-ref v 8) #x3f) #x80))
    (let1 m (#/^([[:xdigit:]]{8})([[:xdigit:]]{4})([[:xdigit:]]{4})([[:xdigit:]]{4})([[:xdigit:]]{12})/ (digest-hexify v))
      #`",(m 1)-,(m 2)-,(m 3)-,(m 4)-,(m 5)")))

(define (opf topic id title author ex series :key (vertical #f) (no-toc #f))
  (define play-order (make-counter))
  (define chapter-order (make-counter))

  (define manifest
    (let1 counter (make-counter)
      (filter-map
       (^x (if (pair? x)
               `(item (@ (id ,#`"id_,(counter)")
                         (href ,#`",(car x).xhtml")
                         (media-type "application/xhtml+xml")))
               #f))
       topic)))

  (define spine
    (let1 counter (make-counter)
      (filter-map
       (^x (if (pair? x)
               `(itemref (@ (idref ,#`"id_,(counter)")))
               #f))
       topic)))
  
  (with-output-to-string
    (^[]
      (display "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n")
      (write-tree
       (srl:parameterizable
        `(*TOP*
          (@@ (*NAMESPACES*
               (dc "http://purl.org/dc/elements/1.1/")
               (dcterms "http://purl.org/dc/terms/")
               (xsi "http://www.w3.org/2001/XMLSchema-instance")
               (opf "http://www.idpf.org/2007/opf")))
          (package
           (@ (xmlns "http://www.idpf.org/2007/opf")
              (unique-identifier "BookId")
              (version "3.0"))
           (metadata
            (dc:title ,title)
            (dc:creator ,author)
            (dc:language "ja")
            (dc:identifier (@ (id "BookId")) ,#`"urn:uuid:,(uuid4 id)")
            (dc:subject "General Fiction")
            ,@(if ex
                  `((dc:description ,ex))
                  '())
            (meta (@ (property "dcterms:modified"))
                  ,(date->string (current-date) "~Y-~m-~dT~H:~M:~SZ"))
            ,@(if-let1 series-title series
                `((meta (@ (name "calibre:series") (content ,series-title)))
                  (meta (@ (name "calibre:series_index") (content "0"))))
                '()))
           (manifest
            (item (@ (id "toc")
                     (href "toc.ncx")
                     (media-type "application/x-dtbncx+xml")))
            (item (@ (id "nav")
                     (href "nav.xhtml")
                     (media-type "application/xhtml+xml")
                     (properties "nav")))
            (item (@ (id "title")
                     (href "title.xhtml")
                     (media-type "application/xhtml+xml")))
            (item (@ (id "style")
                     (href "style.css")
                     (media-type "text/css")))
            ,@manifest)
           (spine (@ (toc "toc")
                     ,@(if vertical
                           '((page-progression-direction "rtl"))
                           '()))
                  (itemref (@ (idref "title")))
                  ,@(if no-toc
                        '()
                        '((itemref (@ (idref "nav")))))
                  ,@spine)
           (guide
            (reference (@ (type "title")
                          (title "title")
                          (href "title.xhtml"))))
           ))
        #f
        '(indent . #f)
        )))))

(define (make-counter)
  (let1 c 0
    (lambda() (inc! c) c)))

(define (ncx topic id title)
  (define play-order (make-counter))
  (define chapter-order (make-counter))

  (define (navPoint x)
    (let1 order (play-order)
      `(navPoint (@ (id ,#`"id_,|order|")
                    (playOrder ,(format #f "~a" order)))
                 (navLabel (text ,(second x)))
                 (content (@ (src ,#`",(car x).xhtml"))))))
  
  (define (nav-grouping x)
    (let loop ((x x))
      (cond [(null?  x) '()]
            [(string? (car x))
             (receive (a b) (span pair? (cdr x))
               (cons
                `(navPoint (@ (id ,#`"chapter_,(chapter-order)"))
                           (navLabel (text ,(car x)))
                           (content
                            (@ (src ,#`",(caar a).xhtml")))
                           ,@(map navPoint a))
                (loop b)))]
            [(pair? (car x))
             (map navPoint x)])))

  (define nav (nav-grouping topic))
  
  (with-output-to-string
    (^[]
      (display "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n")
      (write-tree
       (srl:parameterizable
        `(*TOP*
          (ncx (@ (xmlns "http://www.daisy.org/z3986/2005/ncx/")
                  (xml:lang "en")
                  (version "2005-1"))
            (head
             (meta (@ (name "dtb:uid") (content ,id)))
             (meta (@ (name "dtb:depth") (content "1")))
             (meta (@ (name "dtb:totalPageCount") (content "0")))
             (meta (@ (name "dtb:maxPageNumber") (content "0"))))
            (docTitle
             (text ,title))
            (navMap ,@nav)
            ))
        #f
        '(indent . #f)
        )))))

(define (topic-page topic)
  (with-output-to-string
    (^[]
      (display "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n")
      (display "<!DOCTYPE html>\n")
      (write-tree
       (srl:parameterizable
        `(*TOP*
          (@@ (*NAMESPACES*
               (epub "http://www.idpf.org/2007/ops")))
          (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                   (xml:lang "ja"))
                (head (title "目次")
                      (link (@ (rel "stylesheet")
                               (type "text/css")
                               (href "style.css"))))
                (body
                 (section
                  (@ (epub:type "frontmatter toc"))
                  (h1 "目次")
                  (nav (@ (epub:type "toc")
                          (id "toc"))
                       ,(topic-grouping topic))))))
        #f
        '(omit-xml-declaration . #t)
        '(indent . #f)
        )))))

(define (format-href x) #`",|x|.xhtml")

(define (format-link x)
  `(li (a (@ (href ,(format-href (car x)))) ,(second x))))

(define (topic-grouping x)
  (cons 'ol
        (let loop ((x x))
          (cond [(null?  x) '()]
                [(string? (car x))
                 (receive (a b) (span pair? (cdr x))
                   (cons
                    `(li (span ,(car x))
                         (ol ,@(map format-link a)))
                    (loop b)))]
                [(pair? (car x))
                 (map format-link x)]))))

(define (epubize novel-id title author ex series bodies
                 :key (vertical #f) (line-height #f) (no-toc #f))
  (call-with-output-zip-archive
   (fsencode
    (sanitize #`"[,|author|] ,|title|.epub"))
   (lambda(archive)
     (zip-add-entry archive "mimetype" (mimetype)
                    :compression-level Z_NO_COMPRESSION)
     (zip-add-entry archive "OPS/title.xhtml" (title-page title author ex)
                    :compression-level Z_BEST_COMPRESSION)
     (zip-add-entry archive "OPS/nav.xhtml" (topic-page bodies)
                    :compression-level Z_BEST_COMPRESSION)
     (zip-add-entry archive "OPS/style.css"
                    (style-sheet vertical line-height)
                    :compression-level Z_BEST_COMPRESSION)
     (zip-add-entry archive "META-INF/container.xml" (container)
                    :compression-level Z_BEST_COMPRESSION)
     (zip-add-entry archive "OPS/content.opf"
                    (opf bodies novel-id title author ex series
                         :vertical vertical :no-toc no-toc)
                    :compression-level Z_BEST_COMPRESSION)
     (zip-add-entry archive "OPS/toc.ncx" (ncx bodies novel-id title)
                    :compression-level Z_BEST_COMPRESSION)
     (for-each
      (lambda(x)
        (let ((pathname (car x))
              (title (cadr x))
              (body (caddr x)))
          (zip-add-entry archive #`"OPS/,|pathname|.xhtml"
                         (with-output-to-string
                           (^[]
                             (display "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n")
                             (display "<!DOCTYPE html>\n")
                             (write-tree
                              (srl:parameterizable
                               `(*TOP*
                                 (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                                          (xml:lang "ja"))
                                       (head
                                        (title ,title)
                                        (link (@ (rel "stylesheet")
                                                 (type "text/css")
                                                 (href "style.css"))))
                                       (body
                                        (h2 ,title)
                                        ,@body)))
                               #f
                               '(omit-xml-declaration . #t)
                               '(indent . #f)
                               ))))
                         :compression-level Z_BEST_COMPRESSION)))
      (filter pair? bodies))
     )))

(provide "epub")
