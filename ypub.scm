#!/usr/bin/env gosh
;;; -*- mode:gauche; coding: utf-8 -*-
;;; Author: SAITO Atsushi

(define-constant *fsencode* 'Shift_JIS) ;; file-system encoding

(use rfc.http)
(use rfc.base64)
(use util.digest)
(use gauche.charconv)
(use sxml.sxpath)
(use sxml.serializer)
(use sxml.ssax)
(use text.tree)
(use util.match)
(use srfi-1)
(use gauche.collection)
(use gauche.generator)
(use util.queue)
(use gauche.parameter)
(use gauche.parseopt)
(use srfi-27)
(use sxml.tools)
(use binary.pack)
(use rfc.zlib)
(use rfc.md5)
(use srfi-19)
(use srfi-60)
(use srfi-13)
(use text.progress)

(define option-vertical (make-parameter #f))

(define option-wait-time (make-parameter 2))

(define (limitter-lineheight x)
  (unless (<= 100 x 300) (error "Lineheight must be between 100 to 300."))
  x)

(define option-lineheight (make-parameter 150 limitter-lineheight))

(define (fsencode str)
  (ces-convert str (gauche-character-encoding) *fsencode*))

(define (sanitize title)
  (regexp-replace-all #/[\/()"?<>|:;\r\n]/ title ""))

(define (current-time/dos-format)
  (let1 date (current-date)
    (+ (ash (- (date-year date) 1980) 25)
       (ash (date-month date) 21)
       (ash (date-day date) 16)
       (ash (date-hour date) 11)
       (ash (date-minute date) 5)
       (quotient (date-second date) 2))))

(define (pk0304 port name body compressed timestamp checksum flag)
  (rlet1 lfh-position (port-tell port)
    (pack "VvvvVVVVvva*"
      (list #x04034b50 20  0 (if flag 8 0) timestamp checksum
            (string-size compressed) (string-size body) (string-size name)
            0 name)
      :output port)))

(define (pk0102 port name body compressed timestamp position checksum flag)
  (pack "VvvvvVVVVvvvvvVVa*"
    (list #x02014b50 20 20 0 (if flag 8 0) timestamp checksum
          (string-size compressed) (string-size body) (string-size name)
          0 0 0 0 0 position name)
        :output port))

(define (pk0506 port num eoc cd)
  (pack "VvvvvVVv" (list #x06054b50 0 0 num num (- eoc cd) cd 0) :output port))

(define (zip-encode output-filename lst)
  (receive (names bodies flags) (unzip3 lst)
    (let ((timestamp (current-time/dos-format))
          (compressed-bodies
           (map
            (^[body flag]
              (if flag
                  (deflate-string body :window-bits -15 :compression-level 9)
                  body))
                bodies flags))
          (checksums (map crc32 bodies)))
      (call-with-output-file output-filename
        (^p
         (let ((lfh-pos ;; local file headers
                (map (^[n b c checksum f]
                       (rlet1 x (pk0304 p n b c timestamp checksum f)
                         (display c p)))
                     names bodies compressed-bodies checksums flags))
               (cd-position (port-tell p))) ;;central directory structure
           (for-each
            (^[n b c pos checksum f]
              (pk0102 p n b c timestamp pos checksum f))
            names bodies compressed-bodies lfh-pos checksums flags)
           (let1 eoc-position (port-tell p) ;;end of central directory record
             (pk0506 p (length lst) eoc-position cd-position)
             )))))))

(define (download path)
  (receive (status head body)
      (http-get "ncode.syosetu.com" path)
    (unless (string=? "200" status) (error "http error"))
    (sys-sleep (option-wait-time))
    (regexp-replace-all #/<rb>(.+?)<\/rb>/ body (cut <> 1))))

(define (path-split url)
  (let1 m (#/^http:\/\/([^\/]+)(\/.+)$/ url)
    (values (m 1) (m 2))))

(define (image-download url)
  (receive (domain path)
      (path-split url)
    (receive (status head body)
        (http-get domain path)
      (string-append "data:"
                     (cadr (assoc "content-type" head))
                     ";base64,"
                     (base64-encode-string body)))))

(define (image-replace! x)
  (let* ((src (sxml:attr x 'src))
         (nsrc ((#/\/([^\/]+)\/$/ src) 1)))
    (sxml:change-attr! x `(src ,(image-download #`"http://5626.mitemin.net/userpageimage/viewimage/icode/,|nsrc|/")))))

(define image-pack
  (let1 query (sxpath "//img")
    (^[x]
      (rlet1 nodes (query x)
        (for-each image-replace! nodes)))))

(define (novel-body x)
  (let* ((m (#/<div class=\"novel_view\" id=\"novel_view\">(.+?)<\/div>/ x))
         (sx (ssax:xml->sxml (open-input-string
                              (regexp-replace-all #/ border=0 \/>/
                                                  (m 0)
                                                  " />"))
                             '())))
    (image-pack sx)
    sx))

(define (novel-subtitle x)
  (let1 m (#/<div class=\"novel_subtitle\">(?:<div class=\"chapter_title\">[^<]+<\/div>)?([^<]+)<\/div>/ x)
    (m 1)))

(define (novel-ex x)
  (let1 m (#/<div class=\"novel_ex\">([^<]+)<\/div>/ x)
    (m 1)))

(define (novel-author x)
  (let1 m (#/<div class="novel_writername">.+?(?:\uff1a|>)([^<]+)+<\// x)
    (m 1)))

(define (novel-title x)
  (let1 m
      (#/<div class=\"novel_title\">\r\n(?:<div .+?<\/a><\/div>\r\n)?([^<]+)\r\n<\/div>/ x)
    (m 1)))

(define (novel-series x)
  (if-let1 m
      (#/<div class=\"series\"><a href=\"\/[^\/]+\/\">([^<]+)<\/a><\/div>/ x)
    (m 1)
    #f))

(define (format-href x)
  (let1 m (#/^\/[^\/]+\/(.+)\/$/ x)
    (format #f "~4,,,'0@a.xhtml" (m 1))))

(define (format-link x)
  `(li (a (@ (href ,(format-href (car x)))) ,(cdr x))))

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

(define (rxmatch-item x)
  (cond ((x 1) => values)
        (else (cons (x 2) (x 3)))))

(define novel-list
  (let1 query #/<tr><td class=\"chapter\" colspan=\"4\">([^<]+)<\/td><\/tr>|<td class=\"(?:period|long)_subtitle\"><a href=\"([^\"]+)\">([^<]+)<\/a><\/td>/
    ($ generator->list $ gmap rxmatch-item $ grxmatch query $)))

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
                 (h2 "あらすじ")
                 (p ,ex))))
        #f
        '(omit-xml-declaration . #t)
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

(define (uuid4 src)
  (let1 v (digest-string <md5> src)
    (string-byte-set! v 6 (logior (logand (string-byte-ref v 6) #x0f) #x40))
    (string-byte-set! v 8 (logior (logand (string-byte-ref v 8) #x3f) #x80))
    (let1 m (#/^([[:xdigit:]]{8})([[:xdigit:]]{4})([[:xdigit:]]{4})([[:xdigit:]]{4})([[:xdigit:]]{12})/ (digest-hexify v))
      #`",(m 1)-,(m 2)-,(m 3)-,(m 4)-,(m 5)")))

(define (opf topic id title author ex series)
  (define manifest
    (filter-map
     (^x (if (pair? x)
             (let1 m (#/^\/([^\/]+)\/(.+)\/$/ (car x))
               `(item (@ (id ,(format #f "id_~4,,,'0@a" (m 2)))
                         (href ,(format #f "~4,,,'0@a.xhtml" (m 2)))
                         (media-type "application/xhtml+xml"))))
             #f))
     topic))

  (define spine
    (filter-map
     (^x (if (pair? x)
             (let1 m (#/^\/([^\/]+)\/(.+)\/$/ (car x))
               `(itemref (@ (idref ,(format #f "id_~4,,,'0@a" (m 2))))))
             #f))
     topic))
  
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
            (dc:description ,ex)
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
                     ,@(if (option-vertical)
                           '((page-progression-direction "rtl"))
                           '()))
                  (itemref (@ (idref "title")))
                  (itemref (@ (idref "nav")))
                  ,@spine)
           (guide
            (reference (@ (type "title")
                          (title "title")
                          (href "title.xhtml"))))
           )))))))

(define (line->para a)
  (reverse!
   (receive (x y)
       (fold2 (lambda(elt b para)
                (if (and (pair? elt) (equal? (car elt) 'br))
                    (values (cons (cons 'p (reverse! para)) b) '())
                    (values b (cons elt para))))
              '() '()
              a)
     (if (null? y) x (cons (cons 'p (reverse! y)) x)))))

(define (ncx topic id title)
  (define counter
    (let1 c 0
      (^[](inc! c) c)))

  (define (navPoint x)
    (let1 m (#/^\/([^\/]+)\/(.+)\/$/ (car x))
      `(navPoint (@ (id ,(format #f "id_~4,,,'0@a" (m 2)))
                    (playOrder ,(format #f "~a" (m 2))))
                 (navLabel (text ,(cdr x)))
                 (content (@ (src ,(format #f "~4,,,'0@a.xhtml" (m 2))))))))
  
  (define (nav-grouping x)
    (let loop ((x x))
      (cond [(null?  x) '()]
            [(string? (car x))
             (receive (a b) (span pair? (cdr x))
               (cons
                `(navPoint (@ (id ,#`"chapter_,(counter)"))
                           (navLabel (text ,(car x)))
                           (content
                            (@ (src
                                ,(let1 m (#/^\/([^\/]+)\/(.+)\/$/ (caar a))
                                   (format #f "~4,,,'0@a.xhtml" (m 2))))))
                     ,@(map navPoint a))
                (loop b)))]
            [(pair? (car x))
             (map navPoint x)])))

  (define nav (nav-grouping topic))
  
  (with-output-to-string
    (^[]
      (display "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n")
      (display "<!DOCTYPE ncx PUBLIC \"-//NISO//DTD ncx 2005-1//EN\" \"http://www.daisy.org/z3986/2005/ncx-2005-1.dtd\">")
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
            )))))))

(define (style)
  #`"
,(if (option-vertical) \"html {
 -epub-writing-mode: vertical-rl;
 direction: ltr;
 unicode-bidi:bidi-override;
}\" \"\")
ol {
 list-style-type: none;
 padding: 0;
 margin: 0;
}
p {
 margin: 0;
,(if (option-lineheight) #`\"line-height: ,(* 0.01 (option-lineheight))\" \"\")
}
body {
 margin: 0;
 padding: 0;
}")

(define (container)
  "<?xml version=\"1.0\" ?>
<container version=\"1.0\" xmlns=\"urn:oasis:names:tc:opendocument:xmlns:container\">
   <rootfiles>
      <rootfile full-path=\"OPS/content.opf\" media-type=\"application/oebps-package+xml\"/>
   </rootfiles>
</container>")

(define (mimetype) "application/epub+zip")

(define (usage cmd)
  (print "usage: " (sys-basename cmd) " [option] N-CODE ...\n\n"
         "options:\n"
         "  -v, --vertical             vertical writing mode\n"
         "  -l NUM, --lineheight=NUM   Specify percentage of line height (100-300)\n"
         "  -w NUM, --waittime=NUM     Downloading interval (Default is 2s)")
  (exit))

(define (epubize n-code)
  (let* ((topic (download #`"/,|n-code|/"))
         (topic-list (novel-list topic))
         (lst (filter-map (^x (and (pair? x) (car x))) topic-list))
         (prog (make-text-progress-bar :header n-code
                                       :header-width 9
                                       :max-value (length lst)))
         (bodies (map
                  (^x (let1 a (download x)
                        (prog 'inc 1)
                        (list x (novel-subtitle a) (novel-body a))))
                  lst))
         (title (novel-title topic))
         (author (novel-author topic))
         (ex (novel-ex topic))
         (series (novel-series topic)))
    (zip-encode
     (fsencode (sanitize #`"[,(novel-author topic)] ,(novel-title topic).epub"))
     `(("mimetype" ,(mimetype) #f)
       ("OPS/title.xhtml" ,(title-page title author ex) #t)
       ("OPS/nav.xhtml" ,(topic-page topic-list) #t)
       ("OPS/style.css" ,(style) #t)
       ("META-INF/container.xml" ,(container) #t)
       ("OPS/content.opf" ,(opf topic-list n-code title author ex series) #t)
       ("OPS/toc.ncx" ,(ncx topic-list n-code title) #t)
       ,@(map (^x
               (let ((pathname (car x))
                     (title (cadr x))
                     (body (caddr x)))
                 (list
                  (rxmatch-case pathname
                    (#/^\/([^\/]+)\/(.+)\/$/ (#f d f)
                     (format #f "OPS/~4,,,'0@a.xhtml" f)))
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
                             ,@(line->para ((sxpath "//div/node()")body)))))
                        #f
                        '(omit-xml-declaration . #t)
                        '(indent . #f)
                        ))))
                  #t)))
              bodies)
       ))))

(define (main args)
  (guard (e ((condition-has-type? e <error>)
             (display (~ e 'message))))
    (let-args (cdr args)
        ((vertical "v|vertical" => (cut option-vertical #t))
         (lineheight "l|lineheight=n" => (cut option-lineheight <>))
         (waittime "w|waittime=n" => (cut option-wait-time <>))
         . rest)
      (when (> 2 (length args)) (usage (car args)))
      (for-each (compose epubize string-downcase) rest))))
