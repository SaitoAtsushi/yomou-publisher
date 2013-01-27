#!/usr/bin/env gosh
;;; -*- mode:gauche; coding: utf-8 -*-
;;; Author: SAITO Atsushi

(define-constant *fsencode*  ;; file-system encoding
  (cond-expand (gauche.os.windows 'Shift_JIS)
               (else 'utf8)))

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
(use rfc.zlib)
(use srfi-27)
(use sxml.tools)
(use binary.pack)
(use rfc.md5)
(use srfi-19)
(use srfi-60)
(use srfi-13)

(add-load-path "." :relative)
(use zip-archive)
(use epub)

(define option-vertical (make-parameter #f))

(define (limitter-lineheight x)
  (unless (<= 100 x 300) (error "Lineheight must be between 100 to 300."))
  x)

(define option-lineheight (make-parameter 150 limitter-lineheight))

(define (fsencode str)
  (ces-convert str (gauche-character-encoding) *fsencode*))

(define (sanitize title)
  (regexp-replace-all #/[\/()"?<>|:;\r\n]/ title ""))

(define (download path)
  (print "download " path)
  (receive (status head body)
      (http-get "www.akatsuki-novels.com" path)
    (sys-sleep 2)
    (unless (string=? "200" status)
      (error "Download error." path))
    (regexp-replace-all #/<br>/
      (regexp-replace-all #/&nbsp;/
        (regexp-replace-all #/<rb>(.+?)<\/rb>/i body (cut <> 1))
        " ")
      "<br />")))

(define (novel-body x)
  (define query #/(?:<div><b>前書き<\/b><\/div><div class="body-novel">[^"]+<\/div>.+?)?(<div class="body-novel">.+?<\/div>)/)
  (ssax:xml->sxml (open-input-string ((query x)1)) '()))

(define (novel-body1 x)
  (define query #/(?:<div><b>前書き<\/b><\/div><div class="body-novel">[^"]+<\/div>.+?)?(<div class="body-novel">.+?<\/div>)/)
  ((query x)1))

(define (novel-subtitle x)
  (if-let1 m (#/<h2>(.+?)<\/h2>/ x)
    (m 1)
    (error "miss match subtitle.")))

(define (novel-ex x)
  (define query (sxpath "//td/text()"))
  (if-let1 m (#/<td class="data" colspan="2">(.*?)<\/td>/ x)
    (string-join
     (query
      (ssax:xml->sxml (open-input-string (m 0)) '()))
     "\n")
    (error "miss match ex.")))

(define (novel-author x)
  (if-let1 m (#/<a href="\/users\/view\/\d+">([^<]+)<\/a>/ x)
    (m 1)
    (error "miss match author.")))

(define (novel-title x)
  (if-let1 m
      (#/<a href="[^"]+" class="LookNovel" id="LookNovel">([^<]+)<\/a><\/h3>/ x)
    (m 1)
    (error "miss match title.")))

(define (format-href x)
  (if-let1 m (#/^\/stories\/view\/(\d+)\/novel_id~\d+$/ x)
    (format #f "~a.xhtml" (m 1))
    (error "miss match href.")))

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
  (let1 query #/<b>([^<]+)<\/b>|<td>(?:  )?<a href=\"([^"]+)\">([^<]+)<\/a> <\/td>/
    ($ generator->list $ gmap rxmatch-item $ grxmatch query $)))

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

(define (opf topic id title author ex)
  (define manifest
    (filter-map
     (^x (if (pair? x)
             (if-let1 m (#/^\/stories\/view\/(\d+)\/novel_id~\d+$/ (car x))
               `(item (@ (id ,(format #f "id_~a" (m 1)))
                         (href ,(format #f "~a.xhtml" (m 1)))
                         (media-type "application/xhtml+xml")))
               (error "miss match."))
             #f))
     topic))

  (define spine
    (filter-map
     (^x (if (pair? x)
             (let1 m (#/^\/stories\/view\/(\d+)\/novel_id~\d+$/ (car x))
               `(itemref (@ (idref ,(format #f "id_~a" (m 1))))))
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
            )
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

(define (ncx topic id title)
  (define counter
    (let ((x 0))
      (lambda() (inc! x) x)))
  
  (define (navPoint x)
    (let1 m (#/^\/stories\/view\/(\d+)\/novel_id~\d+$/ (car x))
      `(navPoint (@ (id ,(format #f "id_~a" (m 1)))
                    (playOrder ,(x->string (counter))))
                 (navLabel (text ,(cdr x)))
                 (content (@ (src ,(format #f "~a.xhtml" (m 1))))))))

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
                                ,(let1 m (#/^\/stories\/view\/(\d+)\/novel_id~\d+$/ (caar a))
                                   (format #f "~4,,,'0@a.xhtml" (m 1))))))
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
            )))))))

(define (usage cmd)
  (print "usage: " (sys-basename cmd) " [option] novel-id ...\n\n"
         "options:\n"
         "  -v, --vertical             vertical writing mode\n"
         "  -l NUM, --lineheight=NUM   Specify percentage of line height (100-300)\n")
  (exit))

(define (zip-encode filename lst)
  (let1 za (open-output-zip-archive filename)
    (map(^x (zip-add-entry za (first x) (second x)
                           :compression-level (if (third x)
                                                  Z_BEST_COMPRESSION
                                                  Z_NO_COMPRESSION)))
        lst)
    (zip-close za)))

(define (epubize novel-id)
  (let* ((topic (download #`"/stories/index/novel_id~,|novel-id|/"))
         (topic-list #?=(novel-list topic))
         (lst (filter-map (^x (and (pair? x) (car x))) topic-list))
         (bodies (map
                  (^x (let1 a (download x)
                        (list x (novel-subtitle a) (novel-body a))))
                  lst))
         (info (download #`"/novels/view/,|novel-id|"))
         (title (novel-title topic))
         (author (novel-author topic))
         (ex (novel-ex info)))
    (zip-encode
     (fsencode (sanitize #`"[,(novel-author topic)] ,(novel-title topic).epub"))
     `(("mimetype" ,(mimetype) #f)
       ("OPS/title.xhtml" ,(title-page title author ex) #t)
       ("OPS/nav.xhtml" ,(topic-page topic-list) #t)
       ("OPS/style.css" ,(style-sheet (option-vertical) (option-lineheight)) #t)
       ("META-INF/container.xml" ,(container) #t)
       ("OPS/content.opf" ,(opf topic-list novel-id title author ex) #t)
       ("OPS/toc.ncx" ,(ncx topic-list novel-id title) #t)
       ,@(map (^x
               (let ((pathname (car x))
                     (title (cadr x))
                     (body (caddr x)))
                 (list
                  (rxmatch-case pathname
                    (#/^\/stories\/view\/(\d+)\/novel_id~\d+$/ (#f f)
                     (format #f "OPS/~a.xhtml" f)))
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
                             ,@(line->paragraph
                                ((sxpath "//div/node()") body)))))
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
         . rest)
      (when (> 2 (length args)) (usage (car args)))
      (for-each (compose epubize string-downcase) rest))))
