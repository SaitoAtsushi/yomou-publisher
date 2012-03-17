#!/usr/bin/env gosh
;;; -*- mode:gauche; coding: utf-8 -*-
;;; Author: SAITO Atsushi

(define-constant *fsencode* 'Shift_JIS) ;; file-system encoding

(use rfc.http)
(use gauche.charconv)
(use sxml.sxpath)
(use sxml.serializer)
(use text.tree)
(use util.match)
(use srfi-1)
(use gauche.collection)
(use util.queue)
(use gauche.parameter)
(use srfi-27)
(use sxml.tools)
(use binary.pack)
(use rfc.zlib)
(use srfi-19)
(use srfi-60)
(use srfi-13)

(require "htmlprag") ;; http://www.neilvandyke.org/htmlprag/

(cond-expand
 [gauche.sys.threads
  (define *max-thread* (make-parameter 18))
  (use control.thread-pool)
  (define (parallel-map proc lst)
    (let1 pool (make-thread-pool (*max-thread*))
      (for-each (^x (add-job! pool (cut proc x) #t)) lst)
      (terminate-all! pool)
      (map! (cut ~ <> 'result) (queue->list (thread-pool-results pool)))
      ))]
 [else
  (define parallel-map map)])

(define (fsencode str)
  (ces-convert str (gauche-character-encoding) *fsencode*))

(define (sanitize title)
  (regexp-replace-all #/[\/()"?<>|:;\r\n]/ title ""))

(define (current-time/dos-format)
  (let* ((date (current-date))
         (year (date-year date))
         (month (date-month date))
         (day (date-day date))
         (hour (date-hour date))
         (minute (date-minute date))
         (second (date-second date)))
    (+ (ash (- year 1980) 25) (ash month 21) (ash day 16)
       (ash hour 11) (ash minute 5) (quotient second 2))))

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
    body))

(define novel-body (sxpath "//div[@id='novel_view']/node()"))
(define novel-subtitle (if-car-sxpath "//div[@class='novel_subtitle']/text()"))

(define novel-ex
  (let1 query (sxpath "//div[@class='novel_ex']/text()")
    (^x (apply string-append (query x)))))

(define novel-author
  (let ((query (if-car-sxpath "//div[@class='novel_writername']/a/text()"))
        (query2 (if-car-sxpath "//div[@class='novel_writername']/text()")))
    (^x (if-let1 author (query x)
          author
          ((#/\uff1a(.+)/ (query2 x)) 1)))))

(define novel-list (sxpath "//div[@class='novel_sublist']//a[starts-with(@href,'/')]/@href/text()"))

(define novel-title
  (let1 query (sxpath "//div[@class='novel_title']/text()")
    (^x (apply string-append (query x)))))

(define novel-series (if-car-sxpath "//div[@class='series']/a/text()"))

(define (topic-grouping x)
  (cond ((null?  x) '())
        ((eqv? 'h2 (caar x))
         (cons (car x) (topic-grouping (cdr x))))
        ((eqv? 'li (caar x))
         (receive (a b)
             (span (^x (eqv? (car x) 'li)) x)
           (cons (cons 'ul a) (topic-grouping b))))))

(define topic-item
  (let1 query (sxpath "//div[@class='novel_sublist']//node()[@class='chapter' or starts-with(@href,'/')]")
    (^x
     (topic-grouping
      (map
       (match-lambda
        (('td ('|@| . _) m) `(h2 ,m))
        (('a ('@ ('href (? string? (= #/^\/([^\/]+)\/(.+)\/$/ m)))) t)
         `(li (a (@ (href ,(format #f "~4,,,'0@a.xhtml" (m 2)))) ,t)))
        (a a) )
       (query x))))))

(define (title-page topic)
  (with-output-to-string
    (^[]
      (display "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n")
      (display "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n")
      (write-tree
       (srl:parameterizable
        `(*TOP*
          (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                   (xml:lang "ja"))
                (head (title ,(novel-title topic)))
                (body
                 (h1 ,(novel-title topic))
                 (h2 "作者")
                 (p ,(novel-author topic))
                 (h2 "あらすじ")
                 (p ,(novel-ex topic)))))
        #f
        '(omit-xml-declaration . #t)
        '(indent . #f)
        )))))

(define (topic-page topic)
  (with-output-to-string
    (^[]
     (display "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n")
     (display "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n")
     (write-tree
      (srl:parameterizable
       `(*TOP*
         (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                  (xml:lang "ja"))
               (head (title "目次")
                     (link (@ (rel "stylesheet")
                              (type "text/css")
                              (href "style.css"))))
               (body
                (h1 "目次")
                ,@(topic-item topic)
                )))
       #f
       '(omit-xml-declaration . #t)
       '(indent . #f)
       )))))

(define (opt topic id)
  (define manifest
    (let1 query (sxpath "//div[@class='novel_sublist']//a[starts-with(@href,'/')]")
      (map
       (match-lambda
        (('a ('@ ('href (? string? (= #/^\/([^\/]+)\/(.+)\/$/ m)))) t)
         `(item (@ (id ,(format #f "id_~4,,,'0@a" (m 2)))
                   (href ,(format #f "~4,,,'0@a.xhtml" (m 2)))
                   (media-type "application/xhtml+xml")))))
       (query topic))))

  (define spine
    (let1 query (sxpath "//div[@class='novel_sublist']//a[starts-with(@href,'/')]")
      (map
       (match-lambda
        (('a ('@ ('href (? string? (= #/^\/([^\/]+)\/(.+)\/$/ m)))) t)
         `(itemref (@ (idref ,(format #f "id_~4,,,'0@a" (m 2)))))))
       (query topic))))
      
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
             (version "2.0"))
          (metadata
           (dc:title ,(novel-title topic))
           (dc:creator ,(novel-author topic))
           (dc:language "ja")
           (dc:identifier (@ (id "BookId")) ,id)
           (dc:subject "General Fiction")
           (dc:description ,(novel-ex topic))
           ,@(if-let1 series-title (novel-series topic)
               `((meta (@ (name "calibre:series") (content ,series-title)))
                 (meta (@ (name "calibre:series_index") (content "0"))))
               '()))
          (manifest
           (item (@ (id "toc")
                    (href "toc.ncx")
                    (media-type "application/x-dtbncx+xml")))
           (item (@ (id "title")
                    (href "title.xhtml")
                    (media-type "application/xhtml+xml")))
           (item (@ (id "TableOfContents")
                    (href "topic.xhtml")
                    (media-type "application/xhtml+xml")))
           (item (@ (id "style")
                    (href "style.css")
                    (media-type "text/css")))
           ,@manifest)
          (spine (@ (toc "toc"))
            (itemref (@ (idref "title")))
            (itemref (@ (idref "TableOfContents")))
            ,@spine)
          (guide
           (reference (@ (type "title")
                         (title "title")
                         (href "title.xhtml")))
           (reference (@ (type "toc")
                         (title "Table of Contents")
                         (href "topic.xhtml"))))
          )))))))

(define (ncx topic id)
  (define nav
    (let1 query (sxpath "//div[@class='novel_sublist']//a[starts-with(@href,'/')]")
      (map
       (match-lambda
        (('a ('@ ('href (? string? (= #/^\/([^\/]+)\/(.+)\/$/ m)))) t)
         `(navPoint (@ (id ,(format #f "id_~4,,,'0@a" (m 2)))
                       (playOrder ,(format #f "~a" (m 2))))
            (navLabel (text ,t))
            (content (@ (src ,(format #f "~4,,,'0@a.xhtml" (m 2))))))))
       (query topic))))
  
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
             (text ,(novel-title topic)))
            (navMap
             ,@nav)
            )))))))


(define (line->para a)
  (reverse!
   (fold2 (lambda(elt b para)
            (if (and (pair? elt) (equal? (car elt) 'br))
                (values (cons (cons 'p (reverse! para)) b) '())
                (values b (cons elt para))))
          '() '()
          a)))

(define (style)
"ul {
 list-style-type: none;
 padding: 0;
 margin: 0;
}
p {
 margin: 0.3em 0 0.3em 0;
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
  (print "usage: " cmd " N-CODE")
  (exit))

(define (epubize n-code)
  (let* ((topic (html->sxml (download #`"/,|n-code|/")))
         (lst (novel-list topic))
         (bodies (parallel-map (^x (let1 a (html->sxml (download x))
                                     (list x (novel-subtitle a) (novel-body a))))
                               lst)))
    (zip-encode
     (fsencode (sanitize #`"[,(novel-author topic)] ,(novel-title topic).epub"))
    `(("mimetype" ,(mimetype) #f)
      ("OPS/title.xhtml" ,(title-page topic) #t)
      ("OPS/topic.xhtml" ,(topic-page topic) #t)
      ("OPS/style.css" ,(style) #t)
      ("META-INF/container.xml" ,(container) #t)
      ("OPS/content.opf" ,(opt topic n-code) #t)
      ("OPS/toc.ncx" ,(ncx topic n-code) #t)
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
                     (display "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n")
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
                                ,@(line->para body))))
                       #f
                       '(omit-xml-declaration . #t)
                       '(indent . #f)
                       ))))
                 #t
                 )))
             bodies)
      ))))

(define (main args)
  (when (> 2 (length args)) (usage (car args)))
  (for-each (compose epubize string-downcase) (cdr args)))
