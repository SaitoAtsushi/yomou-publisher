#!/usr/bin/env gosh
;;; -*- mode:gauche; coding: utf-8 -*-
;;; Author: SAITO Atsushi

(use rfc.http)
(use rfc.base64)
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
(use srfi-19)
(use srfi-60)
(use srfi-13)
(use text.progress)
(use www.cgi)

(add-load-path "." :relative)
(use epub)

(define option-vertical (make-parameter #f))

(define option-wait-time (make-parameter 2))

(define option-no-image (make-parameter #f))

(define image-accum (make-parameter '()))

(define (limitter-lineheight x)
  (unless (<= 100 x 300) (error "Lineheight must be between 100 to 300."))
  x)

(define option-lineheight (make-parameter 150 limitter-lineheight))

(define (download path)
  (receive (status head body)
      (http-get "ncode.syosetu.com" path)
    (unless (string=? "200" status) (error "http error"))
    (sys-sleep (option-wait-time))
    (regexp-replace-all #/border="0"/
      (regexp-replace-all #/<rb>(.+?)<\/rb>/ body (cut <> 1))
      "")))

(define (path-split url)
  (let1 m (#/^http:\/\/([^\/]+)(\/.+)$/ url)
    (values (m 1) (m 2))))

(define (image-download url)
  (rlet1 name ((#/\/([^\/]+)$/ url) 1)
    (unless (assoc-ref (image-accum) name #f)
      (receive (domain path)
          (path-split url)
        (receive (status head body)
            (http-get domain path)
          (push! (image-accum) (cons name body)))))))

(define (image-replace! x)
  (let* ((src (sxml:attr x 'src))
         (nsrc ((#/\/([^\/]+)\/$/ src) 1))
         (url (receive (status head body)
                  (http-get "5626.mitemin.net"
                            #`"/userpageimage/viewimage/icode/,|nsrc|"
                            :redirect-handler #f)
                (cgi-get-parameter "location" head))))
    (sxml:change-attr! x `(src ,#`",(image-download url)"))))

(define (image-replace-to-empty! x)
  (sxml:change-name! x 'span)
  (sxml:change-attrlist! x '())
  (sxml:change-content! x '("[画像省略]")))

(define image-pack
  (let1 query (sxpath "//img")
    (^[x]
      (let1 nodes (query x)
        (for-each
         (if (option-no-image) image-replace-to-empty! image-replace!)
         nodes)
        x))))

(define (novel-body x)
  (rxmatch-cond
    ((rxmatch #/<div id="novel_honbun" class="novel_view">(.+?)<\/div>/ x)
     (m _)
     (image-pack
      (ssax:xml->sxml (open-input-string
                       (regexp-replace-all #/ border=0 \/>/ m " />"))
                      '())))
    (else #f)))

(define (novel-subtitle x)
  (if-let1 m (#/<p class="novel_subtitle">([^<]+)<\/p>/ x)
    (m 1)
    (error "subtitle.")))

(define (novel-ex x)
  (if-let1 m (#/<div id="novel_ex">(.+?)<\/div>/ x)
    (regexp-replace-all #/<br \/>/ (m 1) "\n")
    #f))

(define (novel-author x)
  (if-let1 m (#/<div class="novel_writername">.+?(?:\uff1a|>)([^<]+)+<\// x)
    (m 1)
    (error "author")))

(define (novel-title x)
  (if-let1 m
      (#/<p class=\"novel_title\">([^<]+)<\/p>/ x)
    (m 1)
    (error "title")))

(define (novel-series x)
  (if-let1 m
      (#/<p class=\"series_title\"><a href=\"\/[^\/]+\/\">([^<]+)<\/a><\/p>/ x)
    (m 1)
    #f))

(define (rxmatch-item x)
  (cond ((x 1) => values)
        (else (cons (x 2) (x 3)))))

(define novel-list
  (let1 query #/<div class="chapter_title">([^<]+)<\/div>|<dd class="subtitle">\n<a href="([^"]+)">([^<]+)<\/a>\n<\/dd>/
    (lambda(x)
      (if (novel-body x)
          #f
          ($ generator->list $ gmap rxmatch-item $ grxmatch query x)))))

(define (usage cmd)
  (print "usage: " (sys-basename cmd) " [option] N-CODE ...\n\n"
         "options:\n"
         "  -v, --vertical             vertical writing mode\n"
         "  -n, --noimage              Deny illustration\n"
         "  -l NUM, --lineheight=NUM   Specify percentage of line height (100-300)\n"
         "  -w NUM, --waittime=NUM     Downloading interval (Default is 2s)")
  (exit))

(define (path->page-number x)
  (let1 m (#/^\/(?:[^\/]+)\/(.+)\/$/ x)
    (format #f "~4,,,'0@a" (m 1))))
  
(define (get-novels n-code)
  (let* ((topic (download #`"/,|n-code|/"))
         (topic-list (novel-list topic))
         (lst (if topic-list
                  (filter-map (^x (and (pair? x) (car x))) topic-list)
                  '(dummy)))
         (prog (make-text-progress-bar :header n-code
                                       :header-width 9
                                       :max-value (length lst)))
         (title (novel-title topic))
         (bodies (if topic-list
                     (map
                      (^x
                       (if (pair? x) 
                           (let1 a (download (car x))
                             (prog 'inc 1)
                             (list (path->page-number (car x))
                                   (novel-subtitle a)
                                   (line->paragraph
                                    ((sxpath "//div/node()") (novel-body a)))))
                           x))
                      topic-list)
                     (begin
                       (prog 'inc 1)
                       `(("0000"
                          ,title
                          ,(line->paragraph
                            ((sxpath "//div/node()") (novel-body topic))))))))
         (author (novel-author topic))
         (ex (novel-ex topic))
         (series (novel-series topic)))
    (prog 'finish)
    (cons* n-code title author ex series bodies (if ex '() '(:no-toc #t)))))

(define (main args)
  (guard (e ((condition-has-type? e <error>)
             (display (~ e 'message))))
    (let-args (cdr args)
        ((vertical "v|vertical" => (cut option-vertical #t))
         (lineheight "l|lineheight=n" => (cut option-lineheight <>))
         (waittime "w|waittime=n" => (cut option-wait-time <>))
         (noimage "n|noimage" => (cut option-no-image #t))
         . rest)
      (when (> 2 (length args)) (usage (car args)))
      (for-each (lambda(ncode)
                  (parameterize ((image-accum '()))
                    (let1 novel-data ($ get-novels $ string-downcase ncode)
                      (apply
                       (cut epubize <> <> <> <> <> <>
                            :vertical (option-vertical)
                            :line-height (option-lineheight)
                            :images (image-accum)
                            <...>)
                       novel-data))))
                rest))))
