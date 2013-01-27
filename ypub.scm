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

(add-load-path "." :relative)
(use epub)

;; file-system encoding
(*fsencode*
 (cond-expand (gauche.os.windows 'Shift_JIS)
              (else 'utf8)))

(define option-vertical (make-parameter #f))

(define option-wait-time (make-parameter 2))

(define (limitter-lineheight x)
  (unless (<= 100 x 300) (error "Lineheight must be between 100 to 300."))
  x)

(define option-lineheight (make-parameter 150 limitter-lineheight))

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

(define (rxmatch-item x)
  (cond ((x 1) => values)
        (else (cons (x 2) (x 3)))))

(define novel-list
  (let1 query #/<tr><td class=\"chapter\" colspan=\"4\">([^<]+)<\/td><\/tr>|<td class=\"(?:period|long)_subtitle\"><a href=\"([^\"]+)\">([^<]+)<\/a><\/td>/
    ($ generator->list $ gmap rxmatch-item $ grxmatch query $)))

(define (usage cmd)
  (print "usage: " (sys-basename cmd) " [option] N-CODE ...\n\n"
         "options:\n"
         "  -v, --vertical             vertical writing mode\n"
         "  -l NUM, --lineheight=NUM   Specify percentage of line height (100-300)\n"
         "  -w NUM, --waittime=NUM     Downloading interval (Default is 2s)")
  (exit))

(define (path->page-number x)
  (let1 m (#/^\/(?:[^\/]+)\/(.+)\/$/ x)
    (format #f "~4,,,'0@a" (m 1))))
  
(define (get-novels n-code)
  (let* ((topic (download #`"/,|n-code|/"))
         (topic-list (novel-list topic))
         (lst (filter-map (^x (and (pair? x) (car x))) topic-list))
         (prog (make-text-progress-bar :header n-code
                                       :header-width 9
                                       :max-value (length lst)))
         (bodies (map
                  (^x
                   (if (pair? x) 
                       (let1 a (download (car x))
                         (prog 'inc 1)
                         (list (path->page-number (car x))
                               (novel-subtitle a)
                               (line->paragraph
                                ((sxpath "//div/node()") (novel-body a)))))
                       x))
                  topic-list))
         (title (novel-title topic))
         (author (novel-author topic))
         (ex (novel-ex topic))
         (series (novel-series topic)))
    (prog 'finish)
    (values n-code title author ex series bodies)))

(define (main args)
  (guard (e ((condition-has-type? e <error>)
             (display (~ e 'message))))
    (let-args (cdr args)
        ((vertical "v|vertical" => (cut option-vertical #t))
         (lineheight "l|lineheight=n" => (cut option-lineheight <>))
         (waittime "w|waittime=n" => (cut option-wait-time <>))
         . rest)
      (when (> 2 (length args)) (usage (car args)))
      (for-each (compose (cut epubize <> <> <> <> <> <>
                              :vertical (option-vertical)
                              :line-height (option-lineheight))
                         get-novels string-downcase)
                rest))))
