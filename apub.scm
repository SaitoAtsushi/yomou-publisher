#!/usr/bin/env gosh
;;; -*- mode:gauche; coding: utf-8 -*-
;;; Author: SAITO Atsushi

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
(use gauche.parameter)
(use gauche.parseopt)
(use srfi-27)
(use sxml.tools)
(use rfc.md5)
(use srfi-19)
(use srfi-13)
(use text.progress)

(add-load-path "." :relative)
(use zip-archive)
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
      (http-get "www.akatsuki-novels.com" path)
    (sys-sleep (option-wait-time))
    (unless (string=? "200" status)
      (error "Download error." path))
    (regexp-replace-all #/<br>/
      (regexp-replace-all #/&nbsp;/
        (regexp-replace-all #/<\/?(?:RP|RT|RUBY)>/
          (regexp-replace-all #/<rb>(.+?)<\/rb>/i body (cut <> 1))
          (lambda(x) (string-downcase (x 0))))
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

(define (rxmatch-item x)
  (cond ((x 1) => values)
        (else (cons (x 2) (x 3)))))

(define novel-list
  (let1 query #/<b>([^<]+)<\/b>|<td>(?:  )?<a href=\"([^"]+)\">([^<]+)<\/a> <\/td>/
    ($ generator->list $ gmap rxmatch-item $ grxmatch query $)))

(define (usage cmd)
  (print "usage: " (sys-basename cmd) " [option] novel-id ...\n\n"
         "options:\n"
         "  -v, --vertical             vertical writing mode\n"
         "  -l NUM, --lineheight=NUM   Specify percentage of line height (100-300)\n"
         "  -w NUM, --waittime=NUM     Downloading interval (Default is 2s)")
  (exit))

(define (path->page-number x)
  (let1 m (#/^\/stories\/view\/(\d+)\/novel_id~\d+$/ x)
    (m 1)))

(define (get-novels novel-id)
  (let* ((topic (download #`"/stories/index/novel_id~,|novel-id|/"))
         (topic-list (novel-list topic))
         (lst (filter-map (^x (and (pair? x) (car x))) topic-list))
         (prog (make-text-progress-bar :header novel-id
                                       :header-width 9
                                       :max-value (length lst)))
         (bodies (map
                  (^x
                   (if (pair? x) 
                       (let1 a (download (car x))
                         (prog 'inc 1)
                         (list (path->page-number (car x))
                               (cdr x)
                               (line->paragraph
                                ((sxpath "//div/node()") (novel-body a)))))
                       x))
                       topic-list))
         (info (download #`"/novels/view/,|novel-id|"))
         (title (novel-title topic))
         (author (novel-author topic))
         (ex (novel-ex info)))
    (prog 'finish)
    (values novel-id title author ex #f bodies)))

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

