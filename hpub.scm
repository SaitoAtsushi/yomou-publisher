#!/usr/bin/env gosh
;;; -*- mode:gauche; coding: utf-8 -*-
;;; Author: SAITO Atsushi

(use rfc.http)
(use sxml.sxpath)
(use sxml.ssax)
(use srfi-1)
(use gauche.collection)
(use gauche.generator)
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

;;; For parallel download. But, this is not used now.
;; (cond-expand
;;  [gauche.sys.threads
;;   (define *max-thread* (make-parameter 10))
;;   (use control.thread-pool)
;;   (define (parallel-map proc lst)
;;     (let1 pool (make-thread-pool (*max-thread*))
;;       (for-each (^x (add-job! pool (cut proc x) #t)) lst)
;;       (terminate-all! pool)
;;       (map! (cut ~ <> 'result) (queue->list (thread-pool-results pool)))
;;       ))]
;;  [else
;;   (define parallel-map map)
;;   ])

(define (download path)
  (receive (status head body)
      (http-get "syosetu.org" path)
    (unless (string=? "200" status) (error "http error"))
    (sys-sleep (option-wait-time))
    (regexp-replace-all #/<rb>(.*?)<\/rb>/ body (cut <> 1))))

(define (path-split url)
  (let1 m (#/^http:\/\/([^\/]+)(\/.+)$/ url)
    (values (m 1) (m 2))))

(define (split-line x)
  (receive (before after)
      (break (pa$ equal? '(br)) x)
    (if (null? after)
        (list `(p ,@x))
        (cons `(p ,@before) (split-line (cdr after)))
        )))

(define (split-br x)
  (receive (before after)
      (string-scan x "<br>" 'both)
    (if before
        (string-append before "\n" (split-br after))
        x)
    ))

(define (novel-body x)
  (let* ((query (sxpath "//div/node()"))
         (m (#/<font size=\+1>[^<]+<\/font><BR><BR>\n(.+?)<BR>/ x))
         (sx (ssax:xml->sxml (open-input-string
                              (string-append "<div>"
                                (regexp-replace-all #/&(?!amp;|quot;|lt;|gt;)/
                                  (regexp-replace-all #/<br>/
                                                      (m 1)
                                                      "<br />")
                                  "&amp;")
                                "</div>"))
                             '())))
   (split-line (query sx))))

(define (novel-subtitle x)
  (let1 m (#/<font size=\+1>([^<]+)<\/font>/ x)
    (m 1)))

(define (novel-ex x)
  (let1 m (#/<\/div>\n<div class="ss">(.+?)\n<hr / x)
    (split-br (m 1))))

(define (novel-author x)
  (let1 m (#/<a href=\.\.\/\.\.\/\?mode=user&uid=\d+>([^<]+)<\/a>/ x)
    (m 1)))

(define (novel-title x)
  (let1 m
      (#/<title>(.[^<]+)<\/title>/ x)
    (m 1)))

(define (rxmatch-item x)
  (cond ((x 1) => values)
        (else (cons (x 2) (x 3)))))

(define novel-list
  (let1 query #/<strong>([^<]+)<\/strong>|<a href=\.\/([^ >]+) [^>]+>([^<]+)<\/a>/
    ($ generator->list $ gmap rxmatch-item $ grxmatch query $)))

(define (usage cmd)
  (print "usage: " (sys-basename cmd) " [option] N-CODE ...\n\n"
         "options:\n"
         "  -v, --vertical             vertical writing mode\n"
         "  -l NUM, --lineheight=NUM   Specify percentage of line height (100-300)\n")
  (exit))

(define (path->page-number x)
  (let1 m (#/^([^\.]+)\.html$/ x)
    (format #f "~4,,,'0@a" (m 1))))

(define (get-novel novel-id)
  (let* ((topic (download #`"/Novel/,|novel-id|/"))
         (topic-list (novel-list topic))
         (lst (filter-map (^x (and (pair? x) (car x))) topic-list))
         (prog (make-text-progress-bar :header novel-id
                                       :header-width 9
                                       :max-value (length lst)))
         (bodies (map
                  (^x
                   (if (pair? x) 
                       (let1 a (download #`"/Novel/,|novel-id|/,(car x)")
                         (prog 'inc 1)
                         (list (path->page-number (car x))
                               (novel-subtitle a)
                               (novel-body a)))
                       x))
                  topic-list))
         (title (novel-title topic))
         (author (novel-author topic))
         (ex (novel-ex topic)))
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
                         get-novel string-downcase)
                rest))))
