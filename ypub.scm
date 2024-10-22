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
(use gauche.sequence)
(use srfi-27)
(use sxml.tools)
(use srfi-19)
(use srfi-60)
(use srfi-13)
(use text.progress)
(use www.cgi)
(use file.util)
(use rfc.json)
(use rfc.md5)
(use epub)

(define option-vertical (make-parameter #f))

(define option-no-image (make-parameter #f))

(define image-accum (make-parameter '()))

(define (style-sheet)
  #"
~(if (option-vertical) \"html {
 -epub-writing-mode: vertical-rl;
}\" \"\")
ol {
 list-style-type: none;
 padding: 0;
 margin: 0;
}
p {
 margin: 0;
}
body {
 margin: 0;
 padding: 0;
}")

(define-class <novel> ()
  ((title :init-keyword :title)
   (ncode :init-keyword :ncode)
   (author :init-keyword :author)
   (author-id :init-keyword :author-id :init-value #f)
   (author-furigana :init-keyword :author-furigana :init-value #f)
   (description :init-keyword :description)
   (number-of-episodes :init-keyword :noe)
   (update :init-keyword :update)
   (episodes :init-value #f)
   (images :inive-value '())))

(define-class <episode> ()
  ((title :init-keyword :title)
   (chapter :init-keyword :chapter :init-value #f)
   (body :init-keyword :body)))

(define-class <author> ()
  ((name :init-keyword :name)
   (furigana :init-keyword :furigana)))

(define (api ncode)
  (receive (status head body)
      (http-get "api.syosetu.com" `("/novelapi/api/"
                                    (out "json")
                                    (ncode ,ncode)
                                    (of "t-n-u-w-s-ga-nu")))
    (unless (string=? "200" status)
      (error "http error"))
    (let1 j (vector-ref (parse-json-string body) 1)
      (make <novel>
        :ncode ncode
        :title (assoc-ref j "title")
        :author (assoc-ref j "writer")
        :author-id (assoc-ref j "userid")
        :description (assoc-ref j "story")
        :noe (assoc-ref j "general_all_no")
        :update (string->date (assoc-ref j "novelupdated_at")
                              "~Y-~m-~d ~H:~M:~S"))
      )))

(define (author-api user-id)
  (receive (status head body)
      (http-get "api.syosetu.com" `("/userapi/api/"
                                    (out "json")
                                    (userid ,user-id)
                                    (of "n-y")))
    (unless (string=? "200" status)
      (error "http error"))
    (let1 j (vector-ref (parse-json-string body) 1)
      (make <author>
        :name (assoc-ref j "name")
        :furigana (assoc-ref j "yomikata"))
      )))

(define (path-split url)
  (let1 m (#/^https?:\/\/([^\/]+)(\/.+)$/ url)
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
                            #"/userpageimage/viewimage/icode/~|nsrc|"
                            :redirect-handler #f)
                (cgi-get-parameter "location" head))))
    (sxml:change-attr! x `(src ,#"~(image-download url)"))))

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

(define episode-html-collect
  (compose
   (cut regexp-replace-all #/<a href="\/\/\d+?.mitemin.net\/i\d+?\/" target="_blank">(.+?)<\/a>/ <> "\\1")
   (cut regexp-replace-all #/ border=0 \/>/ <> " />")))


(define (episode-body x)
  (rxmatch-cond
   ((rxmatch #/<div class="js-novel-text p-novel__text">(.+?)<\/div>/ x)
    (m _)
    ((sxpath "//div/node()")
     (image-pack
      (ssax:xml->sxml (open-input-string
                       (episode-html-collect m))
                      '()))))
   (else #f)))

(define (episode-title x)
  (if-let1 m (#/<h1 class="p-novel__title p-novel__title--rensai">(.+?)<\/h1>/ x)
    (m 1)
    (error "episode-title")))

(define (episode-chapter x)
  (if-let1 m (#/<p class="chapter_title">(.+?)<\/p>/ x)
    (m 1)
    #f))

(define-method get-episode ((novel <novel>) (episode-number <integer>))
  (receive (status head body)
      (http-get "ncode.syosetu.com" #"/~(~ novel 'ncode)/~|episode-number|/")
    (unless (string=? "200" status) (error "http error"))
    (let1 src (regexp-replace-all #/border="0"/ (regexp-replace-all #/<\/?rb>/ body "") "")
      (make <episode>
        :title (episode-title src)
        :chapter (episode-chapter src)
        :body (episode-body src))
      )))

(define (get-novel ncode)
  (parameterize ((image-accum '()))
    (let* ((info (api ncode))
           (author-info (author-api (~ info 'author-id)))
           (num (~ info 'number-of-episodes))
           (episodes
            (do ((i 1 (+ i 1))
                 (result '() (cons (get-episode info i) result)))
                ((> i num) (reverse! result)))))
      (set! (~ info 'episodes) episodes)
      (set! (~ info 'images) (image-accum))
      (set! (~ info 'author) (~ author-info 'name))
      (set! (~ info 'author-furigana) (~ author-info 'furigana))
      info)))

(define (usage cmd)
  (print "usage: " (sys-basename cmd) " [option] N-CODE ...\n\n"
         "options:\n"
         "  -v, --vertical             vertical writing mode\n"
         "  -n, --noimage              Deny illustration\n"
         "  -w NUM, --waittime=NUM     Downloading interval (Default is 2s)")
  (exit))

(define (uuid4 src)
  (let1 v (digest-string <md5> src)
    (string-byte-set! v 6 (logior (logand (string-byte-ref v 6) #x0f) #x40))
    (string-byte-set! v 8 (logior (logand (string-byte-ref v 8) #x3f) #x80))
    (let1 m (#/^([[:xdigit:]]{8})([[:xdigit:]]{4})([[:xdigit:]]{4})([[:xdigit:]]{4})([[:xdigit:]]{12})/ (digest-hexify v))
      #`",(m 1)-,(m 2)-,(m 3)-,(m 4)-,(m 5)")))

(define (sanitize title)
  (regexp-replace-all #/[\/()"?<>|:;*~\r\n]/ title "_"))

(define (main args)
  (let-args (cdr args)
      ((vertical "v|vertical" => (cut option-vertical #t))
       (noimage "n|noimage" => (cut option-no-image #t))
       . rest)
    (when (> 2 (length args)) (usage (car args)))
    (for-each (lambda(ncode)
                (let* ((novel (get-novel ncode))
                       (book (book-open
                              #"[~(sanitize (~ novel 'author))] ~(sanitize (~ novel 'title)).epub"
                              (uuid4 ncode)
                              (~ novel 'title)
                              (~ novel 'author)
                              (~ novel 'description)
                              (if (option-vertical) "rtl" "default")
                              :author-furigana (~ novel 'author-furigana)
                              :source #"https://ncode.syosetu.com/~|ncode|/")))
                  (add-style book "style.css" (style-sheet))
                  (let1 prev #f
                    (for-each-with-index
                     (lambda(num episode)
                       (unless (equal? prev (~ episode 'chapter))
                         (if (equal? prev #f)
                             (chapter-begin book (~ episode 'chapter))
                             (begin (chapter-end book)
                                    (chapter-begin book (~ episode 'chapter)))))
                       (add-html book
                                 (format #f "~4,'0d.xhtml" num)
                                 (~ episode 'title)
                                 `(html (|@|
                                         (xmlns "http://www.w3.org/1999/xhtml")
                                         (xml:lang "ja"))
                                        (head (title ,(~ episode 'title))
                                              (link (|@|
                                                     (rel "stylesheet")
                                                     (type "text/css")
                                                     (href "style.css"))))
                                        (body
                                         (h1 ,(~ episode 'title))
                                         ,@(~ episode 'body))))
                       (set! prev (~ episode 'chapter)))
                     (~ novel 'episodes))
                    (when prev (chapter-end book))
                    )
                  (for-each (lambda(image)
                              (add-image book (car image) (cdr image)))
                            (~ novel 'images))
                  (book-close book)))
              rest)))
