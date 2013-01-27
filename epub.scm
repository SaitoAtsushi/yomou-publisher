
(define-module epub
  (use sxml.serializer)
  (use text.tree)
  (use gauche.collection)
  (export container
          mimetype
          style-sheet
          line->paragraph
          title-page))

(select-module epub)

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
                         (media-type "application/oebps-package+xml")))))))))))

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
                 (h2 "あらすじ")
                 (p ,ex))))
        #f
        '(omit-xml-declaration . #t)
        '(indent . #f)
        )))))

(provide "epub")
