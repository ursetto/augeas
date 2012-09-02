#> #include <augeas.h> <#

(use lolevel) ;; free

(define-syntax begin0                 ; multiple values discarded
  (syntax-rules () ((_ e0 e1 ...)
                    (let ((tmp e0)) e1 ... tmp))))

(define-record augeas ptr)
(define-foreign-type augeas (c-pointer "augeas") (lambda (a) (augeas-ptr a)))

(define _aug_init (foreign-lambda (c-pointer "augeas") aug_init c-string c-string int))
(define _aug_get (foreign-lambda int aug_get augeas c-string (c-pointer c-string)))
(define _aug_set (foreign-lambda int aug_set augeas c-string c-string))
(define _aug_setm (foreign-lambda int aug_setm augeas c-string c-string c-string))
(define _aug_rm (foreign-lambda int aug_rm augeas c-string))
(define _aug_match (foreign-lambda int aug_match augeas c-string (c-pointer (c-pointer c-string))))
(define _aug_error (foreign-lambda int aug_error augeas))  ;; error code
(define _aug_error_message (foreign-lambda c-string aug_error_message augeas))  ;; human-readable error
(define _aug_error_minor_message (foreign-lambda c-string aug_error_minor_message augeas)) ;; elaboration of error message
(define _aug_error_details (foreign-lambda c-string aug_error_details augeas))  ;; human-readable details

(define (aug-init #!key root loadpath (flags 0))
  (make-augeas (or (_aug_init root loadpath flags)
                   (error 'aug-init "initialization failed"))))
(define (aug-get a path)
  (let-location ((v c-string))
    (let ((rc (_aug_get a path #$v)))
      (if (< rc 0)
          (augeas-error a 'aug-get path)
          v))))
(define (aug-exists? a path)
  (let ((rc (_aug_get a path #f)))
    (if (< rc 0)
        (augeas-error a 'aug-exists? path)
        (> rc 0))))
(define (aug-set! a path val)
  (let ((rc (_aug_set a path val)))
    (if (< rc 0)
        (augeas-error a 'aug-set! path)
        (void))))
(define (aug-set-multiple! a base sub value)
  (let ((rc (_aug_setm a base sub value)))
    (if (< rc 0)
        (augeas-error a 'aug-set-multiple! base sub)
        rc)))
(define (aug-remove! a path)
  (let ((rc (_aug_rm a path)))
    (if (< rc 0)
        (augeas-error a 'aug-remove! path)
        rc)))
(define (aug-match-count a path)
  (let ((rc (_aug_match a path #f)))
    (if (< rc 0)
        (augeas-error a 'aug-match-count path)
        rc)))
(define (aug-match a path)
  (define _aug_match_index
    (foreign-lambda* c-string* (((c-pointer c-string) v) (int i))
      "return(v[i]);"))
  (let-location ((v c-pointer))
    (let ((rc (_aug_match a path #$v)))
      (when (< rc 0)
        (augeas-error a 'aug-match path))
      (begin0
          (list-tabulate rc (lambda (i) (_aug_match_index v i)))
        (free v)))))  ;; free array; elts were freed by c-string*.

(define (augeas-error a loc . args)   ;; internal: raise augeas error
  (abort
   (make-composite-condition
    (make-property-condition 'exn
                             'location loc
                             'message (_aug_error_message a)
                             'arguments args)
    (make-property-condition 'augeas
                             'code (_aug_error a)
                             ;; human-readable code symbol (pathx)? or add a property condition for it?
                             'message (_aug_error_message a)
                             'minor-message (_aug_error_minor_message a)
                             'details (_aug_error_details a))
    (make-property-condition 'unknown))))

