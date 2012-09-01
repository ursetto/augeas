#> #include <augeas.h> <#

(define-record augeas ptr)
(define-foreign-type augeas (c-pointer "augeas") (lambda (a) (augeas-ptr a)))

(define _aug_init (foreign-lambda (c-pointer "augeas") aug_init c-string c-string int))
(define _aug_get (foreign-lambda int aug_get augeas c-string (c-pointer c-string)))
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

