#> #include <augeas.h> <#

;; todo: init flags; load; save; defvar; defnode; srun; rename; span (?); transform (?)
;; todo: init using AUG_NO_ERR_CLOSE (requires 0.10.0).  Easy, but I have no way to
;;       trigger a failure for testing.

(use foreigners)

(define-foreign-enum-type (augeus:errcode int 'unknown)
  (errcode->int int->errcode)
  ((nomem) AUG_ENOMEM)          ;; Out of memory
  ((internal) AUG_EINTERNAL)    ;; Internal error (bug)
  ((pathx) AUG_EPATHX)          ;; Invalid path expression
  ((nomatch) AUG_ENOMATCH)      ;; No match for path expression
  ((mmatch) AUG_EMMATCH)        ;; Too many matches for path expression
  ((syntax) AUG_ESYNTAX)        ;; Syntax error in lens file
  ((nolens) AUG_ENOLENS)        ;; Lens lookup failed
  ((mxfm) AUG_EMXFM)            ;; Multiple transforms
  ((nospan) AUG_ENOSPAN)        ;; No span for this node
  ((mvdesc) AUG_EMVDESC)        ;; Cannot move node into its descendant
  ((cmdrun) AUG_ECMDRUN)        ;; Failed to execute command
  ((badarg) AUG_EBADARG)        ;; Invalid argument in function call
  ((label) AUG_ELABEL) )        ;; Invalid label

(use lolevel) ;; free

(define-syntax begin0                 ; multiple values discarded
  (syntax-rules () ((_ e0 e1 ...)
                    (let ((tmp e0)) e1 ... tmp))))

(define-record augeas ptr)
(define-foreign-type augeas (c-pointer "augeas")
  (lambda (a) (or (augeas-ptr a)
             (error 'augeus "operation on closed handle"))))

(define _aug_init (foreign-lambda (c-pointer "augeas") aug_init c-string c-string int))
(define _aug_close (foreign-lambda void aug_close augeas))
(define _aug_get (foreign-lambda int aug_get augeas c-string (c-pointer c-string)))
(define _aug_set (foreign-lambda int aug_set augeas c-string c-string))
(define _aug_setm (foreign-lambda int aug_setm augeas c-string c-string c-string))
(define _aug_rm (foreign-lambda int aug_rm augeas c-string))
(define _aug_match (foreign-lambda int aug_match augeas c-string (c-pointer (c-pointer c-string))))
(define _aug_insert (foreign-lambda int aug_insert augeas c-string c-string bool))
(define _aug_print (foreign-lambda int aug_print augeas (c-pointer "FILE") c-string))
(define _aug_load (foreign-lambda int aug_load augeas))
(define _aug_error (foreign-lambda int aug_error augeas))  ;; error code
(define _aug_error_message (foreign-lambda c-string aug_error_message augeas))  ;; human-readable error
(define _aug_error_minor_message (foreign-lambda c-string aug_error_minor_message augeas)) ;; elaboration of error message
(define _aug_error_details (foreign-lambda c-string aug_error_details augeas))  ;; human-readable details

(define (aug-init #!key root loadpath (flags 0))
  (make-augeas (or (_aug_init root loadpath flags)
                   (error 'aug-init "initialization failed"))))
(define (aug-close a)        ;; safe to call this multiple times
  (when (augeas-ptr a)
    (_aug_close a)
    (augeas-ptr-set! a #f))
  (void))
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
(define (aug-insert! a path label #!optional before?)
  (let ((rc (_aug_insert a path label before?)))
    (when (< rc 0)
      (augeas-error a 'aug-insert! path label))
    (void)))
(define (aug-load! a)
  (when (< (_aug_load a) 0)
    (augeas-error a 'aug-load!))
  (void))

;; (define stdout (foreign-value "stdout" c-pointer))

;; Print matching nodes at PATH to PORT; PORT must be associated with a file descriptor and
;; must consequently be a stream port, not e.g. a string port.
(define (aug-print a path #!optional (port (current-output-port)))
  ;; Not sure if we need to flush before and/or after
  (define (port->file p)
    (##sys#check-port p 'aug-print)
    (or ((foreign-lambda* c-pointer ((scheme-object p)) "return(C_port_file(p));")
         p)
        (error 'aug-print "not a stream port" port)))
  (when (< (_aug_print a (port->file port) path) 0)
    (augeas-error a 'aug-print path))
  (void))

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
    (make-property-condition (int->errcode (_aug_error a))))))

