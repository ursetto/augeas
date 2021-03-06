#> #include <augeas.h> <#

;; todo: srun; span (?); transform (?); rename (? -- needs unreleased 2012-08)
;; todo: init using AUG_NO_ERR_CLOSE (requires 0.10.0).  Easy, but I have no way to
;;       trigger a failure for testing.
;; todo: use init/close_memstream from augeas internal.h to write FD output to memory
;;       for string ports for both aug_print and aug_srun.  has been available since 0.2.0 (2008-06)

(use foreigners)
(use lolevel) ;; free
(use srfi-1)  ;; list-tabulate

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
  ;; not available in 0.10.0 release
  ;; ((label) AUG_ELABEL)       ;; Invalid label
  )

(define-foreign-enum-type (augeas:initflag int 'unknown)
  (initflag->int int->initflag)
  ((none) AUG_NONE)
  ((save-overwrite) AUG_NONE)
  ((save-backup) AUG_SAVE_BACKUP)
  ((save-newfile) AUG_SAVE_NEWFILE)
  ((save-noop) AUG_SAVE_NOOP)
  ((type-check) AUG_TYPE_CHECK)
  ((no-stdinc) AUG_NO_STDINC)   ;; better name?  no-builtin-search-path?
  ((no-load) AUG_NO_LOAD)
  ((no-module-autoload) AUG_NO_MODL_AUTOLOAD)
  ((enable-span) AUG_ENABLE_SPAN)
  ;; ((no-error-close) AUG_NO_ERR_CLOSE)      ;; don't expose this--we should handle it transparently
  )

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
(define _aug_mv (foreign-lambda int aug_mv augeas c-string c-string))
(define _aug_match (foreign-lambda int aug_match augeas c-string (c-pointer (c-pointer c-string))))
(define _aug_insert (foreign-lambda int aug_insert augeas c-string c-string bool))
(define _aug_print (foreign-lambda int aug_print augeas (c-pointer "FILE") c-string))
(define _aug_load (foreign-lambda int aug_load augeas))
(define _aug_save (foreign-lambda int aug_save augeas))
(define _aug_defvar (foreign-lambda int aug_defvar augeas c-string c-string))
(define _aug_defnode (foreign-lambda int aug_defnode augeas c-string c-string c-string (c-pointer bool)))
(define _aug_error (foreign-lambda int aug_error augeas))  ;; error code
(define _aug_error_message (foreign-lambda c-string aug_error_message augeas))  ;; human-readable error
(define _aug_error_minor_message (foreign-lambda c-string aug_error_minor_message augeas)) ;; elaboration of error message
(define _aug_error_details (foreign-lambda c-string aug_error_details augeas))  ;; human-readable details

(define (aug-init #!key root loadpath (flags 'none))
  (make-augeas (or (_aug_init root loadpath (initflag->int flags))
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
(define (aug-move! a from to)
  (let ((rc (_aug_mv a from to)))
    (if (< rc 0)
        (augeas-error a 'aug-move! from to)
        (void))))
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
(define (aug-save! a #!optional mode)
  (when (and mode
             (not (memq mode '(overwrite backup newfile noop))))
    (error 'aug-save! "Illegal save mode" mode))
  (let ((old-mode (and mode (aug-get a "/augeas/save")))
        (mode (if (symbol? mode) (symbol->string mode) mode)))
    (when (and old-mode
               (not (string=? old-mode mode)))
      (aug-set! a "/augeas/save" mode))
    (when (< (_aug_save a) 0)
      (augeas-error a 'aug-save!))
    ;; FIXME: if save errors out, we don't restore mode.  Use handle-exceptions,
    ;; but be careful as the restore could fail too
    (when (and old-mode
               (not (string=? old-mode mode)))
      (aug-set! a "/augeas/save" old-mode))
    (void)))
(define (aug-defvar a name expr)
  (let ((rc (_aug_defvar a name expr)))
    (when (< rc 0)
      (augeas-error a 'aug-defvar name expr))
    rc))
(define (aug-defnode a name expr value)
  (let-location ((created bool))
    (let ((rc (_aug_defnode a name expr value #$created)))
      (when (< rc 0)
        (augeas-error a 'aug-defnode name expr value))
      (values rc created))))

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

