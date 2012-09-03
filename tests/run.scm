(use augeas)
(use test)

(define-syntax expect-error    ;; return #t on augeas error code, else throw error
  (syntax-rules ()
    ((_ code e0 e1 ...)
     (condition-case (begin e0 e1 ...)
                     ((exn augeas code) #t)))))
(define-syntax begin0                 ; multiple values discarded
  (syntax-rules () ((_ e0 e1 ...)
                    (let ((tmp e0)) e1 ... tmp))))

(define (aug-match-pairs a path)
  (map (lambda (x) (cons x (aug-get a x)))
       (aug-match a path)))

(define root "../sandbox")
(define a (aug-init root: root))

(test-group
 "exists"
 (test "existent" #t
       (aug-exists? a "/files/etc/hosts/1/ipaddr"))
 (test "existent node with no value" #t
       (aug-exists? a "/files/etc/hosts/1"))
 (test "non-existent" #f
       (aug-exists? a "/files/etc/hosts/1/ipadd")))

(test-group
 "get"
 (test "get hosts/ipaddr" "127.0.0.1" (aug-get a "/files/etc/hosts/1/ipaddr"))
 (test "get missing node" #f (aug-get a "/files/etc/hosts/1/missing"))
 (test "get too many matches" #t
       (expect-error mmatch (aug-get a "/files/etc/hosts//alias")))
 (test "get invalid path expression" #t
       (expect-error pathx (aug-get a "/files/etc/hosts/alias["))))

(test-group
 "match"
 (test "match count hosts/*/ipaddr" 2
       (aug-match-count a "/files/etc/hosts/*/ipaddr"))   ; hosts/1 and hosts/2
 (test "match hosts/*/ipaddr"
       '("/files/etc/hosts/1/ipaddr" "/files/etc/hosts/2/ipaddr")
       (aug-match a "/files/etc/hosts/*/ipaddr"))
 (test "match hosts"     ;; probably a redundant test
       '("/files/etc/hosts/#comment[1]" "/files/etc/hosts/#comment[2]"
         "/files/etc/hosts/1" "/files/etc/hosts/#comment[3]"
         "/files/etc/hosts/#comment[4]" "/files/etc/hosts/2")
       (aug-match a "/files/etc/hosts/*")))

(test-group
 "init"
 ;; Init DB and verify /augeas/save is set to proper flag.  We'll test that
 ;; saving actually works later.
 (define (test-cfg label flags)
   (let ((a (aug-init flags: (append flags '(no-load no-module-autoload)))))
     (begin0 (aug-get a (string-append "/augeas/" label))
       (aug-close a))))
 (test "save default" "overwrite"
       (test-cfg "save" '()))
 (test "save overwrite" "overwrite"
       (test-cfg "save" '(save-overwrite)))
 (test "save noop" "noop"
       (test-cfg "save" '(save-noop)))
 (test "save newfile" "newfile"
       (test-cfg "save" '(save-newfile)))
 (test "save backup" "backup"
       (test-cfg "save" '(save-backup)))

 (test-assert
  "do not load files (no-load)"
  (let ((a (aug-init root: root flags: '(no-load))))
    (begin0
        (and (= 0 (aug-match-count a "/files//*"))
             (< 0 (aug-match-count a "/augeas/load//*")))
      (aug-close a))))

 (test-assert
  "do not load files or modules (no-load + no-module-autoload)"
  (let ((a (aug-init root: root flags: '(no-load no-module-autoload))))
    (begin0
        (and (= 0 (aug-match-count a "/files//*"))
             (= 0 (aug-match-count a "/augeas/load//*")))
      (aug-close a))))

 (test "span disabled by default" "disable"
       (test-cfg "span" '()))
 (test "span enabled" "enable"
       (test-cfg "span" '(enable-span)))

 ;; Untested: no-stdinc, type-check

 )

(test-group
 "set"
 ;; rm ipaddr node, or reload
 (test "assert nonexistent" #f
       (aug-get a "/files/etc/hosts/01/ipaddr"))
 (test "set!" (void)
       (aug-set! a "/files/etc/hosts/01/ipaddr" "192.168.5.4"))
 (test "get" "192.168.5.4"
       (aug-get a "/files/etc/hosts/01/ipaddr"))
 (test "set! > 1 match" #t
       (expect-error mmatch
                     (aug-set! a "/files/etc/hosts//ipaddr" "192.168.5.5"))))

(test-group
 "rm"
 (let ((p "/files/etc/hosts/01/ipaddr")
       (v "192.168.5.4"))
   (test "set!" (void) (aug-set! a p v))
   (test "assert get" v (aug-get a p))
   (test "rm" 1 (aug-remove! a "/files/etc/hosts/01/ipaddr"))
   (test "assert get missing" #f (aug-get a p))))

(test-group
 "load"
 (test "verify initial state"
       '(("/files/etc/hosts/1/ipaddr" . "127.0.0.1")
         ("/files/etc/hosts/2/ipaddr" . "172.31.122.14"))
       (aug-match-pairs a "/files/etc/hosts/*/ipaddr"))
 (test "update ip addresses" #t
       (begin
         (aug-set! a "/files/etc/hosts/1/ipaddr" "192.168.2.4")
         (aug-set! a "/files/etc/hosts/2/ipaddr" "192.168.2.4")
         #t))
 ;; We do the above instead of set-multiple! so we can do this before testing set-multiple!
 ;; (test "set multiple" 2
 ;;       (aug-set-multiple! a "/files/etc/hosts/*/ipaddr" #f "192.168.2.4"))
 (test "verify updated state"
       '(("/files/etc/hosts/1/ipaddr" . "192.168.2.4")
         ("/files/etc/hosts/2/ipaddr" . "192.168.2.4"))
       (aug-match-pairs a "/files/etc/hosts/*/ipaddr"))
 (test-assert "load"
              (aug-load! a))
 (test "verify reverted state"      ;; well, part of it, anyway
       '(("/files/etc/hosts/1/ipaddr" . "127.0.0.1")
         ("/files/etc/hosts/2/ipaddr" . "172.31.122.14"))
       (aug-match-pairs a "/files/etc/hosts/*/ipaddr")))

;; FIXME: match would be better here
(test-group
 "set multiple, rm multiple"
 (test "set multiple" 2
       (aug-set-multiple! a "/files/etc/hosts/*[label() != '#comment']"
                          "test" "foo"))
 (test "verify existence"
       '("/files/etc/hosts/1/test" "/files/etc/hosts/2/test")
       (aug-match a "/files/etc/hosts/*/test"))
 (test "rm multiple" 2
       (aug-remove! a "/files/etc/hosts/*/test"))
 (test "verify non-existence" '()
       (aug-match a "/files/etc/hosts/*/test")))

(test-group
 "insert"
 (test "verify initial state"
       '(("/files/etc/hosts/1/alias[1]" . "localhost")
         ("/files/etc/hosts/1/alias[2]" . "galia.watzmann.net")
         ("/files/etc/hosts/1/alias[3]" . "galia"))
       (aug-match-pairs a "/files/etc/hosts/1/alias"))
 (test-assert "insert before"
              (aug-insert! a "/files/etc/hosts/1/alias[2]" "alias" #t))
 (test-assert "insert after"
              (aug-insert! a "/files/etc/hosts/1/alias[3]" "alias"))
 (test "verify inserted state"
       '(("/files/etc/hosts/1/alias[1]" . "localhost")
         ("/files/etc/hosts/1/alias[2]" . #f)
         ("/files/etc/hosts/1/alias[3]" . "galia.watzmann.net")
         ("/files/etc/hosts/1/alias[4]" . #f)
         ("/files/etc/hosts/1/alias[5]" . "galia"))
       (aug-match-pairs a "/files/etc/hosts/1/alias"))
 ;; (aug-load! a)
 ;; ;; or, instead of load!, we can do:
 (aug-remove! a "/files/etc/hosts/1/alias[4]")
 (aug-remove! a "/files/etc/hosts/1/alias[2]")
 (test "verify state after removal"
       '(("/files/etc/hosts/1/alias[1]" . "localhost")
         ("/files/etc/hosts/1/alias[2]" . "galia.watzmann.net")
         ("/files/etc/hosts/1/alias[3]" . "galia"))
       (aug-match-pairs a "/files/etc/hosts/1/alias")
       ))

(test-group
 "move"
 (test-assert "set /a/b"
              (aug-set! a "/a/b" "foo"))
 (test-assert "move /a/b /x/y"
              (aug-move! a "/a/b" "/x/y"))
 (test "verify dest exists" "foo"
       (aug-get a "/x/y"))
 (test "verify src does not exist" #f
       (aug-exists? a "/a/b"))
 (test-assert "delete dest"
              (aug-remove! a "/x/y")))

(test-group
 "defvar"
 (test "$hosts is undefined" #t
       (expect-error pathx (aug-get a "$hosts/1/ipaddr")))
 (test "defvar $hosts" 1
       (aug-defvar a "hosts" "/files/etc/hosts"))
 (test "get $hosts/1/ipaddr" "127.0.0.1"
       (aug-get a "$hosts/1/ipaddr"))
 (test "un-defvar $hosts" 0
       (aug-defvar a "hosts" #f))
 (test "$hosts is undefined again" #t
       (expect-error pathx (aug-get a "$hosts/1/ipaddr")))
 (test "defvar $ipaddrs" 2  ;; confirm nodeset works
       (aug-defvar a "ipaddrs" "/files/etc/hosts//ipaddr"))
 (test "undefvar $ipaddrs" 0
       (aug-defvar a "ipaddrs" #f)) 
 ;; we do not test definition error (bad path, probably)
 )

;; Unfortunately we can't output to string, only stream, so we have to
;; write and read a file to check the output.
(test-group
 "print"
 (test "print host node to a file and verify output"
       ;; note that EOF heredoc eats the final newline
       #<<EOF
/files/etc/hosts/1
/files/etc/hosts/1/ipaddr = "127.0.0.1"
/files/etc/hosts/1/canonical = "localhost.localdomain"
/files/etc/hosts/1/alias[1] = "localhost"
/files/etc/hosts/1/alias[2] = "galia.watzmann.net"
/files/etc/hosts/1/alias[3] = "galia"

EOF
       (let ((fn (create-temporary-file)))   ;; insecure
         (call-with-output-file fn (lambda (p) (aug-print a "/files/etc/hosts/1" p)))
         (begin0
             (with-input-from-file fn read-string)
           (delete-file fn)))))


;; Must come at end of tests; closes handle

(test-group
 "close"
 (test "verify handle is open" #t
       (aug-exists? a "/files/etc/hosts/1/ipaddr"))
 (test-assert "close handle"             ;; return val unspecified, in practice void
              (aug-close a))
 (test-assert "duplicate close ignored"  ;; return val unspecified, in practice void
              (aug-close a))
 (test-error "error: operation on closed handle"
             (aug-exists? a "/files/etc/hosts/1/ipaddr")))



