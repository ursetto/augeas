(use augeas)
(use test)
(use files)

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

(define root "root")
;; (define root "tests/root")                     ;; if testing with system egg from main dir 
(define (pn fn)
  (make-pathname root fn))
(define (delete-files . files)
  (for-each (lambda (fn)
              (when (file-exists? fn) (delete-file fn)))
            files))
;; get to known file state
(define (known-file-state!)
  (file-copy (pn "etc/hosts.pristine")
             (pn "etc/hosts")
             #t)
  (delete-files (pn "etc/hosts.augsave") (pn "etc/hosts.augnew")))

(known-file-state!)
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


(test-group
 "defnode"
 (test "defnode $myalias, appending new node with last()+1"
       '(1 #t)
       (receive (aug-defnode a "myalias" "/files/etc/hosts/1/alias[last()+1]"
                             "crichton.xorinia.dim")))
;; does not set here, as node exists.  only sets if creates
 (test "defnode $myalias1 as same node as $myalias, using last()"
       '(1 #f)
       (receive (aug-defnode a "myalias1" "/files/etc/hosts/1/alias[last()]"
                             "crichton2.xorinia.dim")))
 (test "get $myalias (original value, defnode does not modify existing nodes)"
       "crichton.xorinia.dim" (aug-get a "$myalias"))
 (test "get $myalias1 (same as $myalias)" "crichton.xorinia.dim"
       (aug-get a "$myalias1"))
 (test "remove $myalias" 1
       (aug-remove! a "$myalias"))
 (test "verify $myalias removed" #f (aug-get a "$myalias"))
 (test "verify $myalias1 removed" #f (aug-get a "$myalias1")) 
 (test "undefine node vars" 0
       (+
        (aug-defvar a "myalias" #f)
        (aug-defvar a "myalias1" #f))))

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

;; Library note: aug_load will reload a file if it's been modified in memory
;;               OR if its mtime (to the second) differs from the last load or save time.
;;               Therefore if you save a file, modify it externally, and load it
;;               all within one second, the changes will not be picked up.  (as of 0.10.0)
(test-group
 "save"
 (define (file-compare fn1 fn2)
   (string=? (with-input-from-file fn1 read-string)
             (with-input-from-file fn2 read-string)))
 
 (test "baseline: verify pristine and sullied files differ" #f
       (file-compare (pn "etc/hosts.pristine") (pn "etc/hosts.sullied")))

 (define (test-save-mode mode new-file-name #!optional backup-file-name)
   (test-group
    (symbol->string (or mode 'default))
    (known-file-state!) ;; pristine, angel!
    (test "baseline: verify current and sullied files differ" #f ;; overkill but hey
          (file-compare (pn "etc/hosts") (pn "etc/hosts.sullied")))
    (test "baseline: verify current and pristine files identical" #t
          (file-compare (pn "etc/hosts") (pn "etc/hosts.pristine")))
    (test-assert "load"
                 (begin
                   ;; trick augeas into reloading externally modified file
                   (aug-set! a "/augeas/files/etc/hosts/mtime" #f)       ;; "0" works too
                   (aug-load! a)))
    (test "verify original file ipaddr"
          "127.0.0.1"
          (aug-get a "/files/etc/hosts/1/ipaddr"))
    (test-assert "set new ipaddr"
                 (aug-set! a "/files/etc/hosts/1/ipaddr" "33.34.35.36"))
    (test-assert "update file"
                 (if mode
                     (aug-save! a mode)
                     (aug-save! a)))        ;; explicitly test default, even though #f allowed
    (test-assert "verify current and sullied files now identical"
                 (file-compare (pn new-file-name) (pn "etc/hosts.sullied")))
    (when backup-file-name
      (test-assert "verify backup and pristine files still identical"
                   (file-compare (pn backup-file-name) (pn "etc/hosts.pristine"))))
    ))
 (test-save-mode 'overwrite "etc/hosts")
 (test-save-mode 'backup "etc/hosts" "etc/hosts.augsave")
 (test-save-mode 'newfile "etc/hosts.augnew" "etc/hosts")
 (test-save-mode #f "etc/hosts")
 (known-file-state!))

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



