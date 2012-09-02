(use test augeas)

(define-syntax expect-error    ;; return #t on augeas error code, else throw error
  (syntax-rules ()
    ((_ code e0 e1 ...)
     (condition-case (begin e0 e1 ...)
                     ((exn augeas code) #t)))))

(define a (aug-init root: "../sandbox"))

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

;; FIXME: match would be better here
(test-group
 "set multiple, rm multiple"
 (test "set multiple" 3          ;; 3 because 1 and 2 are in file, and we added 01 above
       (aug-set-multiple! a "/files/etc/hosts/*[label() != '#comment']"
                          "test" "foo"))
 (test "verify existence"
       '("/files/etc/hosts/1/test" "/files/etc/hosts/2/test" "/files/etc/hosts/01/test")
       (aug-match a "/files/etc/hosts/*/test"))
 (test "rm multiple" 3
       (aug-remove! a "/files/etc/hosts/*/test"))
 (test "verify non-existence" '()
       (aug-match a "/files/etc/hosts/*/test")))

(test-group
 "insert"
 (test "verify initial state"
       '(("/files/etc/hosts/1/alias[1]" . "localhost")
         ("/files/etc/hosts/1/alias[2]" . "galia.watzmann.net")
         ("/files/etc/hosts/1/alias[3]" . "galia"))
       (map (lambda (x) (cons x (aug-get a x))) (aug-match a "/files/etc/hosts/1/alias")))
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
       (map (lambda (x) (cons x (aug-get a x))) (aug-match a "/files/etc/hosts/1/alias")))
 (aug-remove! a "/files/etc/hosts/1/alias[4]")
 (aug-remove! a "/files/etc/hosts/1/alias[2]")
 (test "verify state after removal"
       '(("/files/etc/hosts/1/alias[1]" . "localhost")
         ("/files/etc/hosts/1/alias[2]" . "galia.watzmann.net")
         ("/files/etc/hosts/1/alias[3]" . "galia"))
       (map (lambda (x) (cons x (aug-get a x))) (aug-match a "/files/etc/hosts/1/alias"))))

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
