(use test augeas)

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
 (test-error "get too many matches" (aug-get a "/files/etc/hosts//alias"))
 (test-error "get invalid path expression" (aug-get a "/files/etc/hosts/alias[")))

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
 (test-error "set! > 1 match"
             (aug-set! a "/files/etc/hosts//ipaddr" "192.168.5.5")))


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
 (test "verify one get" "foo"
       (aug-get a "/files/etc/hosts/1/test"))
 (test "rm multiple" 3
       (aug-remove! a "/files/etc/hosts/*/test"))
 (test "verify non-existent" #f
       (aug-exists? a "/files/etc/hosts/1/test")))


;; Note: errors contain info, we should check against expected error type:

#|

#;76> (aug-get a "/files/etc/hosts//alias")
Error: (aug-get) Too many matches for path expression: "/files/etc/hosts//alias"
#;76> ,exn
condition: (exn augeas unknown)
 exn
	location: aug-get
	message: "Too many matches for path expression"
	arguments: ("/files/etc/hosts//alias")
 augeas
	code: 5
	message: "Too many matches for path expression"
	minor-message: #f
	details: "There are 4 nodes matching /files/etc/hosts//alias"
 unknown

#;77> (aug-get a "/files/etc/hosts/alias[")
Error: (aug-get) Invalid path expression: "/files/etc/hosts/alias["
#;77> ,exn
condition: (exn augeas unknown)
 exn
	location: aug-get
	message: "Invalid path expression"
	arguments: ("/files/etc/hosts/alias[")
 augeas
	code: 3
	message: "Invalid path expression"
	minor-message: "illegal string literal"
	details: "/files/etc/hosts/alias[|=|"
 unknown

|#
