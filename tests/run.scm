(use test)

(test "get" "127.0.0.1" (aug-get a "/files/etc/hosts/1/ipaddr"))

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
