(use posix srfi-13 files)
(define pkg-config-version
  (with-input-from-pipe "pkg-config --version" read-line))
(when (eof-object? pkg-config-version)
  (error "Unable to locate pkg-config"))
(define augeas-version
  (with-input-from-pipe "pkg-config --modversion augeas" read-line))
(when (eof-object? augeas-version)
  (error "Unable to obtain augeas version"))

(define augeas-cflags
  (with-input-from-pipe "pkg-config --cflags augeas" read-line))
(define augeas-ldflags
  (with-input-from-pipe "pkg-config --libs augeas" read-line))

(define augeas-options
  (string-append
   (string-join (string-split augeas-cflags) " -C " 'prefix)
   (string-join (string-split augeas-ldflags) " -L " 'prefix)))

