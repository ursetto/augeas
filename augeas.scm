(module augeas
(aug-init aug-close aug-get aug-exists? aug-set!
          aug-set-multiple! aug-remove! aug-move!
          aug-match aug-match-count
          aug-insert! aug-load! aug-save!
          aug-defvar aug-defnode
          aug-print)
(import scheme chicken foreign)
(include "augeas-mod.scm"))
