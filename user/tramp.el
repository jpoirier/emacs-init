;;; tramp

(setq tramp-default-method "ssh")
(setq tramp-default-user "jpoirier")

(add-to-list 'tramp-default-user-alist
                  '("ssh" "\\`here\\.somewhere\\.else\\'" nil))


(tramp-set-completion-function "ssh"
           '((tramp-parse-sconfig "/etc/ssh_config")
             (tramp-parse-sconfig "~/.ssh/config")))
          
               ((tramp-parse-sconfig "/etc/ssh_config")
                   (tramp-parse-sconfig "~/.ssh/config"))

; 66.228.48.111:60101
; ~/.ssh/id_rsa





