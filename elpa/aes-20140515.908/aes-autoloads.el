;;; aes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (aes-InvCipher aes-Cipher) "aes" "aes.el" (21366
;;;;;;  42326 616049 466000))
;;; Generated autoloads from aes.el

(autoload 'aes-Cipher "aes" "\
Perform a complete aes encryption of the unibyte string PLAIN.
Return a new string containing the encrypted string PLAIN.
Use KEYS as the expanded key as defined in `aes-SubShiftMixKeys'.
NB is the number of 32-bit words in PLAIN.  NR is the number of rounds.
The length of KEYS is (1 + NR) * NB.

\(fn PLAIN KEYS NB &optional NR)" nil nil)

(autoload 'aes-InvCipher "aes" "\
Perform a complete aes decryption of the unibyte string CIPHER.
Return a new string containing the decrypted string CIPHER.
Use KEYS as the expanded key as defined in `aes-InvSubShiftMixKeys'.
NB is the number of 32-bit words in CIPHER.  NR is the number of rounds.
The length of KEYS is (1 + NR) * NB.

\(fn CIPHER KEYS NB &optional NR)" nil nil)

;;;***

;;;### (autoloads nil nil ("aes-pkg.el") (21366 42326 628691 577000))

;;;***

(provide 'aes-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; aes-autoloads.el ends here
