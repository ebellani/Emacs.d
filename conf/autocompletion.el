;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autocomplete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat *my-default-lib* "/auto-complete"))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat *my-default-lib* "/auto-complete/ac-dict"))
(ac-config-default)

;; dirty fix for having AC everywhere
;; (define-globalized-minor-mode real-global-auto-complete-mode
;;   auto-complete-mode (lambda ()
;;                        (if (not (minibufferp (current-buffer)))
;;                          (auto-complete-mode 1))
;;                        ))

(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (or (minibufferp (current-buffer))
                                    (not (numberp
                                          (compare-strings "*eshell*" 0 7
                                                           (buffer-name
                                                            (current-buffer)) 0 7)))))
                         (auto-complete-mode 1))))

;; tab in insert mode calls autocomplete
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "C-c TAB")

(real-global-auto-complete-mode 1)
