(setq inhibit-startup-message t)

;;Remover o Menu
(tool-bar-mode -1)
(menu-bar-mode -1)

;;Remover scrollbar
(scroll-bar-mode -1)

;;Shell
(global-set-key (kbd "C-x S") 'shell)

;;Adicionar número de linhas
(global-linum-mode t)

;;Fontes config
(set-face-attribute 'default nil :height 120)

;;Não fazer backup dos arquivos
(setq make-backup-files nil)

;;Pacotes
(require 'package)

;;Desativar pacotes ao iniciar
(setq package-enable-at-startup nil)

;;Adicionar packages disponivéis do Melp
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))




;;Iniciar os pacotes
(package-initialize)

;;Atualizar pacotes
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;org-mode
;; -*- mode: elisp -*-



;;;;Org mode configuration
;; Enable Org mode
(require 'org)
;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)
(setq org-log-done 'time)
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
;; Make Org mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen
;;web-mode para templates

(use-package web-mode
  :ensure t)

;;adicionando sintaxe highlight pra jsx 
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) 
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(defun web-mode-init-hook ()
  ;;evo mata o javascripto
  (setq web-mode-markup-indent-offset 4))
  
(add-hook 'web-mode-hook  'web-mode-init-hook)
;; nem preceisava disso
;; (require 'eglot)
;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)

(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(setq web-mode-engines-alist
  '(("php"    . "\\.phtml\\'")
    ("blade"  . "\\.blade\\."))
)

(setq-default indent-tabs-mode nil)
(setq web-mode-code-indent-offset 4)
(setq web-mode-indent-style 4)

;;quick run para compilar rapidamente
(require 'quickrun)

;;neo-tree e all-icons || treemacs
(global-set-key (kbd "C-<tab>") 'other-window)

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (progn
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
  :bind (("C-\\" . 'neotree-toggle)))
(setq-default neo-show-hidden-files t)

;;python suporte
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;java suporte
(add-hook 'java-mode-hook #'lsp)

;;react suporte
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-selected-packages '(lsp-mode yasnippet helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode spacemacs-theme json-mode))
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))
(load-theme 'spacemacs-dark t)
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'prog-mode-hook #'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      create-lockfiles nil) ;; lock files will kill `npm start'

(with-eval-after-load 'lsp-mode
  (require 'dap-chrome)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))
