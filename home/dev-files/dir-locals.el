((nil
   .
   (
     (projectile-project-run-cmd . "gulp serve")
     (projectile-project-test-cmd . "npm run test")
     (projectile-project-compilation-cmd . "npm run start")
     (com-css-sort-sort-file . ".sort-order.config")
     (mocha-project-test-directory . "test")
     (mocha-reporter . "spec")
     (eval .
       (progn
         (add-to-list 'ignoramus/directory-basename-exact-names '".log")
         (add-to-list 'ignoramus/directory-basename-exact-names '".npm-cache")
         (add-to-list 'ignoramus/directory-basename-exact-names '".yarn-cache")))
     (eval .
       (progn
         (with-eval-after-load 'zel
           (add-to-list 'zel-exclude-patterns ignoramus-boring-file-regexp))
         ))
     (eval .
       (progn
         (with-eval-after-load 'helm
           (setq helm-grep-ignored-files
             (cons ".#*"
               (delq nil
                 (mapcar
                   #'(lambda
                       (pat)
                       (concat "*" pat))
                   ignoramus/file-basename-endings))))
           (setq helm-grep-ignored-directories ignoramus/directory-basename-exact-names))))
     (eval .
       (progn
         (with-eval-after-load 'neotree
           (add-to-list 'neo-hidden-regexp-list ignoramus-boring-file-regexp))))
     (eval .
       (progn
         (with-eval-after-load 'ignoramus
           (setq projectile-globally-ignored-file-suffixes
             (mapcar
               #'(lambda
                   (ext)
                   (replace-regexp-in-string "\\`\\." "" ext))
               ignoramus/file-basename-endings))
           (setq projectile-globally-ignored-files ignoramus/file-basename-exact-names)
           (setq projectile-globally-ignored-directories
             (append ignoramus/directory-basename-exact-names
               (--map
                 (s-append "/" it)
                 (--map
                   (s-prepend "-" it)
                   ignoramus/directory-basename-exact-names))))

           (setq ignoramus-file-basename-beginnings ignoramus/file-basename-beginnings)
           (setq ignoramus-file-basename-endings ignoramus/file-basename-endings)
           (setq ignoramus-file-basename-exact-names
             (append ignoramus/file-basename-exact-names ignoramus/directory-basename-exact-names))
           (setq ignoramus-file-basename-regexps ignoramus/file-basename-regexps)
           )))
     (eval .
       (progn
         (ignoramus-setup
           '(comint completions dired eshell grep ido pcomplete shell))))
     (eval .
       (progn))
     (eval . (progn))
     (eval . (progn)))))
