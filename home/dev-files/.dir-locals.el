;;; .dir-locals.el --- description                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Andrew Cates

;; Author: Andrew Cates <andrew@cates.io>
((nil . ((projectile-project-compilation-cmd . "export $(cat .env | grep -v ^# | xargs) && npm run start")
          (projectile-project-test-cmd . "npm run test")
          (projectile-project-run-cmd . "export $(cat .env | grep -v ^# | xargs) && node --inspect server.js"))))
