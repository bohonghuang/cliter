(defsystem cliter
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "mit"
  :description "A simple closure-based iterator library for Common Lisp"
  :homepage "https://github.com/bohonghuang/cliter"
  :bug-tracker "https://github.com/bohonghuang/cliter/issues"
  :source-control (:git "https://github.com/bohonghuang/cliter.git")
  :components ((:file "package"))
  :depends-on (#:alexandria)
  :in-order-to ((test-op (test-op #:cliter/test))))

(defsystem cliter/test
  :depends-on (#:asdf #:parachute #:cliter)
  :components ((:file "test"))
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:cliter.test))))
