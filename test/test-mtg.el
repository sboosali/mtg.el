;;; test-mtg.el --- Unit Tests ‘mtg.el’ -*- coding: utf-8; lexical-binding: t -*-

(ert-deftest ert-test/mismatch ()

  (should (eql (cl-mismatch "" "") nil))
  (should (eql (cl-mismatch "" "a") 0))
  (should (eql (cl-mismatch "a" "a") nil))
  (should (eql (cl-mismatch "ab" "a") 1))
  (should (eql (cl-mismatch "Aa" "aA") 0))
  (should (eql (cl-mismatch '(a b c) '(a b d)) 2)))

;;; test-mtg.el ends here