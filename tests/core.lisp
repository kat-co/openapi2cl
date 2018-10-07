(in-package :cl-user)
(defpackage :openapi2cl/tests/core
  (:use :cl :rove :openapi2cl/core))
(in-package :openapi2cl/tests/core)

(deftest media-type-p-tests
  (testing "should validate `application/json` as valid"
    (ok (openapi2cl/core::media-type-p '("application/json"))))

  (testing "should validate `text/json` as valid"
    (ok (openapi2cl/core::media-type-p '("text/json"))))

  (testing "should validate `application/foo+json` as valid"
    (ok (openapi2cl/core::media-type-p '("application/foo+json"))))

  (testing "should validate `applications/yaml` as valid"
    (ok (openapi2cl/core::media-type-p '("application/yaml")))))
