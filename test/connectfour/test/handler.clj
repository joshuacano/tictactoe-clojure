(ns connectfour.test.handler
  (:require [clojure.test :refer :all]
            [ring.mock.request :refer :all]
            [connectfour.handler :refer :all]))

(deftest test-app
  (testing "main route"
    (let [response (app (request :get "/"))]
      (is (= 200 (:status response)))))

  (testing "matrix route"
    (let [response (app (request :get "/matrix"))]
      (is (= 200 (:status response)))))

  (testing "about route"
    (let [response (app (request :get "/about"))]
      (is (= 200 (:status response)))))

  (testing "init route"
    (let [response (app (request :get "/init"))]
      (is (= 200 (:status response)))))

  (testing "not-found route"
    (let [response (app (request :get "/invalid"))]
      (is (= 404 (:status response))))))

