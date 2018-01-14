(ns rest-client.core-test
  (:require [cheshire.core :as cheshire]
            [clojure.test :as t]
            [rest-client.core :as sut]))

(t/deftest test-main
  (t/testing "HTTP GET 200"
    (let [args ["http://localhost:3000/members" "GET"]
          res-body (cheshire/generate-string
                    [{:member-id 1
                      :first-name "You"
                      :last-name "Watanabe"
                      :email "y.watanabe@uranohoshi.ac.jp"}
                     {:member-id 2
                      :first-name "Yoshiko"
                      :last-name "Tsushima"
                      :email "y.tsushima@uranohoshi.ac.jp"}])]
      (with-redefs [clj-http.client/get (constantly {:status 200
                                                     :body res-body})]
        (t/is (= (str res-body "\n")
                 (with-out-str (apply sut/-main args)))))))
  (t/testing "HTTP GET 404"
    (let [args ["http://localhost:3000/invalid-path" "GET"]
          res-body (cheshire/generate-string
                    {:error "not-found"})]
      (with-redefs [clj-http.client/get (constantly {:status 404
                                                     :body res-body})]
        (t/is (= (str "404\t" res-body "\n")
                 (with-out-str (apply sut/-main args)))))))
  (t/testing "HTTP POST 200"
    (let [args ["http://localhost:3000/members" "POST"]
          input (cheshire/generate-string
                 {:first-name "Dia"
                  :last-name "Kurosawa"
                  :email "d.kurosawa@uranohoshi.ac.jp"})
          res-body (cheshire/generate-string
                    {:member-id 3
                     :first-name "Dia"
                     :last-name "Kurosawa"
                     :email "d.kurosawa@uranohoshi.ac.jp"})]
      (with-redefs [clj-http.client/post (constantly {:status 200
                                                      :body res-body})]
        (t/is (= (str res-body "\n")
                 (with-in-str input
                   (with-out-str (apply sut/-main args))))))))
  (t/testing "HTTP POST 404"
    (let [args ["http://localhost:3000/invalid-path" "POST"]
          input (cheshire/generate-string
                 {:first-name "Dia"
                  :last-name "Kurosawa"
                  :email "d.kurosawa@uranohoshi.ac.jp"})
          res-body (cheshire/generate-string
                    {:error "not-found"})]
      (with-redefs [clj-http.client/post (constantly {:status 404
                                                      :body res-body})]
        (t/is (= (str "404\t" res-body "\n")
                 (with-in-str input
                   (with-out-str (apply sut/-main args))))))))
  (t/testing "invalid number of args"
    (let [args ["http://localhost:3000/members"]]
      (t/is (thrown? IllegalArgumentException
                     (apply sut/-main args))))
    (let [args ["http://localhost:3000/members" "GET" "42"]]
      (t/is (thrown? IllegalArgumentException
                     (apply sut/-main args)))))
  (t/testing "invalid HTTP method"
    (let [args ["http://localhost:3000/members" "PUT"]]
      (t/is (thrown? UnsupportedOperationException
                     (apply sut/-main args)))))
  (t/testing "invalid JSON input"
    (let [args ["http://localhost:3000/members" "POST"]
          input "{\"first-name\":"]
      (t/is (thrown? com.fasterxml.jackson.core.JsonParseException
                     (with-in-str input
                       (apply sut/-main args)))))))
