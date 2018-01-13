(ns rest-server.handler.member-test
  (:require [clojure.test :as t]
            [integrant.core :as ig]
            [rest-server.handler.member :as sut]
            [shrubbery.core :as shrubbery]))

(t/deftest test-list
  (let [handler (ig/init-key ::sut/list
                             {:db (shrubbery/stub
                                   rest-server.boundary.db.member/Members
                                   {:list-members [{:id 1
                                                    :first-name "You"
                                                    :last-name "Watanabe"
                                                    :email "y.watanabe@uranohoshi.ac.jp"}
                                                   {:id 2
                                                    :first-name "Yoshiko"
                                                    :last-name "Tsushima"
                                                    :email "y.tsushima@uranohoshi.ac.jp"}]})})]
    (t/is (= [:ataraxy.response/ok [{:member-id 1
                                     :first-name "You"
                                     :last-name "Watanabe"
                                     :email "y.watanabe@uranohoshi.ac.jp"}
                                    {:member-id 2
                                     :first-name "Yoshiko"
                                     :last-name "Tsushima"
                                     :email "y.tsushima@uranohoshi.ac.jp"}]]
             (handler {:ataraxy/result [:member/list]})))))

(t/deftest test-create
  (let [handler (ig/init-key ::sut/create
                             {:db (shrubbery/stub
                                   rest-server.boundary.db.member/Members
                                   {:create-member 3})})
        member {:first-name "Dia"
                :last-name "Kurosawa"
                :email "d.kurosawa@uranohoshi.ac.jp"}]
    (t/is (= [:ataraxy.response/ok {:member-id 3
                                    :first-name "Dia"
                                    :last-name "Kurosawa"
                                    :email "d.kurosawa@uranohoshi.ac.jp"}]
             (handler {:ataraxy/result [:member/create member]})))))

(t/deftest test-fetch
  (t/testing "found"
    (let [handler (ig/init-key ::sut/fetch
                               {:db (shrubbery/stub
                                     rest-server.boundary.db.member/Members
                                     {:fetch-member {:id 2
                                                     :first-name "Yoshiko"
                                                     :last-name "Tsushima"
                                                     :email "y.tsushima@uranohoshi.ac.jp"}})})
          member-id 2]
      (t/is (= [:ataraxy.response/ok {:member-id 2
                                      :first-name "Yoshiko"
                                      :last-name "Tsushima"
                                      :email "y.tsushima@uranohoshi.ac.jp"}]
               (handler {:ataraxy/result [:member/fetch member-id]})))))
  (t/testing "not found"
    (let [handler (ig/init-key ::sut/fetch
                               {:db (shrubbery/stub
                                     rest-server.boundary.db.member/Members
                                     {:fetch-member nil})})
          member-id 100]
      (t/is (nil? (handler {:ataraxy/result [:member/fetch member-id]}))))))
