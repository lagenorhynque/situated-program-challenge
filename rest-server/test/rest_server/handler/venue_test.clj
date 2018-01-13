(ns rest-server.handler.venue-test
  (:require [clojure.test :as t]
            [integrant.core :as ig]
            [rest-server.handler.venue :as sut]
            [shrubbery.core :as shrubbery]))

(t/deftest test-list
  (let [handler (ig/init-key ::sut/list
                             {:db (shrubbery/stub
                                   rest-server.boundary.db.venue/Venues
                                   {:list-venues [{:id 1
                                                   :name "Yokohama Arena"
                                                   :postal-code "222-0033"
                                                   :prefecture "Kanagawa-ken"
                                                   :city "Yokohama-shi"
                                                   :street1 "Kohoku-ku"
                                                   :street2 "3-10 Shinyokohama"
                                                   :group-id 1}
                                                  {:id 2
                                                   :name "Nippon Gaishi Hall"
                                                   :postal-code "457-0833"
                                                   :prefecture "Aichi-ken"
                                                   :city "Nagoya-shi"
                                                   :street1 "Minami-ku"
                                                   :street2 "5-1-16 Higashimatabeecho"
                                                   :group-id 1}]})})
        group-id 1]
    (t/is (= [:ataraxy.response/ok [{:venue-id 1
                                     :venue-name "Yokohama Arena"
                                     :address {:postal-code "222-0033"
                                               :prefecture "Kanagawa-ken"
                                               :city "Yokohama-shi"
                                               :address1 "Kohoku-ku"
                                               :address2 "3-10 Shinyokohama"}}
                                    {:venue-id 2
                                     :venue-name "Nippon Gaishi Hall"
                                     :address {:postal-code "457-0833"
                                               :prefecture "Aichi-ken"
                                               :city "Nagoya-shi"
                                               :address1 "Minami-ku"
                                               :address2 "5-1-16 Higashimatabeecho"}}]]
             (handler {:ataraxy/result [:venue/list group-id]})))))

(t/deftest test-create
  (let [handler (ig/init-key ::sut/create
                             {:db (shrubbery/stub
                                   rest-server.boundary.db.venue/Venues
                                   {:create-venue 3})})
        group-id 1
        venue {:venue-name "Kobe World Memorial Hall"
               :address {:postal-code "650-0046"
                         :prefecture "Hyogo-ken"
                         :city "Kobe-shi"
                         :address1 "Chuo-ku"
                         :address2 "6-6-12-2 Minatojima Nakamachi"}}]
    (t/is (= [:ataraxy.response/ok {:venue-id 3
                                    :venue-name "Kobe World Memorial Hall"
                                    :address {:postal-code "650-0046"
                                              :prefecture "Hyogo-ken"
                                              :city "Kobe-shi"
                                              :address1 "Chuo-ku"
                                              :address2 "6-6-12-2 Minatojima Nakamachi"}}]
             (handler {:ataraxy/result [:venue/create group-id venue]})))))
