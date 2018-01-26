(ns rest-server.handler.meetup-test
  (:require [clojure.test :as t]
            [integrant.core :as ig]
            [rest-server.handler.meetup :as sut]
            [shrubbery.core :as shrubbery]))

(t/deftest test-list
  (let [handler (ig/init-key ::sut/list
                             {:db (reify
                                    rest-server.boundary.db.meetup/Meetups
                                    (list-meetups [_ _]
                                      [{:id 1
                                        :title "Aqours First LoveLive!"
                                        :start-at #inst "2017-02-25T07:30:00.000Z"
                                        :end-at #inst "2017-02-25T12:30:00.000Z"
                                        :venue-id 1
                                        :group-id 1
                                        :online-venue-id nil}
                                       {:id 2
                                        :title "Aqours 2nd LoveLive! Nagoya"
                                        :start-at #inst "2017-08-05T08:00:00.000Z"
                                        :end-at #inst "2017-08-05T13:00:00.000Z"
                                        :venue-id 2
                                        :group-id 1
                                        :online-venue-id 4}])
                                    (fetch-meetup-members [_ meetup-id]
                                      (get {1 [{:id 1
                                                :first-name "You"
                                                :last-name "Watanabe"
                                                :email "y.watanabe@uranohoshi.ac.jp"}
                                               {:id 2
                                                :first-name "Yoshiko"
                                                :last-name "Tsushima"
                                                :email "y.tsushima@uranohoshi.ac.jp"}]
                                            2 [{:id 3
                                                :first-name "Dia"
                                                :last-name "Kurosawa"
                                                :email "d.kurosawa@uranohoshi.ac.jp"}
                                               {:id 4
                                                :first-name "Ruby"
                                                :last-name "Kurosawa"
                                                :email "r.kurosawa@uranohoshi.ac.jp"}]}
                                           meetup-id))
                                    rest-server.boundary.db.venue/Venues
                                    (fetch-venue [_ venue-id]
                                      (get {1 {:id 1
                                               :name "Yokohama Arena"
                                               :postal-code "222-0033"
                                               :prefecture "Kanagawa-ken"
                                               :city "Yokohama-shi"
                                               :street1 "Kohoku-ku"
                                               :street2 "3-10 Shinyokohama"
                                               :group-id 1
                                               :url nil}
                                            2 {:id 2
                                               :name "Nippon Gaishi Hall"
                                               :postal-code "457-0833"
                                               :prefecture "Aichi-ken"
                                               :city "Nagoya-shi"
                                               :street1 "Minami-ku"
                                               :street2 "5-1-16 Higashimatabeecho"
                                               :group-id 1
                                               :url nil}
                                            4 {:id 4
                                               :name "ニコニコ動画"
                                               :postal-code nil
                                               :prefecture nil
                                               :city nil
                                               :street1 nil
                                               :street2 nil
                                               :group-id 1
                                               :url "http://www.nicovideo.jp/"}}
                                           venue-id)))})
        group-id 1]
    (t/is (= [:ataraxy.response/ok [{:event-id 1
                                     :title "Aqours First LoveLive!"
                                     :start-at #inst "2017-02-25T07:30:00.000Z"
                                     :end-at #inst "2017-02-25T12:30:00.000Z"
                                     :venue {:venue-id 1
                                             :venue-name "Yokohama Arena"
                                             :address {:postal-code "222-0033"
                                                       :prefecture "Kanagawa-ken"
                                                       :city "Yokohama-shi"
                                                       :address1 "Kohoku-ku"
                                                       :address2 "3-10 Shinyokohama"}}
                                     :online-venue nil
                                     :members [{:member-id 1
                                                :first-name "You"
                                                :last-name "Watanabe"
                                                :email "y.watanabe@uranohoshi.ac.jp"}
                                               {:member-id 2
                                                :first-name "Yoshiko"
                                                :last-name "Tsushima"
                                                :email "y.tsushima@uranohoshi.ac.jp"}]}
                                    {:event-id 2
                                     :title "Aqours 2nd LoveLive! Nagoya"
                                     :start-at #inst "2017-08-05T08:00:00.000Z"
                                     :end-at #inst "2017-08-05T13:00:00.000Z"
                                     :venue {:venue-id 2
                                             :venue-name "Nippon Gaishi Hall"
                                             :address {:postal-code "457-0833"
                                                       :prefecture "Aichi-ken"
                                                       :city "Nagoya-shi"
                                                       :address1 "Minami-ku"
                                                       :address2 "5-1-16 Higashimatabeecho"}}
                                     :online-venue {:online-venue-id 4
                                                    :venue-name "ニコニコ動画"
                                                    :url "http://www.nicovideo.jp/"}
                                     :members [{:member-id 3
                                                :first-name "Dia"
                                                :last-name "Kurosawa"
                                                :email "d.kurosawa@uranohoshi.ac.jp"}
                                               {:member-id 4
                                                :first-name "Ruby"
                                                :last-name "Kurosawa"
                                                :email "r.kurosawa@uranohoshi.ac.jp"}]}]]
             (handler {:ataraxy/result [:meetup/list group-id]})))))

(t/deftest test-create
  (let [handler (ig/init-key ::sut/create
                             {:db (reify
                                    rest-server.boundary.db.meetup/Meetups
                                    (create-meetup [_ _] 3)
                                    (fetch-meetup-members [_ _] [])
                                    rest-server.boundary.db.venue/Venues
                                    (fetch-venue [_ venue-id]
                                      (get {3 {:id 3
                                               :name "Kobe World Memorial Hall"
                                               :postal-code "650-0046"
                                               :prefecture "Hyogo-ken"
                                               :city "Kobe-shi"
                                               :street1 "Chuo-ku"
                                               :street2 "6-6-12-2 Minatojima Nakamachi"
                                               :group-id 1
                                               :url nil}
                                            5 {:id 5
                                               :name "AbemaTV"
                                               :postal-code nil
                                               :prefecture nil
                                               :city nil
                                               :street1 nil
                                               :street2 nil
                                               :group-id 1
                                               :url "https://abema.tv/"}}
                                           venue-id)))})
        group-id 1
        meetup {:title "Aqours 2nd LoveLive! Kobe"
                :start-at "2017-08-19T08:00:00.000Z"
                :end-at "2017-08-19T13:00:00.000Z"
                :venue-id 3
                :online-venue-id 5}]
    (t/is (= [:ataraxy.response/ok {:event-id 3
                                    :title "Aqours 2nd LoveLive! Kobe"
                                    :start-at #inst "2017-08-19T08:00:00.000Z"
                                    :end-at #inst "2017-08-19T13:00:00.000Z"
                                    :venue {:venue-id 3
                                            :venue-name "Kobe World Memorial Hall"
                                            :address {:postal-code "650-0046"
                                                      :prefecture "Hyogo-ken"
                                                      :city "Kobe-shi"
                                                      :address1 "Chuo-ku"
                                                      :address2 "6-6-12-2 Minatojima Nakamachi"}}
                                    :online-venue {:online-venue-id 5
                                                   :venue-name "AbemaTV"
                                                   :url "https://abema.tv/"}
                                    :members []}]
             (handler {:ataraxy/result [:meetup/create group-id meetup]})))))

(t/deftest test-fetch
  (t/testing "found"
    (let [handler (ig/init-key ::sut/fetch
                               {:db (reify
                                      rest-server.boundary.db.meetup/Meetups
                                      (fetch-meetup [_ _]
                                        {:id 2
                                         :title "Aqours 2nd LoveLive! Nagoya"
                                         :start-at #inst "2017-08-05T08:00:00.000Z"
                                         :end-at #inst "2017-08-05T13:00:00.000Z"
                                         :venue-id 2
                                         :group-id 1
                                         :online-venue-id 4})
                                      (fetch-meetup-members [_ _]
                                        [{:id 3
                                          :first-name "Dia"
                                          :last-name "Kurosawa"
                                          :email "d.kurosawa@uranohoshi.ac.jp"}
                                         {:id 4
                                          :first-name "Ruby"
                                          :last-name "Kurosawa"
                                          :email "r.kurosawa@uranohoshi.ac.jp"}])
                                      rest-server.boundary.db.venue/Venues
                                      (fetch-venue [_ venue-id]
                                        (get {2 {:id 2
                                                 :name "Nippon Gaishi Hall"
                                                 :postal-code "457-0833"
                                                 :prefecture "Aichi-ken"
                                                 :city "Nagoya-shi"
                                                 :street1 "Minami-ku"
                                                 :street2 "5-1-16 Higashimatabeecho"
                                                 :group-id 1
                                                 :url nil}
                                              4 {:id 4
                                                 :name "ニコニコ動画"
                                                 :postal-code nil
                                                 :prefecture nil
                                                 :city nil
                                                 :street1 nil
                                                 :street2 nil
                                                 :group-id 1
                                                 :url "http://www.nicovideo.jp/"}}
                                             venue-id)))})
          group-id 1
          meetup-id 2]
      (t/is (= [:ataraxy.response/ok {:event-id 2
                                      :title "Aqours 2nd LoveLive! Nagoya"
                                      :start-at #inst "2017-08-05T08:00:00.000Z"
                                      :end-at #inst "2017-08-05T13:00:00.000Z"
                                      :venue {:venue-id 2
                                              :venue-name "Nippon Gaishi Hall"
                                              :address {:postal-code "457-0833"
                                                        :prefecture "Aichi-ken"
                                                        :city "Nagoya-shi"
                                                        :address1 "Minami-ku"
                                                        :address2 "5-1-16 Higashimatabeecho"}}
                                      :online-venue {:online-venue-id 4
                                                     :venue-name "ニコニコ動画"
                                                     :url "http://www.nicovideo.jp/"}
                                      :members [{:member-id 3
                                                 :first-name "Dia"
                                                 :last-name "Kurosawa"
                                                 :email "d.kurosawa@uranohoshi.ac.jp"}
                                                {:member-id 4
                                                 :first-name "Ruby"
                                                 :last-name "Kurosawa"
                                                 :email "r.kurosawa@uranohoshi.ac.jp"}]}]
               (handler {:ataraxy/result [:meetup/fetch group-id meetup-id]})))))
  (t/testing "not found"
    (let [handler (ig/init-key ::sut/fetch
                               {:db (shrubbery/stub
                                     rest-server.boundary.db.meetup/Meetups
                                     {:fetch-meetup nil})})
          group-id 1
          meetup-id 100]
      (t/is (nil? (handler {:ataraxy/result [:meetup/fetch group-id meetup-id]}))))))

(t/deftest test-join
  (let [handler (ig/init-key ::sut/join
                             {:db (reify
                                    rest-server.boundary.db.meetup/Meetups
                                    (create-meetup-member [_ _] nil)
                                    (fetch-meetup [_ _]
                                      {:id 2
                                       :title "Aqours 2nd LoveLive! Nagoya"
                                       :start-at #inst "2017-08-05T08:00:00.000Z"
                                       :end-at #inst "2017-08-05T13:00:00.000Z"
                                       :venue-id 2
                                       :group-id 1
                                       :online-venue-id 4})
                                    (fetch-meetup-members [_ _]
                                      [{:id 1
                                        :first-name "You"
                                        :last-name "Watanabe"
                                        :email "y.watanabe@uranohoshi.ac.jp"}
                                       {:id 3
                                        :first-name "Dia"
                                        :last-name "Kurosawa"
                                        :email "d.kurosawa@uranohoshi.ac.jp"}
                                       {:id 4
                                        :first-name "Ruby"
                                        :last-name "Kurosawa"
                                        :email "r.kurosawa@uranohoshi.ac.jp"}])
                                    rest-server.boundary.db.venue/Venues
                                    (fetch-venue [_ venue-id]
                                      (get {2 {:id 2
                                               :name "Nippon Gaishi Hall"
                                               :postal-code "457-0833"
                                               :prefecture "Aichi-ken"
                                               :city "Nagoya-shi"
                                               :street1 "Minami-ku"
                                               :street2 "5-1-16 Higashimatabeecho"
                                               :group-id 1
                                               :url nil}
                                            4 {:id 4
                                               :name "ニコニコ動画"
                                               :postal-code nil
                                               :prefecture nil
                                               :city nil
                                               :street1 nil
                                               :street2 nil
                                               :group-id 1
                                               :url "http://www.nicovideo.jp/"}}
                                           venue-id)))})
        member-id 1
        meetup-id 2]
    (t/is (= [:ataraxy.response/ok {:event-id 2
                                    :title "Aqours 2nd LoveLive! Nagoya"
                                    :start-at #inst "2017-08-05T08:00:00.000Z"
                                    :end-at #inst "2017-08-05T13:00:00.000Z"
                                    :venue {:venue-id 2
                                            :venue-name "Nippon Gaishi Hall"
                                            :address {:postal-code "457-0833"
                                                      :prefecture "Aichi-ken"
                                                      :city "Nagoya-shi"
                                                      :address1 "Minami-ku"
                                                      :address2 "5-1-16 Higashimatabeecho"}}
                                    :online-venue {:online-venue-id 4
                                                   :venue-name "ニコニコ動画"
                                                   :url "http://www.nicovideo.jp/"}
                                    :members [{:member-id 1
                                               :first-name "You"
                                               :last-name "Watanabe"
                                               :email "y.watanabe@uranohoshi.ac.jp"}
                                              {:member-id 3
                                               :first-name "Dia"
                                               :last-name "Kurosawa"
                                               :email "d.kurosawa@uranohoshi.ac.jp"}
                                              {:member-id 4
                                               :first-name "Ruby"
                                               :last-name "Kurosawa"
                                               :email "r.kurosawa@uranohoshi.ac.jp"}]}]
             (handler {:ataraxy/result [:meetup/join member-id meetup-id]})))))
