(ns rest-server.handler.group-test
  (:require [clojure.test :as t]
            [integrant.core :as ig]
            [rest-server.handler.group :as sut]
            [shrubbery.core :as shrubbery]))

(t/deftest test-list
  (let [handler (ig/init-key ::sut/list
                             {:db (reify
                                    rest-server.boundary.db.group/Groups
                                    (list-groups [_]
                                      [{:id 1
                                        :name "μ's"
                                        :created-at #inst "2010-06-29T15:00:00.000Z"}
                                       {:id 2
                                        :name "Aqours"
                                        :created-at #inst "2015-06-29T15:00:00.000Z"}])
                                    (fetch-group-admin-members [_ group-id]
                                      (get {1 [{:id 1
                                                :first-name "Umi"
                                                :last-name "Sonoda"
                                                :email "u.sonoda@otonokizaka.ac.jp"}
                                               {:id 2
                                                :first-name "Eli"
                                                :last-name "Ayase"
                                                :email "e.ayase@otonokizaka.ac.jp"}]
                                            2 [{:id 3
                                                :first-name "You"
                                                :last-name "Watanabe"
                                                :email "y.watanabe@uranohoshi.ac.jp"}
                                               {:id 4
                                                :first-name "Yoshiko"
                                                :last-name "Tsushima"
                                                :email "y.tsushima@uranohoshi.ac.jp"}]}
                                           group-id))
                                    rest-server.boundary.db.venue/Venues
                                    (list-venues [_ group-id venue-type]
                                      (if (= venue-type :venue-type/physical)
                                        (get {1 [{:id 1
                                                  :name "Tokyo Dome"
                                                  :postal-code "112-0004"
                                                  :prefecture "Tokyo-to"
                                                  :city "Bunkyo-ku"
                                                  :street1 "1-3-61 Koraku"
                                                  :street2 ""
                                                  :group-id 1
                                                  :url nil}]
                                              2 [{:id 2
                                                  :name "Yokohama Arena"
                                                  :postal-code "222-0033"
                                                  :prefecture "Kanagawa-ken"
                                                  :city "Yokohama-shi"
                                                  :street1 "Kohoku-ku"
                                                  :street2 "3-10 Shinyokohama"
                                                  :group-id 2
                                                  :url nil}
                                                 {:id 3
                                                  :name "Nippon Gaishi Hall"
                                                  :postal-code "457-0833"
                                                  :prefecture "Aichi-ken"
                                                  :city "Nagoya-shi"
                                                  :street1 "Minami-ku"
                                                  :street2 "5-1-16 Higashimatabeecho"
                                                  :group-id 2
                                                  :url nil}]}
                                             group-id)
                                        (get {2 [{:id 4
                                                  :name "ニコニコ動画"
                                                  :postal-code nil
                                                  :prefecture nil
                                                  :city nil
                                                  :street1 nil
                                                  :street2 nil
                                                  :group-id 2
                                                  :url "http://www.nicovideo.jp/"}]}
                                             group-id)))
                                    (fetch-venue [_ venue-id]
                                      (get {1 {:id 1
                                               :name "Tokyo Dome"
                                               :postal-code "112-0004"
                                               :prefecture "Tokyo-to"
                                               :city "Bunkyo-ku"
                                               :street1 "1-3-61 Koraku"
                                               :street2 ""
                                               :group-id 1
                                               :url nil}
                                            2 {:id 2
                                               :name "Yokohama Arena"
                                               :postal-code "222-0033"
                                               :prefecture "Kanagawa-ken"
                                               :city "Yokohama-shi"
                                               :street1 "Kohoku-ku"
                                               :street2 "3-10 Shinyokohama"
                                               :group-id 2
                                               :url nil}
                                            3 {:id 3
                                               :name "Nippon Gaishi Hall"
                                               :postal-code "457-0833"
                                               :prefecture "Aichi-ken"
                                               :city "Nagoya-shi"
                                               :street1 "Minami-ku"
                                               :street2 "5-1-16 Higashimatabeecho"
                                               :group-id 2
                                               :url nil}
                                            4 {:id 4
                                               :name "ニコニコ動画"
                                               :postal-code nil
                                               :prefecture nil
                                               :city nil
                                               :street1 nil
                                               :street2 nil
                                               :group-id 2
                                               :url "http://www.nicovideo.jp/"}}
                                           venue-id))
                                    rest-server.boundary.db.meetup/Meetups
                                    (list-meetups [_ group-id]
                                      (get {1 [{:id 1
                                                :title "μ's Final LoveLive!"
                                                :start-at #inst "2016-03-31T05:00:00.000Z"
                                                :end-at #inst "2016-03-31T11:00:00.000Z"
                                                :venue-id 1
                                                :group-id 1
                                                :online-venue-id nil}]
                                            2 [{:id 2
                                                :title "Aqours First LoveLive!"
                                                :start-at #inst "2017-02-25T07:30:00.000Z"
                                                :end-at #inst "2017-02-25T12:30:00.000Z"
                                                :venue-id 2
                                                :group-id 2
                                                :online-venue-id nil}
                                               {:id 3
                                                :title "Aqours 2nd LoveLive! Nagoya"
                                                :start-at #inst "2017-08-05T08:00:00.000Z"
                                                :end-at #inst "2017-08-05T13:00:00.000Z"
                                                :venue-id 3
                                                :group-id 2
                                                :online-venue-id 4}]}
                                           group-id))
                                    (fetch-meetup-members [_ meetup-id]
                                      (get {1 [{:id 1
                                                :first-name "Umi"
                                                :last-name "Sonoda"
                                                :email "u.sonoda@otonokizaka.ac.jp"}
                                               {:id 2
                                                :first-name "Eli"
                                                :last-name "Ayase"
                                                :email "e.ayase@otonokizaka.ac.jp"}
                                               {:id 5
                                                :first-name "Maki"
                                                :last-name "Nishikino"
                                                :email "m.nishikino@otonokizaka.ac.jp"}]
                                            2 [{:id 3
                                                :first-name "You"
                                                :last-name "Watanabe"
                                                :email "y.watanabe@uranohoshi.ac.jp"}
                                               {:id 4
                                                :first-name "Yoshiko"
                                                :last-name "Tsushima"
                                                :email "y.tsushima@uranohoshi.ac.jp"}
                                               {:id 6
                                                :first-name "Dia"
                                                :last-name "Kurosawa"
                                                :email "d.kurosawa@uranohoshi.ac.jp"}]
                                            3 [{:id 3
                                                :first-name "You"
                                                :last-name "Watanabe"
                                                :email "y.watanabe@uranohoshi.ac.jp"}
                                               {:id 4
                                                :first-name "Yoshiko"
                                                :last-name "Tsushima"
                                                :email "y.tsushima@uranohoshi.ac.jp"}]}
                                           meetup-id)))})]
    (t/is (= [:ataraxy.response/ok [{:group-id 1
                                     :group-name "μ's"
                                     :admin [{:member-id 1
                                              :first-name "Umi"
                                              :last-name "Sonoda"
                                              :email "u.sonoda@otonokizaka.ac.jp"}
                                             {:member-id 2
                                              :first-name "Eli"
                                              :last-name "Ayase"
                                              :email "e.ayase@otonokizaka.ac.jp"}]
                                     :venues [{:venue-id 1
                                               :venue-name "Tokyo Dome"
                                               :address {:postal-code "112-0004"
                                                         :prefecture "Tokyo-to"
                                                         :city "Bunkyo-ku"
                                                         :address1 "1-3-61 Koraku"
                                                         :address2 ""}}]
                                     :online-venues []
                                     :meetups [{:event-id 1
                                                :title "μ's Final LoveLive!"
                                                :start-at #inst "2016-03-31T05:00:00.000Z"
                                                :end-at #inst "2016-03-31T11:00:00.000Z"
                                                :venue {:venue-id 1
                                                        :venue-name "Tokyo Dome"
                                                        :address {:postal-code "112-0004"
                                                                  :prefecture "Tokyo-to"
                                                                  :city "Bunkyo-ku"
                                                                  :address1 "1-3-61 Koraku"
                                                                  :address2 ""}}
                                                :online-venue nil
                                                :members [{:member-id 1
                                                           :first-name "Umi"
                                                           :last-name "Sonoda"
                                                           :email "u.sonoda@otonokizaka.ac.jp"}
                                                          {:member-id 2
                                                           :first-name "Eli"
                                                           :last-name "Ayase"
                                                           :email "e.ayase@otonokizaka.ac.jp"}
                                                          {:member-id 5
                                                           :first-name "Maki"
                                                           :last-name "Nishikino"
                                                           :email "m.nishikino@otonokizaka.ac.jp"}]}]}
                                    {:group-id 2
                                     :group-name "Aqours"
                                     :admin [{:member-id 3
                                              :first-name "You"
                                              :last-name "Watanabe"
                                              :email "y.watanabe@uranohoshi.ac.jp"}
                                             {:member-id 4
                                              :first-name "Yoshiko"
                                              :last-name "Tsushima"
                                              :email "y.tsushima@uranohoshi.ac.jp"}]
                                     :venues [{:venue-id 2
                                               :venue-name "Yokohama Arena"
                                               :address {:postal-code "222-0033"
                                                         :prefecture "Kanagawa-ken"
                                                         :city "Yokohama-shi"
                                                         :address1 "Kohoku-ku"
                                                         :address2 "3-10 Shinyokohama"}}
                                              {:venue-id 3
                                               :venue-name "Nippon Gaishi Hall"
                                               :address {:postal-code "457-0833"
                                                         :prefecture "Aichi-ken"
                                                         :city "Nagoya-shi"
                                                         :address1 "Minami-ku"
                                                         :address2 "5-1-16 Higashimatabeecho"}}]
                                     :online-venues [{:online-venue-id 4
                                                      :venue-name "ニコニコ動画"
                                                      :url "http://www.nicovideo.jp/"}]
                                     :meetups [{:event-id 2
                                                :title "Aqours First LoveLive!"
                                                :start-at #inst "2017-02-25T07:30:00.000Z"
                                                :end-at #inst "2017-02-25T12:30:00.000Z"
                                                :venue {:venue-id 2
                                                        :venue-name "Yokohama Arena"
                                                        :address {:postal-code "222-0033"
                                                                  :prefecture "Kanagawa-ken"
                                                                  :city "Yokohama-shi"
                                                                  :address1 "Kohoku-ku"
                                                                  :address2 "3-10 Shinyokohama"}}
                                                :online-venue nil
                                                :members [{:member-id 3
                                                           :first-name "You"
                                                           :last-name "Watanabe"
                                                           :email "y.watanabe@uranohoshi.ac.jp"}
                                                          {:member-id 4
                                                           :first-name "Yoshiko"
                                                           :last-name "Tsushima"
                                                           :email "y.tsushima@uranohoshi.ac.jp"}
                                                          {:member-id 6
                                                           :first-name "Dia"
                                                           :last-name "Kurosawa"
                                                           :email "d.kurosawa@uranohoshi.ac.jp"}]}
                                               {:event-id 3
                                                :title "Aqours 2nd LoveLive! Nagoya"
                                                :start-at #inst "2017-08-05T08:00:00.000Z"
                                                :end-at #inst "2017-08-05T13:00:00.000Z"
                                                :venue {:venue-id 3
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
                                                           :first-name "You"
                                                           :last-name "Watanabe"
                                                           :email "y.watanabe@uranohoshi.ac.jp"}
                                                          {:member-id 4
                                                           :first-name "Yoshiko"
                                                           :last-name "Tsushima"
                                                           :email "y.tsushima@uranohoshi.ac.jp"}]}]}]]
             (handler {:ataraxy/result [:group/list]})))))

(t/deftest test-create
  (let [handler (ig/init-key ::sut/create
                             {:db (shrubbery/stub
                                   rest-server.boundary.db.group/Groups
                                   {:create-group 3
                                    :create-group-members nil}
                                   rest-server.boundary.db.member/Members
                                   {:fetch-members [{:id 7
                                                     :first-name "Sarah"
                                                     :last-name "Kazuno"
                                                     :email "s.kazuno@hakodate-seisen.ac.jp"}
                                                    {:id 8
                                                     :first-name "Leah"
                                                     :last-name "Kazuno"
                                                     :email "l.kazuno@hakodate-seisen.ac.jp"}]})})
        group {:group-name "Saint Snow"
               :admin-member-ids [7 8]}]
    (t/is (= [:ataraxy.response/ok {:group-id 3
                                    :group-name "Saint Snow"
                                    :admin [{:member-id 7
                                             :first-name "Sarah"
                                             :last-name "Kazuno"
                                             :email "s.kazuno@hakodate-seisen.ac.jp"}
                                            {:member-id 8
                                             :first-name "Leah"
                                             :last-name "Kazuno"
                                             :email "l.kazuno@hakodate-seisen.ac.jp"}]}]
             (handler {:ataraxy/result [:group/create group]})))))

(t/deftest test-join
  (let [handler (ig/init-key ::sut/join
                             {:db (reify
                                    rest-server.boundary.db.group/Groups
                                    (create-group-members [_ _] nil)
                                    (fetch-group [_ _]
                                      {:id 2
                                       :name "Aqours"
                                       :created-at #inst "2015-06-29T15:00:00.000Z"})
                                    (fetch-group-admin-members [_ _]
                                      [{:id 3
                                        :first-name "You"
                                        :last-name "Watanabe"
                                        :email "y.watanabe@uranohoshi.ac.jp"}
                                       {:id 4
                                        :first-name "Yoshiko"
                                        :last-name "Tsushima"
                                        :email "y.tsushima@uranohoshi.ac.jp"}
                                       {:id 6
                                        :first-name "Dia"
                                        :last-name "Kurosawa"
                                        :email "d.kurosawa@uranohoshi.ac.jp"}])
                                    rest-server.boundary.db.venue/Venues
                                    (list-venues [_ _ venue-type]
                                      (if (= venue-type :venue-type/physical)
                                        [{:id 2
                                          :name "Yokohama Arena"
                                          :postal-code "222-0033"
                                          :prefecture "Kanagawa-ken"
                                          :city "Yokohama-shi"
                                          :street1 "Kohoku-ku"
                                          :street2 "3-10 Shinyokohama"
                                          :group-id 2
                                          :url nil}
                                         {:id 3
                                          :name "Nippon Gaishi Hall"
                                          :postal-code "457-0833"
                                          :prefecture "Aichi-ken"
                                          :city "Nagoya-shi"
                                          :street1 "Minami-ku"
                                          :street2 "5-1-16 Higashimatabeecho"
                                          :group-id 2
                                          :url nil}]
                                        [{:id 4
                                          :name "ニコニコ動画"
                                          :postal-code nil
                                          :prefecture nil
                                          :city nil
                                          :street1 nil
                                          :street2 nil
                                          :group-id 2
                                          :url "http://www.nicovideo.jp/"}]))
                                    (fetch-venue [_ venue-id]
                                      (get {2 {:id 2
                                               :name "Yokohama Arena"
                                               :postal-code "222-0033"
                                               :prefecture "Kanagawa-ken"
                                               :city "Yokohama-shi"
                                               :street1 "Kohoku-ku"
                                               :street2 "3-10 Shinyokohama"
                                               :group-id 2
                                               :url nil}
                                            3 {:id 3
                                               :name "Nippon Gaishi Hall"
                                               :postal-code "457-0833"
                                               :prefecture "Aichi-ken"
                                               :city "Nagoya-shi"
                                               :street1 "Minami-ku"
                                               :street2 "5-1-16 Higashimatabeecho"
                                               :group-id 2
                                               :url nil}
                                            4 {:id 4
                                               :name "ニコニコ動画"
                                               :postal-code nil
                                               :prefecture nil
                                               :city nil
                                               :street1 nil
                                               :street2 nil
                                               :group-id 2
                                               :url "http://www.nicovideo.jp/"}}
                                           venue-id))
                                    rest-server.boundary.db.meetup/Meetups
                                    (list-meetups [_ _]
                                      [{:id 2
                                        :title "Aqours First LoveLive!"
                                        :start-at #inst "2017-02-25T07:30:00.000Z"
                                        :end-at #inst "2017-02-25T12:30:00.000Z"
                                        :venue-id 2
                                        :group-id 2
                                        :online-venue-id nil}
                                       {:id 3
                                        :title "Aqours 2nd LoveLive! Nagoya"
                                        :start-at #inst "2017-08-05T08:00:00.000Z"
                                        :end-at #inst "2017-08-05T13:00:00.000Z"
                                        :venue-id 3
                                        :group-id 2
                                        :online-venue-id 4}])
                                    (fetch-meetup-members [_ meetup-id]
                                      (get {2 [{:id 3
                                                :first-name "You"
                                                :last-name "Watanabe"
                                                :email "y.watanabe@uranohoshi.ac.jp"}
                                               {:id 4
                                                :first-name "Yoshiko"
                                                :last-name "Tsushima"
                                                :email "y.tsushima@uranohoshi.ac.jp"}
                                               {:id 6
                                                :first-name "Dia"
                                                :last-name "Kurosawa"
                                                :email "d.kurosawa@uranohoshi.ac.jp"}]
                                            3 [{:id 3
                                                :first-name "You"
                                                :last-name "Watanabe"
                                                :email "y.watanabe@uranohoshi.ac.jp"}
                                               {:id 4
                                                :first-name "Yoshiko"
                                                :last-name "Tsushima"
                                                :email "y.tsushima@uranohoshi.ac.jp"}]}
                                           meetup-id)))})
        member-id 6
        group-id 2
        group-member {:admin true}]
    (t/is (= [:ataraxy.response/ok {:group-id 2
                                    :group-name "Aqours"
                                    :admin [{:member-id 3
                                             :first-name "You"
                                             :last-name "Watanabe"
                                             :email "y.watanabe@uranohoshi.ac.jp"}
                                            {:member-id 4
                                             :first-name "Yoshiko"
                                             :last-name "Tsushima"
                                             :email "y.tsushima@uranohoshi.ac.jp"}
                                            {:member-id 6
                                             :first-name "Dia"
                                             :last-name "Kurosawa"
                                             :email "d.kurosawa@uranohoshi.ac.jp"}]
                                    :venues [{:venue-id 2
                                              :venue-name "Yokohama Arena"
                                              :address {:postal-code "222-0033"
                                                        :prefecture "Kanagawa-ken"
                                                        :city "Yokohama-shi"
                                                        :address1 "Kohoku-ku"
                                                        :address2 "3-10 Shinyokohama"}}
                                             {:venue-id 3
                                              :venue-name "Nippon Gaishi Hall"
                                              :address {:postal-code "457-0833"
                                                        :prefecture "Aichi-ken"
                                                        :city "Nagoya-shi"
                                                        :address1 "Minami-ku"
                                                        :address2 "5-1-16 Higashimatabeecho"}}]
                                    :online-venues [{:online-venue-id 4
                                                     :venue-name "ニコニコ動画"
                                                     :url "http://www.nicovideo.jp/"}]
                                    :meetups [{:event-id 2
                                               :title "Aqours First LoveLive!"
                                               :start-at #inst "2017-02-25T07:30:00.000Z"
                                               :end-at #inst "2017-02-25T12:30:00.000Z"
                                               :venue {:venue-id 2
                                                       :venue-name "Yokohama Arena"
                                                       :address {:postal-code "222-0033"
                                                                 :prefecture "Kanagawa-ken"
                                                                 :city "Yokohama-shi"
                                                                 :address1 "Kohoku-ku"
                                                                 :address2 "3-10 Shinyokohama"}}
                                               :online-venue nil
                                               :members [{:member-id 3
                                                          :first-name "You"
                                                          :last-name "Watanabe"
                                                          :email "y.watanabe@uranohoshi.ac.jp"}
                                                         {:member-id 4
                                                          :first-name "Yoshiko"
                                                          :last-name "Tsushima"
                                                          :email "y.tsushima@uranohoshi.ac.jp"}
                                                         {:member-id 6
                                                          :first-name "Dia"
                                                          :last-name "Kurosawa"
                                                          :email "d.kurosawa@uranohoshi.ac.jp"}]}
                                              {:event-id 3
                                               :title "Aqours 2nd LoveLive! Nagoya"
                                               :start-at #inst "2017-08-05T08:00:00.000Z"
                                               :end-at #inst "2017-08-05T13:00:00.000Z"
                                               :venue {:venue-id 3
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
                                                          :first-name "You"
                                                          :last-name "Watanabe"
                                                          :email "y.watanabe@uranohoshi.ac.jp"}
                                                         {:member-id 4
                                                          :first-name "Yoshiko"
                                                          :last-name "Tsushima"
                                                          :email "y.tsushima@uranohoshi.ac.jp"}]}]}]
             (handler {:ataraxy/result [:group/join member-id group-id group-member]})))))
