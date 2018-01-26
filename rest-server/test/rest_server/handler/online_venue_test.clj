(ns rest-server.handler.online-venue-test
  (:require [clojure.test :as t]
            [integrant.core :as ig]
            [rest-server.handler.online-venue :as sut]
            [shrubbery.core :as shrubbery]))

(t/deftest test-list
  (let [handler (ig/init-key ::sut/list
                             {:db (shrubbery/stub
                                   rest-server.boundary.db.venue/Venues
                                   {:list-venues [{:id 1
                                                   :name "ニコニコ動画"
                                                   :postal-code nil
                                                   :prefecture nil
                                                   :city nil
                                                   :street1 nil
                                                   :street2 nil
                                                   :group-id 1
                                                   :url "http://www.nicovideo.jp/"}
                                                  {:id 2
                                                   :name "AbemaTV"
                                                   :postal-code nil
                                                   :prefecture nil
                                                   :city nil
                                                   :street1 nil
                                                   :street2 nil
                                                   :group-id 1
                                                   :url "https://abema.tv/"}]})})
        group-id 1]
    (t/is (= [:ataraxy.response/ok [{:online-venue-id 1
                                     :venue-name "ニコニコ動画"
                                     :url "http://www.nicovideo.jp/"}
                                    {:online-venue-id 2
                                     :venue-name "AbemaTV"
                                     :url "https://abema.tv/"}]]
             (handler {:ataraxy/result [:venue/list group-id]})))))

(t/deftest test-create
  (let [handler (ig/init-key ::sut/create
                             {:db (shrubbery/stub
                                   rest-server.boundary.db.venue/Venues
                                   {:create-venue 3})})
        group-id 1
        venue {:venue-name "LINE LIVE"
               :url "https://live.line.me/"}]
    (t/is (= [:ataraxy.response/ok {:online-venue-id 3
                                    :venue-name "LINE LIVE"
                                    :url "https://live.line.me/"}]
             (handler {:ataraxy/result [:venue/create group-id venue]})))))
