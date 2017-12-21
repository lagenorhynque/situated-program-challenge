(ns rest-server.handler.meetup
  (:require [ataraxy.response :as response]
            [clojure.set :as set]
            [integrant.core :as ig]
            [rest-server.boundary.meetup :as db-meetup]
            [rest-server.boundary.venue :as db-venue]
            [rest-server.handler.member :as member]
            [rest-server.handler.venue :as venue]
            [rest-server.util :as util]))

(defn meetup-with-venue-and-members [{:keys [id title start-at end-at]} venue members]
  {:event-id id
   :title title
   :start-at start-at
   :end-at end-at
   :venue (venue/venue-with-address venue)
   :members (map member/member-with-id members)})

(defn fetch-meetup-detail [db {:keys [id venue-id] :as meetup}]
  (let [venue (db-venue/fetch-venue db venue-id)
        members (db-meetup/fetch-meetup-members db id)]
    (meetup-with-venue-and-members meetup venue members)))

(defn get-meetup [db meetup-id]
  (when-let [meetup (db-meetup/fetch-meetup db meetup-id)]
    (fetch-meetup-detail db meetup)))

(defmethod ig/init-key ::list [_ {:keys [db]}]
  (fn [{[_ group-id] :ataraxy/result}]
    [::response/ok (map (partial fetch-meetup-detail db)
                        (db-meetup/list-meetups db group-id))]))

(defmethod ig/init-key ::create [_ {:keys [db]}]
  (fn [{[_ group-id {:keys [start-at end-at] :as meetup}] :ataraxy/result}]
    (let [meetup' (assoc meetup
                         :start-at (util/string->timestamp start-at)
                         :end-at (util/string->timestamp end-at)
                         :group-id group-id)
          id (db-meetup/create-meetup db meetup')]
      [::response/ok (-> meetup'
                         (assoc :id id)
                         ((partial fetch-meetup-detail db)))])))

(defmethod ig/init-key ::fetch [_ {:keys [db]}]
  (fn [{[_ _ meetup-id] :ataraxy/result}]
    (when-let [meetup (get-meetup db meetup-id)]
      [::response/ok meetup])))

(defmethod ig/init-key ::join [_ {:keys [db]}]
  (fn [{[_ member-id meetup-id] :ataraxy/result}]
    (db-meetup/create-meetup-member db {:meetup-id meetup-id
                                        :member-id member-id})
    [::response/ok (get-meetup db meetup-id)]))
