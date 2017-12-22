(ns rest-server.handler.member
  (:require [ataraxy.response :as response]
            [clojure.set :as set]
            [integrant.core :as ig]
            [rest-server.boundary.db.member :as db.member]))

(defn member-with-id [member]
  (set/rename-keys member {:id :member-id}))

(defmethod ig/init-key ::list [_ {:keys [db]}]
  (fn [_] [::response/ok (map member-with-id
                              (db.member/list-members db))]))

(defmethod ig/init-key ::create [_ {:keys [db]}]
  (fn [{[_ member] :ataraxy/result}]
    (let [id (db.member/create-member db member)]
      [::response/ok (-> member
                         (assoc :id id)
                         member-with-id)])))

(defmethod ig/init-key ::fetch [_ {:keys [db]}]
  (fn [{[_ member-id] :ataraxy/result}]
    (when-let [member (db.member/fetch-member db member-id)]
      [::response/ok (member-with-id member)])))
