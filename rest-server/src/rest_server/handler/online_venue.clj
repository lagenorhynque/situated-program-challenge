(ns rest-server.handler.online-venue
  (:require [ataraxy.response :as response]
            [integrant.core :as ig]
            [rest-server.boundary.db.venue :as db.venue]))

(defn online-venue-with-id [{:keys [id name url] :as online-venue}]
  (when online-venue
    {:online-venue-id id
     :venue-name name
     :url url}))

(defmethod ig/init-key ::list [_ {:keys [db]}]
  (fn [{[_ group-id] :ataraxy/result}]
    [::response/ok (map online-venue-with-id
                        (db.venue/list-venues db
                                              group-id
                                              :venue-type/online))]))

(defmethod ig/init-key ::create [_ {:keys [db]}]
  (fn [{[_ group-id {:keys [venue-name url]}] :ataraxy/result}]
    (let [venue {:name venue-name
                 :group-id group-id
                 :url url
                 :venue-type :venue-type/online}
          id (db.venue/create-venue db venue)]
      [::response/ok (-> venue
                         (assoc :id id)
                         online-venue-with-id)])))
