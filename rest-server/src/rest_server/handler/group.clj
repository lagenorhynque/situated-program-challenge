(ns rest-server.handler.group
  (:require [ataraxy.response :as response]
            [integrant.core :as ig]
            [rest-server.boundary.db.group :as db.group]
            [rest-server.boundary.db.meetup :as db.meetup]
            [rest-server.boundary.db.member :as db.member]
            [rest-server.boundary.db.venue :as db.venue]
            [rest-server.handler.meetup :as meetup]
            [rest-server.handler.member :as member]
            [rest-server.handler.venue :as venue]
            [rest-server.util :as util]))

(defn group-with-admin [{:keys [id name]} admins]
  {:group-id id
   :group-name name
   :admin (map member/member-with-id admins)})

(defn fetch-group-detail [db {:keys [id] :as group}]
  (let [admins (db.group/fetch-group-admin-members db id)
        venues (db.venue/list-venues db id)
        meetups (db.meetup/list-meetups db id)]
    (assoc (group-with-admin group admins)
           :venues (map venue/venue-with-address venues)
           :meetups (map (partial meetup/fetch-meetup-detail db) meetups))))

(defmethod ig/init-key ::list [_ {:keys [db]}]
  (fn [_] [::response/ok (map (partial fetch-group-detail db)
                              (db.group/list-groups db))]))

(defmethod ig/init-key ::create [_ {:keys [db]}]
  (fn [{[_ {:keys [group-name admin-member-ids]}] :ataraxy/result}]
    (let [group {:name group-name
                 :created-at (util/now)}
          id (db.group/create-group db group)
          admins (db.member/fetch-members db admin-member-ids)]
      (db.group/create-group-members db (map (fn [member-id]
                                               {:group-id id
                                                :member-id member-id
                                                :admin true})
                                             admin-member-ids))
      [::response/ok (-> group
                         (assoc :id id)
                         (group-with-admin admins))])))

(defmethod ig/init-key ::join [_ {:keys [db]}]
  (fn [{[_ member-id group-id {:keys [admin]}] :ataraxy/result}]
    (db.group/create-group-members db [{:group-id group-id
                                        :member-id member-id
                                        :admin admin}])
    (when-let [group (db.group/fetch-group db group-id)]
      [::response/ok (fetch-group-detail db group)])))
