(ns rest-server.boundary.meetup
  (:require [clojure.java.jdbc :as jdbc]
            [duct.database.sql]
            [honeysql.core :as sql]
            [rest-server.util :as util]))

(defprotocol Meetups
  (list-meetups [db group-id])
  (create-meetup [db meetup])
  (fetch-meetup [db meetup-id])
  (fetch-meetup-members [db meetup-id])
  (create-meetup-member [db meetup-member]))

(extend-protocol Meetups
  duct.database.sql.Boundary
  (list-meetups [{db :spec} group-id]
    (->> (sql/build :select :*
                    :from :meetups
                    :where [:= :group_id group-id])
         sql/format
         (jdbc/query db)
         util/transform-keys-to-kebab))
  (create-meetup [{db :spec} meetup]
    (->> meetup
         util/transform-keys-to-snake
         (jdbc/insert! db :meetups)
         ffirst
         val))
  (fetch-meetup [{db :spec} meetup-id]
    (->> (sql/build :select :*
                    :from :meetups
                    :where [:= :id meetup-id])
         sql/format
         (jdbc/query db)
         util/transform-keys-to-kebab
         first))
  (fetch-meetup-members [{db :spec} meetup-id]
    (->> (sql/build :select :members.*
                    :from :members
                    :join [:meetups_members [:= :members.id :meetups_members.member_id]]
                    :where [:= :meetups_members.meetup_id meetup-id])
         sql/format
         (jdbc/query db)
         util/transform-keys-to-kebab))
  (create-meetup-member [{db :spec} meetup-member]
    (->> meetup-member
         util/transform-keys-to-snake
         (jdbc/insert! db :meetups_members)
         ffirst
         val)))
