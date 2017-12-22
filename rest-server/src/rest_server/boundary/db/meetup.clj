(ns rest-server.boundary.db.meetup
  (:require [duct.database.sql]
            [honeysql.core :as sql]
            [rest-server.boundary.db.core :as db]))

(defprotocol Meetups
  (list-meetups [db group-id])
  (create-meetup [db meetup])
  (fetch-meetup [db meetup-id])
  (fetch-meetup-members [db meetup-id])
  (create-meetup-member [db meetup-member]))

(extend-protocol Meetups
  duct.database.sql.Boundary
  (list-meetups [db group-id]
    (db/select db (sql/build :select :*
                             :from :meetups
                             :where [:= :group_id group-id])))
  (create-meetup [db meetup]
    (db/insert! db :meetups meetup))
  (fetch-meetup [db meetup-id]
    (db/select-one db (sql/build :select :*
                                 :from :meetups
                                 :where [:= :id meetup-id])))
  (fetch-meetup-members [db meetup-id]
    (db/select db (sql/build :select :members.*
                             :from :members
                             :join [:meetups_members [:= :members.id :meetups_members.member_id]]
                             :where [:= :meetups_members.meetup_id meetup-id])))
  (create-meetup-member [db meetup-member]
    (db/insert! db :meetups_members meetup-member)))
