(ns rest-server.boundary.db.venue
  (:require [duct.database.sql]
            [honeysql.core :as sql]
            [rest-server.boundary.db.core :as db]
            [rest-server.util :as util]))

(defprotocol Venues
  (list-venues [db group-id venue-type])
  (create-venue [db venue])
  (fetch-venue [db venue-id]))

(extend-protocol Venues
  duct.database.sql.Boundary
  (list-venues [db group-id venue-type]
    (db/select db (sql/build :select :*
                             :from :venues
                             :where [:and
                                     [:= :group_id group-id]
                                     [:= :venue_type (util/kw->pgenum venue-type)]])))
  (create-venue [db venue]
    (db/insert! db :venues (update venue :venue-type util/kw->pgenum)))
  (fetch-venue [db venue-id]
    (db/select-one db (sql/build :select :*
                                 :from :venues
                                 :where [:= :id venue-id]))))
