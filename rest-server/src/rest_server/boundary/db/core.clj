(ns rest-server.boundary.db.core
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.core :as sql]
            [rest-server.util :as util]))

(defn select [{db :spec} sql-map]
  (->> sql-map
       sql/format
       (jdbc/query db)
       util/transform-keys-to-kebab))

(defn select-one [{db :spec} sql-map]
  (->> sql-map
       sql/format
       (jdbc/query db)
       util/transform-keys-to-kebab
       first))

(defn insert! [{db :spec} table row-map & {:keys [id-col]
                                           :or {id-col :id}}]
  (->> row-map
       util/transform-keys-to-snake
       (jdbc/insert! db table)
       first
       id-col))

(defn insert-multi! [{db :spec} table row-maps]
  (->> row-maps
       util/transform-keys-to-snake
       (jdbc/insert-multi! db table)))
