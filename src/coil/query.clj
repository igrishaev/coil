(ns coil.query
  (:require
   [clojure.string :as str])
  (:import
   java.net.URLEncoder))


(defn params->pairs [params]
  (reduce-kv
   (fn [result k v]
     (if (sequential? v)
       (into result (for [v v]
                      [k v]))
       (conj result [k v])))
   []
   params))


(defn encode-pair [[k v]]
  (let [encoding "UTF-8"]
    (str
     (URLEncoder/encode (name k) encoding)
     "="
     (URLEncoder/encode (str v) encoding))))


(defn make-query-string [params]
  (->> params
       params->pairs
       (map encode-pair)
       (str/join "&")))
