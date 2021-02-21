(ns coil.reader
  (:import
   java.net.http.HttpRequest$BodyPublishers)
  (:require
   [coil.mults :as m]
   [coil.util :as u]
   [clojure.java.io :as io]))


(defmethod m/make-custom-body-publisher :reader
  [{:keys [body]}]
  (HttpRequest$BodyPublishers/ofInputStream
   (u/->supplier body)))


(defmethod m/handle-as :reader
  [response
   {:keys [reader-params]}]

  (update response :body
          (fn [stream]
            (apply io/reader
                   stream
                   (u/map->seq reader-params)))))
