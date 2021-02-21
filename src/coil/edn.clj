(ns coil.edn
  (:import
   java.io.PushbackReader
   java.net.http.HttpRequest$BodyPublishers)
  (:require
   [coil.mults :as m]
   [clojure.java.io :as io]
   [clojure.edn :as edn]))


(defmethod m/make-custom-body-publisher :edn
  [{:keys [body]}]
  (HttpRequest$BodyPublishers/ofString
   (pr-str body)))


(defmethod m/handle-content-type :edn
  [opt]
  (assoc-in opt
            [:headers :content-type]
            "application/edn"))


;; check content type
(defmethod m/handle-as :edn
  [response
   {:keys [edn-read-params]}]

  (update response :body
          (fn [stream]
            (->> stream
                 io/reader
                 PushbackReader.
                 (edn/read edn-read-params)))))
