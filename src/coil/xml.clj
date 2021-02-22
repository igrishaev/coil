(ns coil.xml
  (:require
   [coil.mults :as m]
   [coil.util :as u]
   [coil.publishers :as pub]

   [clojure.xml :as xml]
   [clojure.java.io :as io]
   [clojure.data.json :as json]))


(defmethod m/make-custom-body-publisher :xml
  [{:keys [body]}]

  (pub/of-string
   (xml/emit body)))


(defmethod m/handle-content-type :xml
  [opt]
  (assoc-in opt
            [:headers :content-type]
            "text/xml"))

(defn resp-xml? [response]
  (u/content-type-matches?
   response
   #"(?i)(application|text)/xml"))


(defmethod m/handle-as :xml
  [response
   _]

  (if-not (resp-xml? response)
    response

    (update response :body
            (fn [stream]
              (xml/parse stream)))))
