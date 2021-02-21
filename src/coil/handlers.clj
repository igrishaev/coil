(ns coil.handlers
  (:import
   java.net.http.HttpResponse$BodyHandlers))


(defn of-byte-array []
  (HttpResponse$BodyHandlers/ofByteArray))


(defn of-input-stream []
  (HttpResponse$BodyHandlers/ofInputStream))


(defn of-lines []
  (HttpResponse$BodyHandlers/ofLines))


(defn of-string
  ([]
   (HttpResponse$BodyHandlers/ofString))
  ([charset]
   (HttpResponse$BodyHandlers/ofString charset)))


(defn discarding []
  (HttpResponse$BodyHandlers/discarding))
