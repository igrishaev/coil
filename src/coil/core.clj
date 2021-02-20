(ns coil.core
  (:import
   java.net.http.HttpClient
   java.net.http.HttpClient$Version

   java.net.http.HttpClient$Redirect
   java.net.http.HttpClient$Version

   java.net.http.HttpRequest
   java.net.http.HttpRequest$Builder
   java.net.http.HttpRequest$BodyPublisher
   java.net.http.HttpRequest$BodyPublishers

   java.net.http.HttpResponse
   java.net.http.HttpResponse$BodyHandlers


   java.net.http.HttpHeaders

   java.time.Duration

   java.io.File
   java.io.InputStream

   java.util.function.Supplier

   java.nio.file.Paths

   java.net.URI



   )
  (:require
   [clojure.string :as str])
  (:gen-class))


(defprotocol IClojure
  (->clj [value]))


(extend-protocol IClojure

  HttpHeaders
  (->clj [headers]
    (reduce
     (fn [result [header values]]
       (assoc result (keyword header) (first values)))
     {}
     (.map headers)))

  HttpResponse
  (->clj [response]
    {:status (.statusCode response)
     :headers (-> response .headers ->clj)
     :url (-> response .uri str)
     :version (-> response .version ->clj)
     :body (.body response)})

  HttpClient$Version
  (->clj [version]
    (case (str version)
      "HTTP_1_1" :http-1.1
      "HTTP_2"   :http-2)))

(defn ->redirect [kword]
  (case kword
    :always
    HttpClient$Redirect/ALWAYS
    :never
    HttpClient$Redirect/NEVER
    :normal
    HttpClient$Redirect/NORMAL))

(defn ->version [kword]
  (case kword
    :http-1.1
    HttpClient$Version/HTTP_1_1
    :http-2
    HttpClient$Version/HTTP_2))


(defn ^HttpClient make-client

  [{:as opt
    :keys [version
           redirect]}]

  (cond-> (HttpClient/newBuilder)

    redirect
    (.followRedirects (->redirect redirect))

    version
    (.version (->version redirect))

    true
    .build)




  ;; version
  ;;
  ;; connectTimeout
  ;; proxy
  ;; authenticator
  ;;
  )


(defn ^java.net.http.HttpRequest$BodyPublisher
  make-body-publisher [body]
  (cond

    ;; offset length
    (bytes? body)
    (HttpRequest$BodyPublishers/ofByteArray body)

    ;; (file? body)
    ;; (HttpRequest$BodyPublishers/ofFile body) ;; wrap file

    (in-stream? body)
    (HttpRequest$BodyPublishers/ofInputStream (->supplier body))

    ;; charset
    (string? body)
    (HttpRequest$BodyPublishers/ofString body)

    (nil? body)
    (HttpRequest$BodyPublishers/noBody)

    :else
    (throw (ex-info "wrong body" {:body body}))))


(defn set-method-&-publisher
  [^HttpRequest$Builder builder
   {:keys [method body]}]

  (let [publisher (make-body-publisher body)
        java-method (-> method name str/upper-case)]
    (.method builder java-method publisher)))


(defn set-headers
  [^HttpRequest$Builder builder headers]
  (doseq [[header value] headers]
    (.header builder (name header) (str value))))


(defn set-timeout
  [^HttpRequest$Builder builder timeout]
  (.timeout builder (java.time.Duration/ofMillis timeout)))


(defn set-url [^HttpRequest$Builder builder url]
  (.uri builder (new URI url)))


(defn build-request [^HttpRequest$Builder builder]
  (.build builder))


(defn ^HttpRequest

  make-request

  [{:as opt
    :keys [url
           expect-continue?
           headers
           timeout]}]

  (cond-> (-> (HttpRequest/newBuilder)
              (set-url url)
              (set-method-&-publisher opt))

    headers
    (set-headers headers)

    timeout
    (set-timeout timeout)

    ;; expect-continue?
    ;; (.expectContinue expect-continue?)

    true
    build-request


    ))


(defn ^HttpResponse$BodyHandlers
  make-body-handler
  [{:keys [as]}]

  (case as

    :bytes
    (HttpResponse$BodyHandlers/ofByteArray)

    ;; path,
    ;; open-option...
    ;; https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/StandardOpenOption.html
    ;; :file
    ;; (HttpResponse$BodyHandlers/ofFile "aaa")


    ;; ofFileDownload
    ;; open-option...
    ;; (HttpResponse$BodyHandlers/ofFile path)

    :stream
    (HttpResponse$BodyHandlers/ofInputStream)

    :lines
    (HttpResponse$BodyHandlers/ofLines)

    ;; charset
    :string
    (HttpResponse$BodyHandlers/ofString)

    ;; :none?
    nil
    (HttpResponse$BodyHandlers/discarding)))


(def file? (partial instance? java.io.File))

(def in-stream? (partial instance? InputStream))


(defn ->path [path]
  (Paths/get path))


(defn ^Supplier ->supplier [val]
  (reify Supplier
    (get [this]
      val)))




(def opt-default
  {:method :get
   :as :string})

(defn ^java.util.function.Function as-function [f]
  (reify java.util.function.Function
    (apply [this arg] (f arg))))


(defn request
  [opt]

  (let [opt* (merge opt-default opt)
        c (make-client opt*)
        req (make-request opt*)
        handler (make-body-handler opt*)

        {:keys [async?]} opt*

        ]

    (if async?

      (-> (.sendAsync c req handler)
          (.thenApply (as-function ->clj)))

      (-> (.send c req handler)
          ->clj))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
