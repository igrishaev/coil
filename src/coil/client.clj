(ns coil.client
  (:import
   java.net.http.HttpClient
   java.net.http.HttpClient$Redirect
   java.net.http.HttpClient$Version
   ))


(def opt-default
  {:method :get
   :redirect :normal
   :version :http-1.1
   ;;

   })


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

  [opt]

  (let [opt (merge opt-default opt)

        {:keys [version
                redirect]} opt]

    (cond-> (HttpClient/newBuilder)

      redirect
      (.followRedirects (->redirect redirect))

      version
      (.version (->version version))

      true
      .build))

  ;;
  ;; connectTimeout
  ;; proxy
  ;; authenticator
  ;; cookieHandler
  ;; executor
  ;; priority
  ;; sslContext
  ;; sslParameters
  ;;

  )
