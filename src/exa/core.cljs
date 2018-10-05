(ns exa.core
  (:require
   [reagent.core :as reagent]
   [exa.assembly :as a]
   [exa.csp :as csp]
   [cljs.reader :as edn]
   [zprint.core :refer [zprint] :as zp]
))

;; Vars

(defonce app-state
  (reagent/atom {:input      ""
                 :transpiled ""}))

(zp/set-options! {:fn-map {"if" :arg1-force-nl
                           "||" :flow}})

;; Helper

(defn transpile-exacode-string [s]
  (try (-> s edn/read-string a/eval-exacode)
       (catch js/Error e
         (str "Error parsing: " s ", Error was: " e))))

(defn pretty-print [s]
  (try
    (-> s edn/read-string zprint with-out-str)
    (catch js/Error e
      (.log js/console "Something happened while pretty printing: " e)
      s)))

(defn download [filename content & [mime-type]]
  (let [mime-type (or mime-type (str "text/plain;charset=" (.-characterSet js/document)))
        blob (new js/Blob
                  (clj->js [content])
                  (clj->js {:type mime-type}))
        anchor (.createElement js/document "a")]
    (set! (.-download anchor) filename)
    (set! (.-href anchor) (js/window.URL.createObjectURL blob))
    (set! (.-target anchor) "_blank")
    (set! (-> anchor .-style .-display) "none")
    (js/document.body.appendChild anchor)
    (.click anchor)
    (js/document.body.removeChild anchor)))

;; Components


(defn editor [state]
  [:textarea.form-control
   {:style        {:min-height  "800px"
                   :font-family :monospace}
    :value        (:input @state)
    :on-change    (fn [event]
                    (let [new-value (-> event .-target .-value)]
                      (swap! state assoc
                             :input new-value
                             :transpiled (transpile-exacode-string new-value))))
    :on-key-press (fn [event]
                    (when (and (.-shiftKey event)
                               (= (.-key event) "Enter"))
                      (.log js/console "been here")
                      (.preventDefault event)
                      (swap! state update :input pretty-print)))}])

(defn transpile-display [state]
  [:textarea.form-control
   {:style     {:min-height  "800px"
                :white-space :pre-wrap}
    :read-only true
    :id        "exacode"
    :value     (:transpiled @state)}])

;; Page

(defn page [state]
  [:div
   [:h1 "Exapunks livecoding!"]
   [:div.container-fluid
    [:div.row
     [:div.col-sm-6
      [:h3 "Editor"]
      [editor state]]
     [:div.col-sm-6
      [:h3 "Transpiled"]
      [transpile-display state]]]
    [:div.row
     [:div.col-sm-12
      [:button.btn.btn-primary
       {:type     "button"
        :on-click (fn []
                    (let [code (.getElementById js/document "exacode")]
                      (.select code)
                      (.execCommand js/document "copy")))}
       "Copy code to clipboard"]
      [:button.btn.btn-primary
       {:type     "button"
        :on-click (fn []
                    (download "solution.txt" (:input @state)))}
       "Download code"]]]]])

;; Initialize App

(defn dev-setup []
  (when ^boolean js/goog.DEBUG
    (enable-console-print!)
    (println "dev mode")))

(defn reload []
  (reagent/render [page app-state]
                  (.getElementById js/document "app")))

(defn ^:export main []
  (dev-setup)
  (reload))
