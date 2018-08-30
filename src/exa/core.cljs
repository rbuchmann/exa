(ns exa.core
  (:require
   [reagent.core :as reagent]
   [exa.assembly :as a]
   [cljs.reader :as edn]
   [fipp.clojure :refer [pprint] :rename {pprint fipp}]))

;; Vars

(defonce app-state
  (reagent/atom {:input      ""
                 :transpiled ""}))

;; Helper

(defn transpile-exacode-string [s]
  (try (-> s edn/read-string a/eval-exacode)
       (catch js/Error e
         (str "Error parsing: " s ", Error was: " e))))

(defn pretty-print [s]
  (try
    (-> s edn/read-string fipp with-out-str)
    (catch js/Error e
      (.log js/console "Something happened while pretty printing: " e)
      s)))


;; Components


(defn editor [state]
  [:textarea.form-control
   {:style        {:min-height "800px"}
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
  [:pre
   {:style {:min-height "800px"}}
   (:transpiled @state)])

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
      [transpile-display state]]]]])

;; Initialize App

(defn dev-setup []
  (when ^boolean js/goog.DEBUG
    (enable-console-print!)
    (println "dev mode")
    ))

(defn reload []
  (reagent/render [page app-state]
                  (.getElementById js/document "app")))

(defn ^:export main []
  (dev-setup)
  (reload))
