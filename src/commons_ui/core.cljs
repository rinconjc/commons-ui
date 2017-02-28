(ns commons-ui.core
  (:require [clojure.browser.repl :as repl]
            [reagent.core :as r :refer [atom]]
            [clojure.string :as str]))

(defn- bind [attrs model type]
  (if-let [[doc & path] model]
    (let [{:keys [on-change value-fn] :or {value-fn identity}} attrs]
      (apply assoc (dissoc attrs :model :options :value-fn)
             (if (= type "file")
               [:on-change (fn [e]
                             (swap! doc assoc-in path (-> e .-target .-files (aget 0)))
                             (if (fn? on-change) (on-change e)))]
               [:value (get-in @doc path)
                :on-change (fn [e]
                             (swap! doc assoc-in path (-> e .-target .-value value-fn))
                             (if (fn? on-change) (on-change e)))])))
    attrs))

(defn- to-options [opts children]
  (if (or children opts)
    (concat [^{:key ""}[:option {:value "" :disabled true} "Select"]]
            children
            (map-indexed
             (fn [i [k v]]
               ^{:key i} [:option {:value k} v]) opts))))

(defn- typeahead [{:keys [model choice-fn result-fn data-source] :as attrs
                   :or {choice-fn identity result-fn identity}}]
  (let [[doc & path] model
        typeahead-hidden? (atom true)
        mouse-on-list? (atom false)
        selected-index (atom -1)
        selections (atom [])
        save! #(swap! doc assoc-in path %)
        value-of #(-> % .-target .-value)
        choose-selected #(when (and (not-empty @selections) (> @selected-index -1))
                           (let [choice (nth @selections @selected-index)]
                             (save! (result-fn choice))
                             (choice-fn choice)
                             (if % (reset! typeahead-hidden? true))))]
    (fn [attrs]
      [:span
       [:input.form-control
        (assoc attrs
               :on-focus    #(save! nil)
               :on-blur     #(when-not @mouse-on-list?
                               (reset! typeahead-hidden? true)
                               (reset! selected-index -1))
               :on-change   #(when-let [value (str/trim (value-of %))]
                               (reset! selections (data-source (.toLowerCase value)))
                               (save! (value-of %))
                               (reset! typeahead-hidden? false)
                               (reset! selected-index -1))
               :on-key-down #(do
                               (case (.-which %)
                                 38 (do
                                      (.preventDefault %)
                                      (when-not (= @selected-index 0)
                                        (swap! selected-index dec)
                                        (choose-selected false)))
                                 40 (do
                                      (.preventDefault %)
                                      (when-not (= @selected-index (dec (count @selections)))
                                        (save! (value-of %))
                                        (swap! selected-index inc)
                                        (choose-selected false)))
                                 9  (choose-selected true)
                                 13 (choose-selected true)
                                 27 (do (reset! typeahead-hidden? true)
                                        (reset! selected-index 0))
                                 "default")))]

       [:ul {:style {:display (if (or (empty? @selections) @typeahead-hidden?) :none :block) }
             :class "typeahead-list"
             :on-mouse-enter #(reset! mouse-on-list? true)
             :on-mouse-leave #(reset! mouse-on-list? false)}
        (doall
         (map-indexed
          (fn [index result]
            [:li {:tab-index     index
                  :key           index
                  :class         (if (= @selected-index index) "highlighted" "typeahead-item")
                  :on-mouse-over #(do
                                    (reset! selected-index (js/parseInt (.getAttribute (.-target %) "tabIndex"))))
                  :on-click      #(do
                                    (reset! typeahead-hidden? true)
                                    (save! (result-fn result))
                                    (choice-fn result))}
             (result-fn result)])
          @selections))]])))

(def focus-wrapper
  (with-meta identity
    {:component-did-mount #(.focus (r/dom-node %))}))

(defn focus-aware [focus? e]
  (if focus?
    [focus-wrapper e]
    e))

(defn bare-input
  [{:keys[model type options] :as attrs} & children]
  (let [attrs (-> attrs (bind model type)
                  (dissoc :validator))
        children (to-options options children)]
    (case type
      "text" [:input.form-control attrs]
      "password" [:input.form-control attrs]
      "select" [:select.form-control  children]
      "textarea" [:textarea.form-control attrs]
      "file" [:input attrs]
      "typeahead" [typeahead (assoc attrs :model model)]
      "radio" [:div.radio
               (doall (for [[k v] (:items attrs)]
                        ^{:key k} [:label.radio-inline
                                   [:input {:type "radio" :value k
                                            :on-change (:on-change attrs)}] v]))]
      [:div {:class type} [:label [:input attrs] (:text attrs)]])))

(defn wrap-validator [v cont]
  (fn [e]
    (let [r (v (-> e .-target .-value))
          r (if (keyword? r) r
                (if r :success :error))]
      (cont r))))

(defn input
  "[input {:type text :model [doc id] }]
  [input {:type \"select\" :options seq :kv-fn}]"
  [{:keys[type label wrapper-class-name label-class-name validator] :as attrs} & children]
  (let [valid-class (atom nil)
        attrs (if validator
                (assoc attrs :on-change
                       (wrap-validator validator
                                       #(reset! valid-class (str "has-" (name %))))) attrs)]
    (fn [{:keys[type label wrapper-class-name label-class-name validator] :as attrs} & children]
      [:div.form-group {:class @valid-class}
       [:label.control-label {:class label-class-name} label]
       (if wrapper-class-name
         [:div {:class wrapper-class-name}
          [focus-aware (:focus attrs) [bare-input attrs children]]]
         [focus-aware (:focus attrs) [bare-input attrs children]])])))

(defn close-button [close-fn]
  [:button.btn {:on-click #(do (close-fn)
                               (doto % .stopPropagation .preventDefault))
                :class "close"}
   [:span "×"]])

(defn progress-overlay []
  [:div {:style {:position "absolute" :width "100%" :height "100%" :z-index 100
                 :top 0 :left 0 :background "rgba(255,255,255,0.5)"
                 :text-align "center"}}
   [:i.fa.fa-spinner.fa-spin.fa-3x {:style {:margin-top "10%"}}]])

(defn alert-box [{:keys [type fade-after] :or {type "danger"}} text show?]
  (when @show?
    (if fade-after (js/setTimeout #(reset! show? false) (* 1000 fade-after)))
    [:div.col-md-3.text-center {:style {:z-index 101}}
     [:div.alert.alert-dismissible {:class (str "alert-" type)}
      [:button.close {:on-click #(reset! show? nil) :aria-label "Close"}
       [:span {:aria-hidden true} "×"]] text]]))

(defn alert [attrs text]
  (let [show? (atom true)]
    [alert-box attrs text show?]))

(defn tooltip [pos text]
  [:div.tooltip {:role "tooltip" :class (name pos)}
   [:div.tooltip-arrow]
   [:div.tooltip-inner text]])
