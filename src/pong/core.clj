(ns pong.core
  (:require [quil.core :as q])
  (:import (java.awt.event KeyEvent)))

; screen settings
(def screen-width 450)
(def screen-height 250)
(def horizontal-padding 10)

; paddle settings
(def paddle-width 10)
(def paddle-height 70)
(def paddle-speed 15)

(defn make-paddle [x y]
  {:x x :y y :w paddle-width :h paddle-height})

(def player-paddle (atom (make-paddle horizontal-padding 65)))

(def ai-paddle (atom (make-paddle (- screen-width horizontal-padding paddle-width)
                                  65)))

(def ball-dir (atom [2 0]))
(def ball (atom {:x 215 :y 105 :w 10 :h 10}))

(defn hit-factor [p b]
  (- (/ (- (:y b) (:y p))
        (:h p))
     0.5))

(defn move-ball [ball [dx dy]]
  (assoc ball :x (+ (:x ball) dx)
              :y (+ (:y ball) dy)))

(defn draw-rec [r]
  (q/rect (:x r) (:y r) (:w r) (:h r)))

(defn move-up [y]
  (let [next-y (- y paddle-speed)]
    (if (>= next-y 0)
      next-y
      y)))

(defn move-down [y]
  (let [next-y (+ y paddle-speed)]
    (if (<= next-y (- screen-height paddle-height))
      next-y
      y)))

(defn key-pressed []
  (cond
    (= (q/key-code) KeyEvent/VK_UP)
      (swap! player-paddle update-in [:y] move-up)
    (= (q/key-code) KeyEvent/VK_DOWN)
      (swap! player-paddle update-in [:y] move-down)))

(defn is-left-of? [a b]
  (< (+ (:x a) (:w a))
     (:x b)))

(defn is-right-of? [a b]
  (is-left-of? b a))

(defn is-above? [a b]
  (< (+ (:y a) (:h a))
     (:y b)))

(defn is-below? [a b]
  (is-above? b a))

(defn rect-intersects [p b]
  (not (or (is-left-of? b p)
           (is-right-of? b p)
           (is-above? b p)
           (is-below? b p))))

(defn update-game []
  (swap! ball move-ball @ball-dir)

  ; ball hit top or bottom border?
  (when (or (< (:y @ball) 0)
            (> (:y @ball) screen-height))
    (swap! ball-dir (fn [[x y]] [x (- y)])))

  ; ball hits player paddle
  (when (rect-intersects @player-paddle @ball)
    (let [t (hit-factor @player-paddle @ball)]
      ; invert the x direction, set y direction to hit factor
      (swap! ball-dir (fn [[x _]] [(- x) t]))))

  ; ball hits AI paddle
  (when (rect-intersects @ai-paddle @ball)
    (let [t (hit-factor @ai-paddle @ball)]
      ; invert the x direction, set y direction to hit factor
      (swap! ball-dir (fn [[x _]] [(- x) t])))))

(defn draw []
  (q/background 0)
  (q/fill 0xff)
  (draw-rec @player-paddle)
  (draw-rec @ai-paddle)
  (draw-rec @ball))

(defn run []
  (update-game)
  (draw))

(defn setup []
  (q/frame-rate 60))

(q/defsketch example
  :title "pong.clj"
  :size [screen-width screen-height]
  :settings #(q/smooth 2)
  :setup setup
  :draw run
  :key-pressed key-pressed)

(defn -main []
  (println "Running..."))