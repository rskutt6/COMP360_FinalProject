#lang comp360_FinalProject

(room wake
  (desc "You awake underwater")
  (item rusted-dagger
    (desc "A rusted dagger")
    (type weapon))
  (power 10)
  (exit north maze1)
  (exit east kelp))

(room kelp
  (desc "Tall forests of kelp")
  (item kelp-wrap
    (desc "A strip of kelp")
    (type armor))
  (power 5)
  (exit west wake))
