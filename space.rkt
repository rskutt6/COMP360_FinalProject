#lang comp360_FinalProject
(room airlock
  (desc "You wake up in the airlock of an abandoned space station. Red emergency lights flicker overhead. You have no memory of how you got here.")
  (exit north corridor))

(room corridor
  (desc "A long metal corridor stretches before you. Sparks fly from damaged wiring on the walls. Something moves in the shadows.")
  (item plasma-cutter
    (desc "A heavy plasma cutter. Could be useful as a weapon.")
    (type weapon))
  (monster alien 15)
  (exit south airlock)
  (exit north lab))

(room lab
  (desc "A research lab in chaos. Broken equipment and scattered papers everywhere. Whatever they were experimenting on, it got loose.")
  (item hazmat-suit
    (desc "A reinforced hazmat suit. Offers some protection.")
    (type armor))
  (monster mutant 20)
  (exit south corridor)
  (exit north bridge))

(room bridge
  (desc "The ship's bridge. Through the cracked viewport you can see the stars stretching endlessly. The commander blocks your path to the escape pod. There is no way back.")
  (monster commander 35))