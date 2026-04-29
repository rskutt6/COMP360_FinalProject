#lang racket

(require brag/support
         "lexer.rkt"
         "parser.rkt")

;; FULL DUNGEON SCRIPT
(define str
  "(room wake
     (desc \"You awake underwater, lungs full of water and yet somehow not drowning. The water is cold and silent and you can see the pale light filtering through cracks above. Something is wrong, and you need to find your way back to the surface.\")
     (item rusted-dagger
       (desc \"A rusted dagger, worn but still sharp enough to harm anything that approaches.\")
       (type weapon))
     (power 10)
     (exit north maze1)
     (exit east kelp))

   (room kelp
     (desc \"Tall forests of kelp block your vision, swaying and tickling your skin. A shudder runs down you as you turn your head and see eyes peering through the green.\")
     (item kelp-wrap
       (desc \"A long strip of kelp, surprisingly strong.\")
       (type armor))
     (power 5)
     (exit west wake)
     (exit north maze2))

   (room maze1
     (desc \"Flooded stone corridors greet you...\")
     (monster eel 25)
     (exit south wake)
     (exit north maze2)
     (exit east maze3))

   (room maze2
     (desc \"You drift through a chamber...\")
     (item broken-helmet
       (desc \"An old iron helmet...\")
       (type armor))
     (power 3)
     (exit south maze1)
     (exit west kelp)
     (exit east maze4))

   (room maze3
     (desc \"A narrow passage forces you sideways...\")
     (monster crab 15)
     (item coral-blade
       (desc \"A blade formed from coral...\")
       (type weapon))
     (power 20)
     (exit west maze1)
     (exit north maze4))

   (room maze4
     (desc \"The pressure increases here...\")
     (exit west maze2)
     (exit south maze3)
     (exit north selkie-lair))

   (room selkie-lair
     (desc \"A hidden chamber opens suddenly...\")
     (item seal-skin
       (desc \"The selkie's own seal-skin.\")
       (type misc))
     (power 0)
     (exit south maze4)
     (exit north abyss))

   (room abyss
     (desc \"The dungeon falls away into an endless abyss...\")
     (monster deep-thing 50))")

;; input port
(define in (open-input-string str))


(define next-token
  (λ () (dungeon-lexer in)))


(define tree
  (parse next-token))


(pretty-print (syntax->datum tree))
