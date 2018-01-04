globals [ rootnd                      ;; The root node
          nodes-list lf-nodes-list    ;; The node-list containing all the nodes and the node-list containg all the leaf nodes of DetectTree
          num-nodes                  ;; Total number of nodes
          tree-height                ;; The height of the tree
          num-gen-trgs num-msgs      ;; The num of generated trgs and the num of exchanges msgs
          tree-display-ratio          ;; The width-ratio for displaying DetectTree. In the remaining resion, the other nodes not contained in DetectTree are displayed
          max-rcvd                    ;; The maximum number of received msgs among all the nodes.
          last-coin-gen
          cur-inrnd-lvl
        ]

breed [roots root] ;; root node breed
breed [inr-nodes inr-node] ;; inner node breed of the tree
breed [nodes node]           
breed [trgs trg]               ;; trigger breed

roots-own     [num-trgs num-rcvd tau coin-received num-detected-trgs cur-round]
inr-nodes-own [num-trgs num-rcvd tau coin-received]
nodes-own     [num-trgs num-rcvd tau]

;; reporters for breeds -------------------------------------------------------------------------------------------------
to-report ntrg ;; procedure for root, dtinr-nodes, dtlf-nodes, nodes
  ifelse nodes = [breed] of self [
    report num-trgs
  ][
    report num-trgs + coin-received * tau
  ]
end
to-report is-round-end ;; procedure for root
  ifelse coin-received = 1
  [ report 1 ]
  [ report 0 ]
end
to-report is-about-to-gen-coin
  ifelse num-trgs >= tau
  [report 1]
  [report 0]  
end
to-report rnd ;; procedure for root
  report cur-round
end
to-report total-detected-trgs ;; procedure for root
  report num-detected-trgs
end
to-report level
  report floor(log (who + 1) 2)
end
to-report rand-upper-level-node
  let lvl [level] of self
  let id-offset random (2 ^ (lvl - 1))
  let rand-upper 2 ^ (lvl - 1) - 1 + id-offset
  report turtle rand-upper
end
to-report tau-val
  report tau
end
to-report left-child
  report turtle (who * 2 + 1)
end
to-report right-child
  report turtle (who * 2 + 2)
end  



;; inc-num-msgs ---------------------------------------------------------------------------------------------------------
to inc-num-msgs [num]
  set num-msgs num-msgs + num
end
  


;; setup ----------------------------------------------------------------------------------------------------------------
to setup
  clear-all
  set num-nodes 2 ^ L
  set tree-height L - 1
  set tree-display-ratio 0.85
  
  ;; set default shapes of the turtles in this simulation
  set-default-shape roots "circle"
  set-default-shape inr-nodes "circle"
  set-default-shape nodes "circle"
  set-default-shape trgs "star"
  
  ;; set initial values for the global variables
  set num-gen-trgs 0
  set num-msgs 0
  set max-rcvd 0
  set nodes-list []
  set lf-nodes-list [] ;; root is initialized after gen-node-tree
  
  ;; generate DetectTree
  gen-node-tree
  ;; generate other nodes
  gen-other-nodes
    
  set rootnd root 0  ;; For ease of coding, set 'root' variable as the root node of DetectTree
  reset-ticks  
  prepare-round 1        ;; prepare the first round
  set last-coin-gen ticks - 1
  
  file-open "CoinRnd_rst.txt"
end

;; The procedures related to gen-node-tree -------------------------------------------------------------------------------
to gen-node-tree
  let gen-height  1
  let yoffset     world-height / (tree-height + 2) ;; the reason for '+ 2' is the root is on height-0 and the num of vertical spaces is h + 2.
  let xoffset     world-width * tree-display-ratio / 2
  let num-nodes-gen-height 0
  let xord        0
  
  ;; create the root node
  create-roots 1 [ init-root ]
  let new-node turtle (count turtles - 1)
  ask new-node [locate-tree-node 1 1 xoffset yoffset]
  ask patch ([pxcor] of new-node) ([pycor] of new-node + 8) [ set plabel "Internal layers" ]
  set nodes-list lput new-node nodes-list
  
  while [ gen-height <= tree-height ]
  [
    set num-nodes-gen-height (2 ^ gen-height)
    set xord 1
    set xoffset world-width * tree-display-ratio / (num-nodes-gen-height + 1)
        
    while [ xord <= num-nodes-gen-height ]
    [
      create-inr-nodes 1 [ init-inner-node ]
      
      set new-node turtle (count turtles - 1)
      ask new-node [locate-tree-node xord (gen-height + 1) xoffset yoffset]
      
      set nodes-list lput new-node nodes-list
      if gen-height = tree-height [
        set lf-nodes-list lput new-node lf-nodes-list
      ]
      set xord xord + 1
    ]
    set gen-height (gen-height + 1)
  ]
end

to gen-other-nodes
  let oth-nodes-width world-width * (1 - tree-display-ratio)
  let xoffset oth-nodes-width / 2
  let yoffset world-height / 2
  
  create-nodes 1 [init-node]
  let new-node turtle (count turtles - 1)
  ask new-node [ setxy ( max-pxcor - oth-nodes-width + xoffset) (max-pycor - yoffset) ]
  set nodes-list lput new-node nodes-list
  
  ask patch (max-pxcor - oth-nodes-width + xoffset) (max-pycor - yoffset + 8)
  [ set plabel "The node not contained in internal layers" ]
end

to init-root ;; the procedure for the root node and the inner nodes of DetectTree
  set color green
  set num-trgs 0
  set num-rcvd 0
  set coin-received 0
  set num-detected-trgs 0
  set cur-round 1
end

to init-inner-node ;; the procedure for the root node and the inner nodes of DetectTree
  set color green
  set num-trgs 0
  set num-rcvd 0
  set coin-received 0
end

to init-node
  set color green
  set num-trgs 0
  set num-rcvd 0
end

to locate-tree-node [xord yord xoffset yoffset]
  setxy (min-pxcor + xord * xoffset) (max-pycor - yoffset * yord )
end
;; End of the procedures related to gen-node-tree -------------------------------------------------------------------------------



;; The procedures related to prepare-round --------------------------------------------------------------------------------------
to prepare-round [round-num]  
  let num-remaining-trgs (num-triggers - [total-detected-trgs] of rootnd)
  let new-t floor( num-remaining-trgs / (4 * num-nodes) )
  if new-t < 1 [ set new-t 1 ]
  
  if ([rnd] of rootnd = 1) or ([tau-val] of rootnd > 1) [  
    let que1 []
    let que2 []
    let lvl 0
    set que1 lput rootnd que1
    ask rootnd [set-new-tau new-t]
    
    while [(length que1) > 0] [
      foreach que1 [
        ask ? [set-new-tau new-t]
        if lvl < tree-height [
          set que2 lput ([left-child] of ?) que2
          set que2 lput ([right-child] of ?) que2
          inc-num-msgs 2                    
        ]
      ]
      set lvl lvl + 1
      set que1 que2
      set que2 []
      if lvl < tree-height [ tick ]     
    ]
  ]  
  if tree-height = (L - 1) [
    ask turtle (2 ^ L - 1) [set-new-tau new-t]
    inc-num-msgs 1
  ]
  tick
    
  if num-remaining-trgs < num-nodes [
    let k 1
    ifelse num-remaining-trgs = 1 [
      set k L
    ][    
      while [ (num-nodes / (2 ^ k)) > num-remaining-trgs ]
      [
        set k k + 1
      ]
    ]
    
    set lf-nodes-list []
    let lvl L - k
    set tree-height lvl
    let first-id 2 ^ (lvl - 1) - 1
    if first-id < 0 [ set first-id 0 ]
    let last-id 2 ^ lvl
    let j first-id
    while [ j < last-id ]
    [
      set lf-nodes-list lput (turtle j) lf-nodes-list
      set j j + 1
    ]    
  ]
    
  print (word "round " round-num ": trgs(" num-remaining-trgs "), t(" new-t ")")
end

to set-new-tau [new-t]
  set tau new-t
end

to reset-node
  if roots = [breed] of self [
    ask self [ reset-inner-node ]    
  ]
  if inr-nodes = [breed] of self [
    ask self [ reset-inner-node ]
  ]  
  if nodes = [breed] of self [
    ask self [ reset-other-node ]
  ]  
end

to reset-inner-node 
  set num-trgs 0
  set coin-received 0
end

to reset-other-node
  set num-trgs 0
end

to gather-trgs
  let que1 []
  let que2 []
  let lvl 0
  let gathered 0
  set que1 lput rootnd que1
  while [(length que1) > 0] [
    foreach que1 [
      set gathered gathered + ([ntrg] of ?)
      ask ? [reset-node]
      if lvl < tree-height [
        set que2 lput ([left-child] of ?) que2
        set que2 lput ([right-child] of ?) que2
        inc-num-msgs 4        
      ]
    ]
    set lvl lvl + 1
    set que1 que2
    set que2 []
    tick
  ]
  if tree-height = (L - 1) [
    let othnd turtle (2 ^ L - 1)
    set gathered gathered + [ntrg] of othnd
    ask othnd [reset-node]
    inc-num-msgs 2
  ]
  ;print (word "num gen: " num-gen-trgs ", gathered: " gathered)
  ;print (word "gathering triggers: " [total-detected-trgs] of rootnd ", " gathered)
  ask rootnd [ set num-detected-trgs num-detected-trgs + gathered ]
  ;print (word "gathering triggers: " [total-detected-trgs] of rootnd )  
end
;; End of the procedures related to prepare-round ----------------------------------------------------------------------------------

  
;; go ------------------------------------------------------------------------------------------------------------------------------
to go
  ;; asking trgs to disapper. at each tick, one trigger can be generated and the old trigger should disapper.
  ask trgs [die]
  
  ;; generating new trigger
  let notify-new-round [is-round-end] of rootnd
  if (notify-new-round = 0) and (num-gen-trgs < num-triggers) [
    gen-trigger
  ]

  if (ticks - last-coin-gen) >= coin-gen-frequency [
    foreach nodes-list [ ask ? [ gen-coin ] ]
  ]
    
  ;; if the current round is ended, prepare the new round
  set notify-new-round [is-round-end] of rootnd
  if notify-new-round = 1 [
    let new-round ([rnd] of rootnd + 1)    
    if new-round > 1 [
      gather-trgs
    ]    
    ifelse ([total-detected-trgs] of rootnd >= num-triggers) [
      print (word "MaxRcvd: " max-rcvd " log w/n: " (log (num-triggers / num-nodes) 2))
      print "end of the simulation"
      file-write (word ([rnd] of rootnd) " " num-msgs " " max-rcvd)
      file-close
      stop            
    ][
      ask rootnd [ set cur-round cur-round + 1 ]    
      prepare-round [rnd] of rootnd
    ]
  ]
  tick
end

to gen-trigger
  let a-node one-of nodes-list
  create-trgs 1 [
    set color red
    set size 6
    setxy [xcor] of a-node [ycor] of a-node
  ]  
  ask a-node [ set num-trgs num-trgs + 1 ]
  set num-gen-trgs num-gen-trgs + 1
end

to gen-coin ;; node procedure.
  if num-trgs >= tau [
    set num-trgs num-trgs - tau
    ;; send 'detect' msg to one of the leaf dnodes uniformly at random
    ;print (word "call proc-detect at " who)
    inc-num-msgs 1
    ask one-of lf-nodes-list [      
      proc-coin
    ]
  ]
end

to proc-coin ;; dtlf-node procedure
  ;print (word "proc-detect at " who)
  set num-rcvd num-rcvd + 1
  if num-rcvd > max-rcvd [ set max-rcvd num-rcvd ]
  
  ifelse coin-received = 0 [
    set coin-received 1
  ][
    ifelse (not (who = 0)) [
      forward-coin-to-rand-upper-level-node
    ][
      set num-trgs num-trgs + tau
    ]
  ]
end

to forward-coin-to-rand-upper-level-node  
  ;let rupnd [rand-upper-level-node] of self
  ;print (word who ", " rupnd)
  ;ask rupnd [ inrproc-detect [who] of myself ]
  
  ask [rand-upper-level-node] of self [ proc-coin ]
end
@#$#@#$#@
GRAPHICS-WINDOW
10
10
1022
283
250
60
2.0
1
10
1
1
1
0
1
1
1
-250
250
-60
60
1
1
1
ticks
30.0

BUTTON
15
345
83
379
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
90
345
154
379
NIL
go\n
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
17
487
217
520
L
L
1
13
7
1
1
NIL
HORIZONTAL

SLIDER
18
409
281
442
num-triggers
num-triggers
1000
80000
80000
1000
1
NIL
HORIZONTAL

MONITOR
380
348
485
397
NIL
num-gen-trgs
17
1
12

PLOT
573
346
987
556
The total num of msgs
ticks
msgs
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"msgs" 1.0 0 -13840069 true "" "plot num-msgs"

MONITOR
381
483
468
532
num-msgs
num-msgs
17
1
12

MONITOR
231
486
321
535
NIL
num-nodes
17
1
12

TEXTBOX
22
458
304
477
num-nodes = 2 ^ L
12
75.0
1

MONITOR
381
421
516
470
num-detected-trgs
[total-detected-trgs] of rootnd
17
1
12

MONITOR
384
542
442
591
round
[rnd] of rootnd
17
1
12

SLIDER
19
538
199
571
coin-gen-frequency
coin-gen-frequency
1
100
1
1
1
NIL
HORIZONTAL

MONITOR
452
544
529
593
NIL
max-rcvd
17
1
12

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

bird
false
0
Polygon -7500403 true true 135 165 90 270 120 300 180 300 210 270 165 165
Rectangle -7500403 true true 120 105 180 237
Polygon -7500403 true true 135 105 120 75 105 45 121 6 167 8 207 25 257 46 180 75 165 105
Circle -16777216 true false 128 21 42
Polygon -7500403 true true 163 116 194 92 212 86 230 86 250 90 265 98 279 111 290 126 296 143 298 158 298 166 296 183 286 204 272 219 259 227 235 240 241 223 250 207 251 192 245 180 232 168 216 162 200 162 186 166 175 173 171 180
Polygon -7500403 true true 137 116 106 92 88 86 70 86 50 90 35 98 21 111 10 126 4 143 2 158 2 166 4 183 14 204 28 219 41 227 65 240 59 223 50 207 49 192 55 180 68 168 84 162 100 162 114 166 125 173 129 180

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
