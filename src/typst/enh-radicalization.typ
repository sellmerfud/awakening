#set page(
  paper: "a4",
  margin: 0.75cm,
  flipped: false, // portrait orientation
  // columns: 2,
)

// Title
#place(
  top + center,
  float: true,
  scope: "parent",
  clearance: 1em,
  box(stroke: none, inset :1em, fill: blue.lighten(80%),
    text(size: 17pt)[
      Enhanced Jihadist Bot Radicalization
    ]
  )
)

// // Definitions
// #place(
//   bottom + left,
//   float: true,
//   scope: "parent",
//   clearance: 1em,
//   box(stroke: none, inset: 0.5em, fill: blue.lighten(80%))[
//     ARP = Auto-Recruit Priority Country \
//     MJP = Major Jihad Priority Country \
//     JSP = Jihad Success Possible
//   ]
// )

#set text(
  font: "Libertinus Serif",
  size: 10pt,
)

#show heading: it => [
  #set text(11pt, weight: "bold")
  #block(smallcaps(it.body))
]

#show terms: set terms(hanging-indent: 0em)

= Introduction
This document explains the _Radicalization_ instructions used by the Enhanced Jihadist Bot.  
These instructions replace the Standard Jihadist Bot instructions listed in Rule *12.4.3 Radicalization* in the _Labyrinth: The Awakening_ rulebook.  Like the Standard Bot, the Enhanced Bot uses *Radicalization* when it performs either no Operation or it performs an Operation using fewer Ops than the cards's Ops Value. It will perform the following actions with the remainin available Ops using Reserves if possible until a total of three cells/rolls have been used.

*Note:* a Cell on the map (not the Track) is eligible to perform a
Radicalization Operation only if it has not already performed an
Operation on the current card (12.4.1, 4th bullet). Likewise, a Cell
may only participate in one Radicalization Operation.

= Radicalization Steps
+ / Plot in US: If a WMD plot is available, plot as many times as possible in the US as long as there are unused cells there.
+ / Adjacent Travel to US: If a WMD plot is available and there are no cells in the US and there is an unused cell adjacent to the US, travel 1 _(max)_ adjacent cell to the US. (Skip if _Biometrics_ is in play)
+ / Minor Jihad: Perform one or more Minor Jihads if possible. (In a Fair country with the _Training Camps_ marker, preserve the last cell in the country unless _Sadr_ is also present.)
+ / Adjacent Travel to Good Muslim countries with no Troops: Travel a single adjacent cell to as many Good countries without troops as possible where a _War of Ideas_ roll would not succeed if the country were worsened to Fair. (Skip if _Biometrics_ is in play)
+ / Recruit: Recruit as many cells as possible in Poor or Auto-Recruit Muslim countries.  Do not recruit in Islamist Rule countries unless the country has fewer than 6 cells and either the country is the _Auto Recruit Priority_ or the _Auto Recruit Priority_  is not Islamist Rule.  Priority to the _Major Jihad Priority_, then using _Recruit/Travel To_ priorities.
+ / Add to reserves: Add surplus card Ops to reserves if there are not already 2 Ops in reserve.
