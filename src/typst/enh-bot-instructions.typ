#set page(
  paper: "a4",
  margin: 0.75cm,
  flipped: false, // portrait orientation
  // columns: 2,
)

#set text(
  font: "Libertinus Serif",
  size: 10pt,
)

#set heading(numbering: "1.1")
#show heading: it => [
  #set text(11pt, weight: "bold")
  #block(
    above: 2em,
    smallcaps(it)
  )
]

#show terms: set terms(hanging-indent: 0em)

#pagebreak(weak: true)
// Radicalization
#place(
  top + center,
  float: true,
  scope: "parent",
  clearance: 1em,
  box(stroke: none, inset :1em, fill: blue.lighten(80%),
    text(size: 17pt)[
      Enhanced Jihadist Bot – Labyrinth: The War on Terror
    ]
  )
)

= Introduction
This document explains the instructions used in implementing the Enhanced Jihadist Bot designed by Florian Ottich.
The Enhanced Bot works similarly to the Standard Bot that was introduced in *Labyrinth: The Awakening*, however the
Enhanced Bot is intended to be implemented in software and would therefore be a bit more complicated to use manually.

= Card Play
When playing a card the Enhanced Bot follows these steps to determine how to use the card:
+ If the event is playable and if playing it has the potential cause an auto victory for the Jihadit, then the Bot will play the event.
+ If the there is Poor Muslim country where it would be possible to conduct a Major Jihad if 3 Ops were available and if doing so would cause an auto victory for the Jihadist:
  - If there are enough Ops on the card plus reserves then the Bot will peform the Major Jihad.
  - If there are not sufficent Ops then the Bot will add the card's Ops to its reserves.
+ If the event is playable and the Bot deems it beneficial, then it will play the event.
+ The Bot will use the Enhanced EvO to determine which operation it will peform.


= Radicalization
These instructions replace the Standard Jihadist Bot instructions for radicalization listed in Rule *12.4.3 Radicalization* in the _Labyrinth: The Awakening_
rulebook.  Like the Standard Bot, the Enhanced Bot uses *Radicalization* when it performs either no Operation or it performs an Operation using fewer Ops than the
cards's Ops Value. It will perform the following actions with the remainin available Ops using Reserves if possible until a total of three cells/rolls have been used.

*Note:* a Cell on the map (not the Track) is eligible to perform a
Radicalization Operation only if it has not already performed an
Operation on the current card (12.4.1, 4th bullet). Likewise, a Cell
may only participate in one Radicalization Operation.

== Radicalization Steps
+ / Plot in US: If a WMD plot is available, plot as many times as possible in the US as long as there are unused cells there.
+ / Adjacent Travel to US: If a WMD plot is available and there are no cells in the US and there is an unused cell adjacent to the US, travel 1 _(max)_ adjacent cell to the US. (Skip if _Biometrics_ is in play)
+ / Plot to increase funding: If Funding < 6
+ / Minor Jihad: Perform one or more Minor Jihads if possible. (In a Fair country with the _Training Camps_ marker, preserve the last cell in the country unless _Sadr_ is also present.)
+ / Adjacent Travel to Good Muslim 2+ resource\* countries with no Troops: Travel a single adjacent cell to as many Good 2+ resource\* countries without troops as possible where a _War of Ideas_ roll would succeed if the country were worsened to Fair. (Skip if _Biometrics_ is in play)
+ / Recruit: Recruit as many cells as possible in Poor or Auto-Recruit Muslim countries.  Do not recruit in Islamist Rule countries unless the country has fewer than 6 cells and either the country is the _Auto Recruit Priority_ or the _Auto Recruit Priority_  is not Islamist Rule.  Priority to the _Major Jihad Priority_, then using _Recruit/Travel To_ priorities.
+ / Travel to _Major Jihad Priority_ country: As many cells a possible.
+ / Add to reserves: Add surplus card Ops to reserves if there are not already 2 Ops in reserve.


= Printed Resource\* Value

When a rule specifies a country's printed resource value followed by an asterisk (\*) this indicates that we are using the resource value as printed on the map and not affected by any current events such as _Oil Price Spike_. (_Exception: The +1 added to Iran's resource value for the Tehran Beirut Land Corridor is still honored._)

= Major Jihad Priority
The Enhanced Bot maintains a target country where it wishes to perform a major jihad.  This country is used when making various
decisions such as where to place cells and reaction markers and where to remove troops/milita and awakening markers.
The following priorities are used to determine the _Major Jihad Priority_ among all muslim countries where a major jihad
could be successfully performed if there were sufficent cell present.
+ Highest Resource\*  _(Only if current Islamist Resources < 4. If Islamit Resources = 1 and the scenario is Let's Roll or You can call me Al, then Pakistan and Central Asia are cosidered to have a printed resource\* value of 3)_
+ Poor/Untested and Printed resource\* > 1 _(Only if current Islamist Resources = 4)_
+ Best jihad DRM
+ Auto-Recruit with no TandM
+ Highest (cells - TandM)
+ Lowest TandM
+ No TandM and highest number of cells present plus moveable adjacent cells
+ Most moveable adjacent cells
+ Besiged Regime
+ Adversary
+ Neutral
+ Ally
+ Unmarked
+ No awakening or reaction markers present
+ Adjacent to the _Auto Recruit Priority_
+ Oil exporter

= Auto Recruit Priority
The Enhanced Bot maintains a top priority auto recruit country.  When there is more than one auto recruit country on the
map, this country takes precendce when performing a recruit operation. The following priorities are used to determine the
_Auto Recruit Priority_ among all muslim countries with governance worse than Fair that have auto recruit status: Islamist Rule, Regime Change, Civil War.
+ Poor Regime Change with highest printed resource\*
+ Poor Caliphate Capital
+ Poor Caliphate member with the highest printed resource\*
+ Poor with Training Camps marker
+ Poor Civil War with highest printed resource\*
+ Islamist Rule Caliphate Capital
+ Islamist Rule with highest printed resource\*
+ Best Jihad DRM
+ Highest (cells - TandM)
+ Lowest TandM
+ No TandM and highest number of cells present plus moveable adjacent cells
+ Most moveable adjacent cells
+ Besiged Regime
+ Adversary
+ Neutral
+ Ally
+ Unmarked
+ No awakening or reaction markers present
+ Oil exporter

= Minor Jihad Candidates
When selecting candidate countries for performing a minor jihad, the Enhance Bot will always peform the jihad in a country
whose governance is Good.  But if the governance is Fair then there are some further condiderations used to ensure that
we do not risk losing the final cell in the country.
+ If the Training Camps marker is present then there must be at least two cells
+ At least of the following must be true:
  - Pakistan
  - Troops are present
  - Ally
  - printed resource\* = 3
  - Auto Recruit
  - Aid marker present
  - At least 3 cells present

= Minor Jihad Priorities
+ Troops present
+ Good
+ Best Jihad DRM
+ Aid marker
+ Regime change with Troops present
+ Highest printed resource\*
+ Most cells present
+ Adjacent to an Islamist Rule country

= Major Jihad Priorities
+ Best Jihad DRM
+ Aid Marker
+ Regime change with Troops present
+ Highest printed resource\*
+ Troops present
+ Most cells present
+ Adjacent to an Islamist Rule country

= Recruit/Travel To priorites <recruit-travelto>
+ Highest printed resource\* (_Only if Islamist Rule Resource < 4_)
+ Poor or Unmarked muslim where printed resource\* > 1 (_Only if Islamist Rule Resource = 4_)
+ Best Jihad DRM
+ Auto Recruit
+ Highest (cells - TandM)
+ Lowest TandM
+ No TandM and highest number of cells present plus moveable adjacent cells
+ Most moveable adjacent cells
+ Besiged Regime
+ Adversary
+ Neutral
+ Ally
+ Unmarked
+ No awakening or reaction markers present
+ Adjacent to the _Auto Recruit Priority_
+ Oil exporter

= Marker Placement Priorities
First if any of the canidates have less than 7 TandM, then we only consider
candidates with less than 7 TandM.  Then among the considered candidates:
+ Highest printed resource\*
+ Poor Muslim
+ Unmarked
+ #link(<recruit-travelto>)[Recruit/Travel To priorities]


= Aligment/Government Priorities
Use #link(<recruit-travelto>)[Recruit/Travel To priorities]

= Plot Candidate Priorities
+ United States
+ Troops that affect prestige present
+ Philippines  _(Labyrinth scenarios only)_
+ Most actve cells
+ Aid marker,
+ Regime change with troops present
+ Highest printed resource\*
+ Neutral
+ Adjacent to Good Ally
+ Fair Non-Muslim
+ Non-Muslim with same posture as US
+ Non-Muslim with lowest recruit value
+ Adjacent to Islamist Rule


= Selecting Plots
#table(
  columns: (auto, auto, auto),
  table.header([*Target*], [*Condition*], [*Instructions*]),
  [US],         [WMD available],                           [select randomly],
  [US],         [WMD not available],                       [lowest plot number],
  [Non-Muslim], [Funding > 7],                             [lowest plot marker],
  [Non-Muslim], [Funding <= 7],                            [highest plot marker (not WMD)],
  [Muslim],     [WMD available & Troops present],          [select randomly],
  [Muslim],     [WMD not available or Troops not present], [highest plot marker (Not WMD)],
)


= Plot OpP Flowchart
The Enhance Bot targets different countries depending on whether it is plotting to harm US prestige or it is
plotting to increase funding.
== Flowchart for Harming Prestige
+ Poor Muslims
+ Fair Muslims
== Flowchart for Increasing Funding      
+ Poor with troops present
+ Poor Non-Muslim
+ Poor Muslims
+ Fair Muslims
+ Good Muslims
+ Non-Muslims

= Recruit OpP Flowchart
+ Poor where (cells - TandM) > 2
+ Poor where TandM < 2
+ _Auto Recruit Priority_
+ Islamist Rule

= Travel To OpP Flowchart
+ Poor where (cells - TandM) > 2
+ Unmarked or Poor with TandM < 2


= Valid Recruiting targets
The Enhanced Bot never recruits in Non-Muslim countries.  When selecting recruiting candidates in Muslime countries
it uses the following criteria:
- Only recruit Good/Fair countries if they are Auto-Recruit (Regime Change/Civil War/Training Camp)
- Recruiting is always allowed in Poor countries
- Only recruit in Islamist Rule countries if they have less than 6 cells AND either
  + It is the _Auto Recruit Priority_
  + The _Auto Recruit Priority_ is not Islamist Rule

= Voluntary Cadre Removal
The Jihadist Bot will voluntarily remove cadres to make it harder for the US player to disrupt for
prestige gain or to execute the Wiretapping Event.  Before each card play the Bot will remove cadres
that meet the following criteria:
- In Muslim countries at Good/Fair that are NOT auto-recruit countries.
- In Philippines (to prevent disrupt if Abu Sayyaf marker is or becomes present).
- US, UK, Canada (unless Wiretapping has been blocked by Leak.

= Ending a Truce
The enhanced Bot will spend funding to end a Truce only if the Truce is in the _Major Jihad Priority_
Country.

= Jihadist Reserves
Unlike the standard Bot, the Enhanced Bot does not add the Ops value of _Unassociated_ event cards
after performing the card's event.  The Enhanced Bot will add Ops to reserves when it cannot play an
event and it cannot find a favoriable operation to play for the current card.

In the rare case where the Jihadist has no cells on the map, it will add Ops from the current card to
reserves. 

When using the _Easier_ difficulty level the Enhanced Bot will add the Ops value of a _US associated_ event
that it triggered during the Bot's turn when the Bot's reserves are empty.

When using the _Easiest_ difficulty level the Enhanced Bot will add the Ops value of a _US associated_ event
that it triggered during the Bot's turn when the Bot's reserves are empty or when there is only 1 Op in reserve.
