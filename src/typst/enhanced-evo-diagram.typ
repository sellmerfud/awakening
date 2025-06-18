
#import "@preview/fletcher:0.5.7" as fletcher: diagram, node, edge
#import fletcher.shapes: diamond, ellipse, pill, hexagon

#set page(
  paper: "a3",
  margin: 0.75cm,
  flipped: false, // portrait orientation
)

#let box-background = blue.lighten(80%)

// Title
#place(
  top + center,
  float: true,
  scope: "parent",
  clearance: 1em,
  box(stroke: none, inset :1em, fill: box-background,
    text(size: 17pt)[
      Enhanced Jihadist Bot EvO
    ]
  )
)


#set text(
  font: "Libertinus Serif",
  size: 7pt,
)

#diagram(
debug: false, // show a coordinate grid
spacing: (1.6cm, 1cm), // column, row
node-stroke: 1pt,
edge-stroke: (thickness: 0.6pt),
crossing-thickness: 8,
{
  node((0,0), name: <start>, shape: ellipse)[Start]
  node((0, 1), name: <no-cells>, shape: diamond)[No cells on map?]
  edge(<start>, <no-cells>, "-|>")
  node((1, 1), name: <place-random>, shape: pill)[Place random\ cells]
  edge(<no-cells>, <place-random>, "-|>", [Yes])
  edge(<no-cells>, <mJihad-poor-ok>, "-|>", [No])

  node((0, 2), name: <mJihad-poor-ok>, shape: diamond)[Major Jihad possible\ at Poor Muslim?]
  node((1, 2), name: <mJihad-poor-desired>, shape: diamond)[Major Jihad desired\ at Poor Muslim?]
  edge(<mJihad-poor-ok>, <mJihad-poor-desired>, "-|>", [Yes])
  node((2, 2), name: <mJihad>, shape: pill)[Perform Major\ Jihad]
  node((2, 2.7), name: <add-reserve>, shape: pill)[Add Ops to\ reserve]
  edge(<mJihad-poor-desired>, <mJihad>, "-|>", [Yes])
  edge(<mJihad-poor-desired>, <add-reserve>, "-|>", [No])
  edge(<mJihad-poor-ok>, <from-afghan>, "-|>", [No])

  node((0, 3), name: <from-afghan>, shape: diamond)[Travel 1 cell from\ Afghanistan?#footnote[
    Only in these scenarios: _Fall Of ISIL_,_ Trump Takes Command_, _Islamic State of Iraq & the Levant (ISIL)_
  ]]
  node((1, 3), name: <travel-from-afghan>, shape: pill)[Travel 1 cell to\ C. Asia/Iran/Pakistan]
  edge(<from-afghan>, <travel-from-afghan>, "-|>", [Yes])
  edge(<from-afghan>, <mission-accomplished>, "-|>", [No])
  
  node((0, 4), name: <mission-accomplished>, shape: diamond)[Mission Accomp.\ & 1 cell, 2+ troops Phillipines\ & Abu SAyyaf?]
  node((1, 4), name: <travel-indonesia>, shape: pill)[Travel cell to\ Indonesia]
  edge(<mission-accomplished>, <travel-indonesia>, "-|>", [Yes])
  edge(<mission-accomplished>, <cell-jsp>, "-|>", [No])

  node((0, 5), name: <cell-jsp>, shape: diamond)[Cell in Good/Fair\ where JSP?]
  node((1, 5), name: <minor-jihad>, shape: pill)[Perform Minor Jihad]
  edge(<cell-jsp>, <minor-jihad>, "-|>", [Yes])
  edge(<cell-jsp>, <ir-res-1-scenarios>, "-|>", [No])


  node((0, 6), name: <ir-res-1-scenarios>, shape: diamond)[Let's Roll/Call Me Al\ & IR Res = 1?]
  edge(<ir-res-1-scenarios>, <move-to-c-asia>, "-|>", [Yes])
  edge(<ir-res-1-scenarios.west>, (-0.75, 6), (-0.75, 9), <gwot-test.west>, "-|>", [No])

  node((0, 7), name: <move-to-c-asia>, shape: diamond)[C. Asia unmarked &\ moveable adj. cell?]
  node((0, 8), name: <d6-A>, shape: hexagon)[Roll d6]
  node((0.8, 7), name: <d6-B>, shape: hexagon)[Roll d6]
  node((0, 9), name: <gwot-test>, shape: diamond)[US Hard,\ GWOT = 0,\ Hard - Soft < 2\ w/ travel cells]
  node((2, 7), name: <travel-c-asia>, shape: pill)[Travel to\ C. Asia]
  node((1, 9), name: <travel-unmarked-2>, shape: pill)[Travel to\ Unmarked Non-Muslim]
  node((1, 8), name: <pakistan-test>, shape: diamond)[Pakistan Fair,\ no troops &\ moveable adj. cell]
  node((2, 8), name: <travel-pakistan>, shape: pill)[Travel to\ Pakistan]
  edge(<move-to-c-asia>, <d6-A>, "-|>", [No])
  edge(<move-to-c-asia>, <d6-B>, "-|>", [Yes])
  edge(<d6-A>, <gwot-test>, "-|>", [1-3])
  edge(<d6-A>, <pakistan-test>, "-|>", label-pos: 21%, crossing: true, [4-6])
  edge(<d6-B>, <pakistan-test>, "-|>", label-pos: 25%, [1-2])
  edge(<d6-B>, <travel-c-asia>, "-|>", [3-4])
  edge(<d6-B>, <gwot-test>, "-|>", label-pos: 10%, crossing: true, [5-6])
  edge(<pakistan-test>, <gwot-test>, "-|>", [No])
  edge(<pakistan-test>, <travel-pakistan>, "-|>", [Yes])
  edge(<gwot-test>, <travel-unmarked-2>, "-|>", [Yes])
  edge(<gwot-test>, <arp-cells>, "-|>", [No])
  
  node((0, 10), name: <arp-cells>, shape: diamond)[ARP \ with < 3 cells?]
  node((1, 10), name: <arp-recruit-low-test>, shape: diamond)[Can recruit \ at ARP?]
  node((1, 11), name: <arp-adj-travel-test>, shape: diamond)[Can adj. travel\ to ARP?]
  node((0, 11.5), name: <funding-less-7>, shape: diamond)[Funding < 7?]
  node((1, 12.25), name: <funding7-no-cards>, shape: diamond)[Funding = 7 &\ No cards\ in Bot hand?]
  node((2, 10), name: <arp-recruit-low>, shape: pill)[Recruit at\ ARP]
  node((2, 11), name: <arp-travel>, shape: pill)[Adj. travel to\ ARP]
  node((0, 12.25), name: <plot-funding>, shape: pill)[Perform Plot\ (funding)]
  edge(<arp-cells>, <arp-recruit-low-test>, "-|>", [Yes])
  edge(<arp-cells>, <funding-less-7>, "-|>", [No])
  edge(<arp-recruit-low-test>, <arp-recruit-low>, "-|>", [Yes])
  edge(<arp-recruit-low-test>, <arp-adj-travel-test>, "-|>", [No])
  edge(<arp-adj-travel-test>, <arp-travel>, "-|>", [Yes])
  edge(<arp-adj-travel-test>, <funding-less-7>, "-|>", [No])
  edge(<funding-less-7>, <plot-funding>, "-|>", [Yes])
  edge(<funding-less-7>, <funding7-no-cards>, "-|>", [No])
  edge(<funding7-no-cards>, <plot-funding>, "-|>", [Yes])
  edge(<funding7-no-cards>, (2.4, 12.25), (2.4, 1),<cell-w-troops.west>, "-|>", label-pos: 10%, [No])

  node((3, 1), name: <cell-w-troops>, shape: diamond)[Prestige > 3 &\ cell w/ troops?]
  node((4, 1), name: <plot-prestige>, shape: pill)[Perform Plot\ (prestige)]
  node((3, 2), name: <mjp-auto-recruit>, shape: diamond)[MJP is\ Auto-Recruit?]
  edge(<cell-w-troops>, <plot-prestige>, "-|>", [Yes])
  edge(<cell-w-troops>, <mjp-auto-recruit>, "-|>", [No])
  edge(<mjp-auto-recruit>, <recuit-mjp-test>, "-|>", [Yes])
  edge(<mjp-auto-recruit>, <adj-travel-mjp-1>, "-|>", [No])
  edge(<recuit-mjp-test>, <adj-travel-mjp-2>, "-|>", [No])

  node((4, 2), name: <recuit-mjp-test>, shape: diamond)[Can Recruit at\ MJP?]
  node((5, 2), name: <recuit-mjp>, shape: pill)[Recruit at\ MJP]
  edge(<recuit-mjp-test>, <recuit-mjp>, "-|>", [Yes])

  node((3, 3), name: <adj-travel-mjp-1>, shape: diamond)[Adj. travel to\ MJP?]
  node((4, 3), name: <adj-travel-mjp-2>, shape: diamond)[Adj. travel to\ MJP?]
  node((3.5, 3.5), name: <adj-travel-mjp>, shape: pill)[Adj. travel to\ MJP]
  let mjp_verts = (
    ((), "-|", (<adj-travel-mjp-1.east>, 50%, <adj-travel-mjp-2.west>)),
    <adj-travel-mjp>
  )
  edge(<adj-travel-mjp-1>, ..mjp_verts, "-|>", [Yes])
  edge(<adj-travel-mjp-2>, ..mjp_verts)

  node((3, 4.2), name: <recruit-mjp-test>, shape: diamond)[Recruit at\ MJP?]
  node((3.7, 4.2), name: <recruit-mjp>, shape: pill)[Recruit at\ MJP]

  edge(<adj-travel-mjp-1>, <recruit-mjp-test>, "-|>", [No])
  edge(<recruit-mjp-test>, <recruit-mjp>, "-|>", [Yes])

  node((4, 5.2), name: <travel-mjp-test>, shape: diamond)[Non-Adj. travel\ to MJP?]
  edge(<adj-travel-mjp-2>, <travel-mjp-test>, "-|>", label-pos: 10%, [No])
  edge(<recruit-mjp-test>, <travel-mjp-test>, "-|>", label-pos: 20%, label-sep: -14pt, [No])

  node((5, 5.2), name: <travel-mjp>, shape: pill)[Travel to\ MJP]
  edge(<travel-mjp-test>, <travel-mjp>, "-|>", [Yes])
  edge(<travel-mjp-test>, <arp-recruit-test>, "-|>", [No])

  node((4, 6.2), name: <arp-recruit-test>, shape: diamond)[Can recruit \ at ARP?]
  node((5, 6.2), name: <recruit-arp>, shape: pill)[Recruit at\ MJP]
  node((4, 7), name: <radicalization>, shape: pill)[Radicalization]
  edge(<arp-recruit-test>, <recruit-arp>, "-|>", [Yes])
  edge(<arp-recruit-test>, <radicalization>, "-|>", [No])

})

#v(4em)
#box(
  stroke: none, inset: 0.8em, fill: box-background,
  text(font: "Consolas Nerd Font", size: 9pt)[
    ARP = Auto-Recruit Priority Country \
    MJP = Major Jihad Priority Country \
    JSP = Jihad Success Possible
  ]
)
