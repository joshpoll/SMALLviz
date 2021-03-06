/* Compiler from SML configuration to TheiaIR */
/* TODO: so many List.revs. Maybe I should change the default? */
open Small.Resugar;
open Sidewinder.Theia;

let rec interleave = (xs, ys) =>
  switch (xs, ys) {
  | ([], _) => ys
  | ([x, ...xs], _) => [x, ...interleave(ys, xs)]
  };

let hSeq = (~gap=0., nodes) => seq(~nodes, ~linkRender=None, ~gap, ~direction=LeftRight, ());

let vSeq = (~gap=0., nodes) => seq(~nodes, ~linkRender=None, ~gap, ~direction=UpDown, ());

let apply = (ops, args) => hSeq(interleave(ops, args));

/* TODO: incoporate name */
let value = (name, node) => box(~tags=[name], ~dx=5., ~dy=5., node, [], ());
let cell = (name, node) => box(~tags=[name], ~dx=5., ~dy=5., node, [], ());

let kv = (key, value) =>
  box(~dx=5., ~dy=5., hSeq([box(key, [], ()), box(value, [], ())]), []);

let map = (~keyHeader, ~valueHeader, kvs) =>
  vSeq(~gap=2., [hSeq(~gap=20., [keyHeader, valueHeader]), ...kvs]);

let split = (list, n) => {
  let rec aux = (i, acc) =>
    fun
    | [] => (List.rev(acc), [])
    | [h, ...t] as l =>
      if (i == 0) {
        (List.rev(acc), l);
      } else {
        aux(i - 1, [h, ...acc], t);
      };
  aux(n, [], list);
};

let insert = (x, xs, i) => {
  let (xs, ys) = split(xs, i);
  xs @ [x, ...ys];
};

let kont = (nodeBuilder, nodes, holePos, hole): Sidewinder.Kernel.node =>
  nodeBuilder(insert(hole, nodes, holePos));

let rec zipper = (focus, konts) =>
  switch (konts) {
  | [] => focus
  | [k, ...konts] => k(zipper(focus, konts))
  };

let compileSCon = (sc: sCon) =>
  switch (sc) {
  | INT(n) => str(string_of_int(n), ())
  };

let rec compileAtExp = a =>
  switch (a) {
  | SCON(sc) => compileSCon(sc)
  | ID(x) => str(x, ())
  | RECORD(None) => str("{}", ())
  | RECORD(Some(er)) => apply([str("{", ()), str("}", ())], [compileExpRow(er)])
  | LET(d, e) =>
    apply(
      [str("let ", ()), str(" in ", ()), str(" end", ())],
      [compileDec(d), compileExp(e)],
    )
  | PAR(e) => apply([str("(", ()), str(")", ())], [compileExp(e)])
  }

and compileExpRow = (EXPROW(lab, exp, rest)) =>
  switch (rest) {
  | None => hSeq([str(lab, ()), str("=", ()), compileExp(exp)])
  | Some(er) =>
    hSeq([str(lab, ()), str("=", ()), compileExp(exp), str(", ", ()), compileExpRow(er)])
  }

and compileExp = e =>
  switch (e) {
  | ATEXP(a) => compileAtExp(a)
  | APP(f, x) => hSeq([compileExp(f), str(" ", ()), compileAtExp(x)])
  | RAISE(e) => hSeq([str("raise ", ()), compileExp(e)])
  | FN(m) => hSeq([str("fn ", ()), compileMatch(m)])
  }

and compileMatch = m =>
  switch (m) {
  | MATCH(mr, None) => compileMRule(mr)
  | MATCH(mr, Some(m)) => vSeq([compileMRule(mr), hSeq([str("| ", ()), compileMatch(m)])])
  }

and compileMRule = mr =>
  switch (mr) {
  | MRULE(p, e) => hSeq([compilePat(p), str(" => ", ()), compileExp(e)])
  }

and compileDec = d =>
  switch (d) {
  | VAL(vb) => hSeq([str("val ", ()), compileValBind(vb)])
  }

and compileValBind = vb =>
  switch (vb) {
  | PLAIN(p, e, None) => hSeq([compilePat(p), str(" = ", ()), compileExp(e)])
  | REC(vb) => hSeq([str("rec ", ()), compileValBind(vb)])
  }

and compileAtPat = a =>
  switch (a) {
  | WILDCARD => str("_", ())
  | ID(x) => str(x, ())
  | RECORD(None) => str("{}", ())
  | RECORD(Some(pr)) => apply([str("{", ()), str("}", ())], [compilePatRow(pr)])
  | PAR(p) => apply([str("(", ()), str(")", ())], [compilePat(p)])
  }
/* and compileExpRow = (EXPROW(lab, exp, rest)) =>
   switch (rest) {
   | None =>
     apply([<> </>, str("=", ()), <> </>], [str(lab, ()), compileExp(exp)])
   | Some(er) =>
     apply(
       [<> </>, str("=", ()), str(", ", ()), <> </>],
       [str(lab, ()), compileExp(exp), compileExpRow(er)],
     )
   } */
and compilePatRow = pr =>
  switch (pr) {
  | DOTS => str("...", ())
  | FIELD(l, p, None) => hSeq([str(l, ()), str("=", ()), compilePat(p)])
  | FIELD(l, p, Some(pr)) =>
    hSeq([str(l, ()), str("=", ()), compilePat(p), str(", ", ()), compilePatRow(pr)])
  }

and compilePat = p =>
  switch (p) {
  | ATPAT(a) => compileAtPat(a)
  | CON(x, ap) => hSeq([str(x, ()), str(" ", ()), compileAtPat(ap)])
  };

let compileSVal = (sv: sVal) =>
  switch (sv) {
  | INT(n) => /* value("", str(string_of_int(n), ())) */ str(string_of_int(n), ())
  };

let compileIdStatus = id =>
  switch (id) {
  | Var => str(~tags=["idStatus"], "Var", ())
  | Con => str(~tags=["idStatus"], "Con", ())
  | Exc => str(~tags=["idStatus"], "Exc", ())
  };

let rec compileVal_ = v =>
  switch (v) {
  | SVAL(sv) => compileSVal(sv)
  | BASVAL(b) => value("", hSeq([str("builtin ", ()), str(b, ())]))
  | VID(x) when x == "nil" => str("/", ())
  | VID(x) => value("", str(x, ()))
  | VIDVAL(vid, v) =>
    /* vSeq([str(vid, ()), compileVal_(v)]) */
    /* value(vid, compileVal_(v)) */
    seq(
      ~tags=[vid],
      ~nodes=[str(vid, ()), compileVal_(v)],
      ~linkRender=None,
      ~gap=0.,
      ~direction=UpDown,
      (),
    )
  | TUPLE(l) =>
    box(
      ~dx=0.,
      ~dy=0.,
      table(
        ~tags=["tuple"],
        ~nodes=[List.map(compileVal_, l)],
        ~linkRender=
          Some(
            (~source, ~target) =>
              <line
                x1={Js.Float.toString(
                  (source->Sidewinder.Rectangle.x2 +. target->Sidewinder.Rectangle.x1) /. 2.,
                )}
                x2={Js.Float.toString(
                  (source->Sidewinder.Rectangle.x2 +. target->Sidewinder.Rectangle.x1) /. 2.,
                )}
                y1={Js.Float.toString(
                  (source->Sidewinder.Rectangle.y1 +. target->Sidewinder.Rectangle.y1) /. 2.,
                )}
                y2={Js.Float.toString(
                  (source->Sidewinder.Rectangle.y2 +. target->Sidewinder.Rectangle.y2) /. 2.,
                )}
                stroke="black"
              />,
          ),
        ~xGap=0.,
        ~yGap=0.,
        ~xDirection=LeftRight,
        ~yDirection=UpDown,
        (),
      ),
      [],
      (),
    )
  | RECORD([]) => value("", str("{}", ()))
  | RECORD(r) => value("", apply([str("{", ()), str("}", ())], [compileRecord(r)]))
  | FCNCLOSURE(m, e, ve) =>
    value(
      "closure",
      hSeq(List.map(n => box(n, [], ()), [compileMatch(m), compileEnv(e), compileEnv(ve)])),
    )
  }

and compileRecord = r =>
  switch (r) {
  | [] => str("", ())
  | [(l, v)] => hSeq([str(l, ()), str("=", ()), compileVal_(v)])
  | [(l, v), ...r] =>
    hSeq([str(l, ()), str("=", ()), compileVal_(v), str(", ", ()), compileRecord(r)])
  }

and compileRecordEnv = rve =>
  switch (rve) {
  | [] => str("", ())
  | [(l, ve)] => hSeq([str(l, ()), str("=", ()), compileEnv(ve)])
  | [(l, ve), ...rve] =>
    hSeq([str(l, ()), str("=", ()), compileEnv(ve), str(", ", ()), compileRecordEnv(rve)])
  }

and compileKVs = ((k, (v, id))) => [
  str(k, ()),
  hSeq(~gap=10., [compileVal_(v), compileIdStatus(id)]),
]

/* TODO: maybe remove List.rev once direction can be controlled in Theia table */
and compileEnv = e =>
  table(
    ~nodes=[[str("Id", ()), str("Val", ())], ...e |> List.map(compileKVs) |> List.rev],
    ~linkRender=None,
    ~xGap=0.,
    ~yGap=0.,
    ~xDirection=LeftRight,
    ~yDirection=UpDown,
    (),
  );

let rec compileStrDec = sd =>
  switch (sd) {
  | DEC(d) => compileDec(d)
  | SEQ(sd1, sd2) => vSeq([hSeq([compileStrDec(sd1), str(";", ())]), compileStrDec(sd2)]) /* TODO:
  should have extra whitespace here probably */
  };

let rec compileTopDec = td =>
  switch (td) {
  | STRDEC(sd, None) => compileStrDec(sd)
  | STRDEC(sd, Some(td)) => vSeq([compileStrDec(sd), compileTopDec(td)]) /* TODO: should have extra whitespace here probably */
  };

let rec compileProgram = p =>
  switch (p) {
  | PROGRAM(td, None) => compileTopDec(td)
  | PROGRAM(td, Some(p)) =>
    vSeq([hSeq([compileTopDec(td), str(";", ())]), compileProgram(p)]) /* TODO: should have extra whitespace here probably */
  };

let compileFocus = f =>
  switch (f) {
  | AtExp(a) => compileAtExp(a)
  | Exp(e) => compileExp(e)
  | Val(v) => compileVal_(v)
  | Dec(d) => compileDec(d)
  | ValBind(vb) => compileValBind(vb)
  | StrDec(sd) => compileStrDec(sd)
  | TopDec(td) => compileTopDec(td)
  | ExpRow(er) => compileExpRow(er)
  | Record(r) => compileRecord(r)
  | Program(p) => compileProgram(p)
  | Match(m, v) => hSeq([compileVal_(v), compileMatch(m)])
  | MRule(mr, v) => hSeq([compileVal_(v), compileMRule(mr)])
  | Pat(p, v) => hSeq([compileVal_(v), compilePat(p)])
  | AtPat(ap, v) => hSeq([compileVal_(v), compileAtPat(ap)])
  /* TODO: improve this */
  | PatRow(pr, r, rve) => vSeq([compileRecordEnv(rve), compilePatRow(pr), compileRecord(r)])
  | FAIL(v) => hSeq([str("FAIL", ()), compileVal_(v)])
  | ValEnv(ve) => compileEnv(ve)
  | Empty => str(" ", ())
  };

let compileCtxt = c =>
  switch (c) {
  | LETD((), e) =>
    kont(hSeq, [str("let ", ()), str(" in ", ()), compileExp(e), str(" end", ())], 1)
  | VALBINDE(p, (), None) => kont(hSeq, [compilePat(p), str(" = ", ())], 2)
  | APPL((), x) => kont(hSeq, [str(" ", ()), compileAtExp(x)], 0)
  | APPR(f, ()) => kont(hSeq, [compileVal_(f), str(" ", ())], 2)
  | SEQL((), sd2) => (
      hole => vSeq([kont(hSeq, [str(";", ())], 0, hole), compileStrDec(sd2)])
    )
  | DECD () => kont(hSeq, [], 0)
  | RECORDER () => kont(hSeq, [str("{", ()), str("}", ())], 1)
  | EXPROWE([], l, (), None) => kont(hSeq, [str(l, ()), str("=", ())], 2)
  | EXPROWE(r, l, (), None) =>
    kont(hSeq, [compileRecord(r), str(", ", ()), str(l, ()), str("=", ())], 4)
  | EXPROWE([], l, (), Some(er)) =>
    kont(hSeq, [str(l, ()), str("=", ()), str(", ", ()), compileExpRow(er)], 2)
  | EXPROWE(r, l, (), Some(er)) =>
    kont(
      hSeq,
      [
        compileRecord(r),
        str(", ", ()),
        str(l, ()),
        str("=", ()),
        str(", ", ()),
        compileExpRow(er),
      ],
      4,
    )
  | PROGRAML((), p) => kont(vSeq, [str(";", ()), compileProgram(p)], 0) /* TODO: this needs a nested hSeq */
  | MATCHMR((), None) => kont(hSeq, [], 0)
  | MATCHMR((), Some(m)) => kont(vSeq, [hSeq([str("| ", ()), compileMatch(m)])], 0)
  | MRULEP((), e) => kont(hSeq, [str(" => ", ()), compileExp(e)], 0)
  | RECVB () => kont(hSeq, [str("rec ", ())], 1)
  | RECORDPR () => kont(hSeq, [str("{", ()), str("}", ())], 1)
  | STRDECSD((), None) => kont(hSeq, [], 0)
  | STRDECSD((), Some(td)) => kont(vSeq, [compileTopDec(td)], 0) /* TODO: more vertical spacing? */
  /* TODO: this printing could probably be better */
  | FIELDP((l, (), None), r, rve) => (
      hole =>
        vSeq([
          kont(hSeq, [compileRecordEnv(rve), str(l, ()), str("=", ())], 3, hole),
          compileRecord(r),
        ])
    )
  | FIELDP((l, (), Some(pr)), r, rve) => (
      hole =>
        vSeq([
          kont(
            hSeq,
            [
              compileRecordEnv(rve),
              str(l, ()),
              str("=", ()),
              str(", ", ()),
              compilePatRow(pr),
            ],
            3,
            hole,
          ),
          compileRecord(r),
        ])
    )
  };

let compileRewrite = ({focus, ctxts}) =>
  zipper(compileFocus(focus), List.map(compileCtxt, ctxts));

let compileFrame = ({rewrite, env}) =>
  vSeq([
    /* "frame", */
    cell("env", compileEnv(env)),
    cell("rewrite", compileRewrite(rewrite)),
  ]);

/* TODO: hSeq? */
let smlToTheiaIR = fs => vSeq(~gap=10., List.map(compileFrame, fs) |> List.rev);