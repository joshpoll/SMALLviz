// Entry point

[@bs.val] external document: Js.t({..}) = "document";

// We're using raw DOM manipulations here, to avoid making you read
// ReasonReact when you might precisely be trying to learn it for the first
// time through the examples later.
let style = document##createElement("style");
document##head##appendChild(style);
style##innerHTML #= ExampleStyles.style;

let makeContainer = text => {
  let container = document##createElement("div");
  container##className #= "container";

  let title = document##createElement("div");
  title##className #= "containerTitle";
  title##innerText #= text;

  let content = document##createElement("div");
  content##className #= "containerContent";

  let () = container##appendChild(title);
  let () = container##appendChild(content);
  let () = document##body##appendChild(container);

  content;
};

// All 4 examples.
ReactDOMRe.render(
  <BlinkingGreeting> {React.string("Hello!")} </BlinkingGreeting>,
  makeContainer("Blinking Greeting"),
);

ReactDOMRe.render(<ReducerFromReactJSDocs />, makeContainer("Reducer From ReactJS Docs"));

ReactDOMRe.render(<FetchedDogPictures />, makeContainer("Fetched Dog Pictures"));

ReactDOMRe.render(<ReasonUsingJSUsingReason />, makeContainer("Reason Using JS Using Reason"));

ReactDOMRe.render(<ReasonUsingJSUsingReason />, makeContainer("Reason Using JS Using Reason"));

/* ReactDOMRe.render(
     <Visualize node=Sidewinder.SidewinderExamples.g width=500. height=300. />,
     makeContainer("linked list"),
   );

   ReactDOMRe.render(
     <Visualize node=Sidewinder.SidewinderExamples.astExample width=500. height=300. />,
     makeContainer("AST"),
   ); */

[|Small.Main.{name: "foo", text: "5"}|] |> Array.map(Small.Main.traceProgram) |> Js.Promise.all;

List.mapi(
  (i, ex) =>
    ReactDOMRe.render(
      <SMALLtrace program=Small.Main.{name: "ex" ++ string_of_int(i), text: ex} />,
      makeContainer("ex" ++ string_of_int(i)),
    ),
  Examples.[ex0, ex1, ex2, ex3, ex4, ex5],
);

let appendExample = {|
fun append (xs, ys) =
  if null xs
  then ys
  else hd (xs) :: append (tl(xs), ys)

val x = [2, 4]
val y = [5, 3, 0]
val z = append(x, y)
|};

ReactDOMRe.render(
  <SMALLtrace program=Small.Main.{name: "append", text: appendExample} />,
  makeContainer("Append"),
);

let simpleList = {|
[2, 4]
|};

ReactDOMRe.render(
  <SMALLtrace program=Small.Main.{name: "simple list", text: simpleList} />,
  makeContainer("simple list"),
);

/* |> Js.Promise.then_(theiaIRTraces => Js.Promise.resolve(Js.log2("theiaIRTraces", theiaIRTraces))); */

/*
 ReactDOMRe.render(
   <Visualize node=Sidewinder.Append.env width=500. height=300. />,
   makeContainer("extended linked list example"),
 ); */