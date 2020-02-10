type state =
  | LoadingTrace
  | ErrorFetchingTrace
  | LoadedTrace(list(Small.SML.configuration));

[@react.component]
let make = (~program) => {
  let (state, setState) = React.useState(() => LoadingTrace);

  // Notice that instead of `useEffect`, we have `useEffect0`. See
  // reasonml.github.io/reason-react/docs/en/components#hooks for more info
  React.useEffect0(() => {
    Small.Main.traceProgram(program)
    |> Js.Promise.then_(trace => {
         setState(_previousState => LoadedTrace(trace));
         Js.Promise.resolve();
       })
    |> Js.Promise.catch(_err => {
         setState(_previousState => ErrorFetchingTrace);
         Js.Promise.resolve();
       })
    |> ignore;

    // Returning None, instead of Some(() => ...), means we don't have any
    // cleanup to do before unmounting. That's not 100% true. We should
    // technically cancel the promise. Unofortunately, there's currently no
    // way to cancel a promise. Promises in general should be way less used
    // for React components; but since folks do use them, we provide such an
    // example here. In reality, this fetch should just be a plain callback,
    // with a cancellation API
    None;
  });

  <div
    style={ReactDOMRe.Style.make(
      ~height="120px",
      ~display="flex",
      ~alignItems="center",
      ~justifyContent="center",
      (),
    )}>
    {switch (state) {
     | ErrorFetchingTrace => React.string("An error occurred!")
     | LoadingTrace => React.string("Loading...")
     | LoadedTrace(trace) =>
       let swTrace = List.map(SMALL2Theia.smlToTheiaIR, trace);
       <> {List.nth(swTrace, 0) |> Sidewinder.Main.render} </>;
     /* ->Belt.Array.mapWithIndex((i, dog) => {
                 let imageStyle =
                   ReactDOMRe.Style.make(
                     ~height="120px",
                     ~width="100%",
                     ~marginRight=i === Js.Array.length(dogs) - 1 ? "0px" : "8px",
                     ~borderRadius="8px",
                     ~boxShadow="0px 4px 16px rgb(200, 200, 200)",
                     ~backgroundSize="cover",
                     ~backgroundImage={j|url($dog)|j},
                     ~backgroundPosition="center",
                     (),
                   );
                 <div key=dog style=imageStyle />;
               })
             ->React.array
           }}
        */
     }}
  </div>;
};