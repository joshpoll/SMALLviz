let leftButtonStyle = ReactDOMRe.Style.make(~borderRadius="4px 0px 0px 4px", ~width="48px", ());
let rightButtonStyle = ReactDOMRe.Style.make(~borderRadius="0px 4px 4px 0px", ~width="48px", ());

let render = n =>
  Sidewinder.(
    n |> LCA.fromKernel |> Layout.computeBBoxes |> RenderLinks.renderLinks |> Render.render
  );

type traceProgress =
  | LoadingTrace
  | ErrorFetchingTrace
  | LoadedTrace(list(Small.Resugar.configuration));

type state = {
  pos: int,
  traceProgress,
};

type action =
  | Increment
  | Decrement
  | Trace(list(Small.Resugar.configuration))
  | Error;

let initialState = {pos: 0, traceProgress: LoadingTrace};

let reducer = (state, action) => {
  switch (state.traceProgress) {
  | LoadedTrace(trace) =>
    switch (action) {
    | Increment => {...state, pos: min(List.length(trace) - 1, state.pos + 1)}
    | Decrement => {...state, pos: max(0, state.pos - 1)}
    | _ => state
    }
  | _ =>
    switch (action) {
    | Error => {...state, traceProgress: ErrorFetchingTrace}
    | Trace(trace) => {...state, traceProgress: LoadedTrace(trace)}
    | _ => state
    }
  };
};

[@react.component]
let make = (~program) => {
  let (state, dispatch) = React.useReducer(reducer, initialState);

  // Notice that instead of `useEffect`, we have `useEffect0`. See
  // reasonml.github.io/reason-react/docs/en/components#hooks for more info
  React.useEffect0(() => {
    Small.Main.traceProgram(program)
    |> Js.Promise.then_(trace => {
         dispatch(Trace(trace |> List.map(Small.Main.resugar)));
         Js.Promise.resolve();
       })
    |> Js.Promise.catch(_err => {
         dispatch(Error);
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

  switch (state.traceProgress) {
  | ErrorFetchingTrace => React.string("An error occurred!")
  | LoadingTrace => React.string("Loading...")
  | LoadedTrace(trace) =>
    let swTrace =
      trace
      |> List.map(SMALL2Theia.smlToTheiaIR)
      |> List.map(Sidewinder.Transform.hide("idStatus"))
      |> List.map((Some(x)) => x)
      |> List.map(Sidewinder.Transform.denest("::", "::"));
    let initState = List.nth(swTrace, state.pos /* 150 */) |> render;
    let width = initState.bbox.sizeOffset->Sidewinder.Rectangle.width;
    let height = initState.bbox.sizeOffset->Sidewinder.Rectangle.height;
    let xOffset =
      initState.bbox.translation.x +. initState.bbox.sizeOffset->Sidewinder.Rectangle.x1;
    let yOffset =
      initState.bbox.translation.y +. initState.bbox.sizeOffset->Sidewinder.Rectangle.y1;
    <div>
      <div> {React.string("state: ")} {React.string(string_of_int(state.pos))} </div>
      <button style=leftButtonStyle onClick={_event => dispatch(Decrement)}>
        {React.string("<-")}
      </button>
      <button style=rightButtonStyle onClick={_event => dispatch(Increment)}>
        {React.string("->")}
      </button>
      <svg
        xmlns="http://www.w3.org/2000/svg"
        width={Js.Float.toString(width)}
        height={Js.Float.toString(height)}>
        <g
          transform={
            "translate("
            ++ Js.Float.toString(-. xOffset)
            ++ ", "
            ++ Js.Float.toString(-. yOffset)
            ++ ")"
          }>
          {initState.rendered}
        </g>
      </svg>
    </div>;
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
  };
};